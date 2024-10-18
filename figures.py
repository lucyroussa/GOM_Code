import os
import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

#get sys arguments
if len(sys.argv) < 2:
    raise Exception('1 command line argument required: <figure abbreviation>')

#setup dataframes
root = os.path.join(os.getcwd(), 'data')
main_df = pd.read_csv(os.path.join(root, 'Genus_functional_assignment_new.csv'))
#remove unneeded data
main_df = main_df[(main_df["taxa"] == 'CILIATES') | (main_df["taxa"] == 'DIATOMS') | (main_df["taxa"] == 'DINOFLAGELLATES')]
station_df = pd.read_csv(os.path.join(root, 'Taxa_Functional_grouping_bar_graph_station_distance_order.csv'))
transect_station_map = {}
station_distance_map = {}
all_sites = set()
taxas = ['CILIATES','DIATOMS','DINOFLAGELLATES']
f_groups = main_df['Functional_type'].drop_duplicates()
taxa_df = pd.DataFrame(index=taxas)
f_groups_df = pd.DataFrame(index=f_groups)
color_map = {
    'CILIATES': '#111111',
    'DIATOMS': '#333333',
    'DINOFLAGELLATES': '#555555',
    'NCM': '#624E88',
    'Heterotroph': '#7695FF',
    '?': '#CB80AB',
    'CM': '#FF4E88',
    'Phototroph': '#E6D9A2'}
df_column_pairs = [
    {'df': taxa_df, 'column': 'taxa', 'columns': taxas},
    {'df': f_groups_df, 'column': 'Functional_type', 'columns': f_groups}]
remap_stations = {'GOM4_9_surf': 'GOM_9_surf', 'GOM4_47_surf': '47'}
skip = set([140, 64])

def get_full_name(station):
    full_name = 'GOM4_' + str(station) + '_surf'
    full_name = remap_stations[full_name] if full_name in remap_stations else full_name
    return full_name

def invert_dict_list(input_dict):
        output = {}
        #if many values for each key
        if type(list(input_dict.values())[0]) in [type([]), type(pd.Series([0]))]:
            [output.update(d) for d in [{value: key for value in l} for key, l in input_dict.items()]]
        #if one value for each key
        else:
            for key, value in input_dict.items():
                if value in output:
                    output[value].append(key)
                else:
                    output[value] = [key]
        return output

#reorganize station grouping
for index, row in station_df.iterrows():
    if row['Station number'] in skip:
        continue
    transect = row['transect']
    all_sites.add(row['Station number'])
    station_distance_map[row['Station number']] = row['Distance from shore']
    if transect in transect_station_map:
        transect_station_map[transect].at[row['Order in transect']] = row['Station number']
    else:
        transect_station_map[transect] = pd.Series(index=[row['Order in transect']], data=[row['Station number']])
station_transect_map = invert_dict_list(transect_station_map)

'''#count functional groups and taxa
for site_i in all_sites:
    column_name = get_full_name(site_i)
    #update taxas and functional groups
    for i in range(len(df_column_pairs)):
        obj = df_column_pairs[i]
        df = obj['df']
        c = obj['column']
        column_list = obj['columns']
        df.insert(len(df.columns), column_name, len(column_list) * [np.nan], True)
        for v in column_list:
            df.at[v, column_name] = main_df[main_df[c] == v][column_name].sum()
        #convert to percentages
        for column in df.columns:
            total = df[column].sum()
            df[column] = df[column] / total
taxa_df = taxa_df.loc[taxa_df.index, taxa_df.sum(axis=0) != 0]
f_groups_df = f_groups_df.loc[f_groups_df.index, f_groups_df.sum(axis=0) != 0]
taxa_df.to_csv(os.path.join(root, 'taxa.csv'))
f_groups_df.to_csv(os.path.join(root, 'functional-groups.csv'))'''

if sys.argv[1] == 'bar':
    #plot
    for transect, stations in transect_station_map.items():
        x = np.arange(len(stations)) * 2
        width = 0.25 # the width of the bars
        multiplier = 0
        offset = 0
        full_station_names = [get_full_name(site_i) for site_i in stations]
        if not (set(full_station_names).issubset(taxa_df.columns) and set(full_station_names).issubset(f_groups_df.columns)):
            print(transect, 'has some sites with no taxa or functional group data')
            continue
        df_column_pairs[0]['df'] = taxa_df[full_station_names]
        df_column_pairs[1]['df'] = f_groups_df[full_station_names]
        fig, ax = plt.subplots(layout='constrained')
        for obj in df_column_pairs:
            df = obj['df']
            previous = [0] * 3
            for i in df.index:
                data = df.loc[i]
                plt.bar(x + offset, data, bottom=previous, color=color_map[i])
                previous += data
            multiplier += 4
            offset = width * multiplier
        legend_elements = [Patch(facecolor=color_map[f], label=f) for f in list(f_groups) + taxas]
        ax.legend(handles=legend_elements)
        ax.set_xticks(x + 0.5, labels=[str(station_distance_map[i]) + '-' + str(i) for i in stations])
        plt.title(transect)
        plt.savefig(os.path.join(os.getcwd(), 'figures', transect + '.png'), dpi=200)
        plt.close()
elif sys.argv[1] == 'mds':
    #setup data
    transect_region_map = {str(station_df['Station number'].loc[i]): station_df['Region'].loc[i] for i in range(len(station_df.index))}
    station_transect_map = {str(k): v for k, v in station_transect_map.items()}
    sheets = {'station': None, 'transect': station_transect_map, 'region': transect_region_map}
    df = pd.read_csv(os.path.join(root, 'Genus_functional_assignment_new.csv')).drop(['taxa', 'Functional_type', '8', '33'], axis=1)
    sample_id_column = df['SAMPLE  ID']
    df = df.drop(['SAMPLE  ID'], axis=1)
    #save sheets
    for grouping, column_map in sheets.items():
        n_df = df.copy()
        if column_map:
            n_df = n_df.T.rename(column_map).groupby(level=0).sum().T
        n_df = n_df.div(n_df.sum(axis=0), axis=1).fillna(0)
        n_df.insert(loc=0, column='SAMPLE  ID', value=sample_id_column)
        save_path = os.path.join(root, 'norm-counts-by-' + grouping + '.csv')
        n_df.to_csv(save_path)
        print('saved to ' + save_path)
elif sys.argv[1] == 'rename-station-columns':
    df = pd.read_csv(os.path.join(root, 'Genus_functional_assignment_new.csv'))
    statino_numbers = df.drop(['SAMPLE  ID', 'taxa', 'Functional_type'], axis=1).columns
    def get_new_column_name(n):
        if n == '47':
            return str(n)
        return n[n.index('_') + 1:n.rfind('_')]
    rename_map = {n: get_new_column_name(n) for n in statino_numbers}
    df.rename(columns=rename_map).to_csv(os.path.join(root, 'renamed-columns.csv'))