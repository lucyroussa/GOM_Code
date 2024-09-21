import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

#setup dataframes
root = os.path.join(os.getcwd(), 'data')
main_df = pd.read_csv(os.path.join(root, 'Taxa_Functional_grouping_bar_graph.csv'))
#remove unneeded data
main_df = main_df[(main_df["taxa"] == 'CILIATES') | (main_df["taxa"] == 'DIATOMS') | (main_df["taxa"] == 'DINOFLAGELLATES')]
station_df = pd.read_csv(os.path.join(root, 'Taxa_Functional_grouping_bar_graph_station_distance_order.csv.csv'))
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
    'NCM': '#EB37A6',
    'Heterotroph': '#EFA3A1',
    '?': '#EB3467',
    'CM': '#EA645E',
    'Phototroph': '#F6C2D3'}
df_column_pairs = [
    {'df': taxa_df, 'column': 'taxa', 'columns': taxas},
    {'df': f_groups_df, 'column': 'Functional_type', 'columns': f_groups}]
remap_stations = {'GOM4_9_surf': 'GOM_9_surf', 'GOM4_47_surf': '47'}
skip = set([140, 64])

def get_full_name(station):
    full_name = 'GOM4_' + str(station) + '_surf'
    full_name = remap_stations[full_name] if full_name in remap_stations else full_name
    return full_name

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

#count functional groups and taxa
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
f_groups_df.to_csv(os.path.join(root, 'functional-groups.csv'))


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
    ax.set_xticks(x + 0.5, labels=[station_distance_map[i] for i in stations])
    plt.title(transect)
    plt.savefig(os.path.join(os.getcwd(), 'figures', transect + '.png'), dpi=200)
    plt.close()

print('done.')