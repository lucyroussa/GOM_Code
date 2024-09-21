import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

#setup dataframes
main_df = pd.read_csv(os.path.join(os.getcwd(), 'data', 'Taxa_Functional_grouping_bar_graph.csv'))
#remove unneeded data
main_df = main_df[(main_df["taxa"] == 'CILIATES') | (main_df["taxa"] == 'DIATOMS') | (main_df["taxa"] == 'DINOFLAGELLATES')]
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


#count functional groups and taxa
for site_i in [19, 21, 26]:
    column_name = 'GOM4_' + str(site_i) + '_surf'
    #update taxas and functional groups
    for obj in df_column_pairs:
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
print(f_groups_df)

#plot
x = ['in shore', 'mid', 'off shore']
previous = [0] * len(f_groups_df.loc[f_groups_df.index[0]])
for i in f_groups_df.index:
    data = f_groups_df.loc[i]
    plt.bar(x, data, bottom=previous, color=color_map[i])
    previous += data
ax = plt.gca()
legend_elements = [Patch(facecolor=color_map[f], label=f) for f in f_groups]
ax.legend(handles=legend_elements)
plt.title('f_groups_df')
plt.show()