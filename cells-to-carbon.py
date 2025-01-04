import os
import pandas as pd

#setup dataframes
root = os.path.join(os.getcwd(), 'data')
counts = pd.read_csv(os.path.join(root, 'Taxa_functional_all_stations.csv'))
carbon_factors = pd.read_csv(os.path.join(root, 'carbon-factors-pg*cell-1.csv'))

def m2d_class(m):
    if m < 10:
        return 'surf'
    elif 10 <= m < 200:
        return 'subsurface'
    elif 200 <= m <= 1000:
        return 'D'
    elif m > 1000:
        return 'verydeep'
    
depth_abbrvs = {'surface': 'surf', 'subsurface': 'subsurface', 'deep': 'D', 'very deep': 'verydeep', 'chl max': 'CLM'}
inv_d_abbrv = {v: k for k, v in depth_abbrvs.items()}

#convert over each count value to carbon
for index, row in counts.iterrows():
    taxa = row['taxa'].title()
    for i_name in row.index:
        if i_name in ['SAMPLE_ID', 'taxa', 'Functional_type', '9_surf', '47_surf', '33_surf']:
            continue
        station, depth = i_name.split('_')
        station = int(station)
        station = 64 if station == 63 else station
        df_station = carbon_factors.loc[carbon_factors['Station'] == station]
        factor = df_station.loc[df_station['Depth_class'] == inv_d_abbrv[depth]][taxa]
        if factor.size < 1:
            print('error finding carbon factor for', taxa, station, depth)
            continue
        factor = factor[factor.index[0]]
        counts.loc[index, i_name] = counts.loc[index, i_name] * factor / 1000

counts.to_csv(os.path.join(root, 'carbon-taxa.csv'))
print('done.')
