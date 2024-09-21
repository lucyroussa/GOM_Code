import pandas as pd
import numpy as np

df = pd.read_csv('GOMECC4_ENV_DATA.csv')

station2index = {}

for index, row in df.iterrows():
    station = int(row['STATION'])
    if station in station2index:
        station2index[station].append(index)
    else:
        station2index[station] = [index]

ndf = pd.DataFrame(columns=df.drop(['TRANSECT'], axis=1).columns)

i = 0
for station, index in station2index.items():
    temp = df.iloc[index].drop(['TRANSECT'], axis=1)
    meaned = temp.replace(0, np.nan).mean(axis=0)
    ndf.loc[i] = meaned
    i += 1

ndf.to_csv('test.csv')