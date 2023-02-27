import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import mannwhitneyu
from scipy.stats import kruskal
import scikit_posthocs as sp

pop = pd.read_csv('20210706_Pop_Fig1.csv')
deaths = pd.read_csv('20210706_Mortality_Fig1.csv')
cases = pd.read_csv('20210706_Infection_Fig1.csv')


ne_pop = pop[pop['Region'] == 'Northeast']
ne_deaths = deaths[deaths['Region'] == 'Northeast']
ne_cases = cases[cases['Region'] == 'Northeast']

p_data = ne_pop[ne_pop['Race'] == 'NHB']
i_data = ne_cases[ne_cases['Race'] == 'NHB']
d_data = ne_deaths[ne_deaths['Race'] == 'NHB']

raceindex = [1, 2, 3, 4]
race = ['', 'NHW', 'NHB', 'Hispanic', 'NHA']
racename = ['', 'NHW', 'NHB', 'Hispanic', 'NHA']

ne_pop = pop[pop['Region'] == 'Northeast']
ne_deaths = deaths[deaths['Region'] == 'Northeast']
ne_cases = cases[cases['Region'] == 'Northeast']

fig, ax = plt.subplots(1, 4,figsize=(60,18))

for i in raceindex:
  p_data = ne_pop[ne_pop['Race'] == race[i]]
  i_data = ne_cases[ne_cases['Race'] == race[i]]
  d_data = ne_deaths[ne_deaths['Race'] == race[i]]

  q2_p = [np.quantile(p_data['Population'].to_list(), 0.5)]
  q2_i = [np.quantile(i_data['Population'].to_list(), 0.5)]
  q2_d = [np.quantile(d_data['Population'].to_list(), 0.5)]

  pdata = []
  pdata.append(p_data['Population'].tolist())
  pdata.append(i_data['Population'].tolist())
  pdata.append(d_data['Population'].tolist())
  
  c = 'black'
  bp = ax[i-1].boxplot(pdata[0], labels = ['Population'], positions = [1], patch_artist=True, widths=0.2,
            boxprops=dict(facecolor='w', color=c, linewidth=3.0),
            capprops=dict(color=c),
            whiskerprops=dict(color=c, linewidth=3.0),
            flierprops=dict(color=c, markeredgecolor=c),
            medianprops=dict(color='black', linewidth=3.0))

  # for box in bp['boxes']:
  #     box.set(hatch = '\\')
  
  c = 'black'
  bp = ax[i-1].boxplot(pdata[1], labels = ['Infection'], positions = [2], patch_artist=True, widths=0.2,
            boxprops=dict(facecolor='w', color=c, linewidth=3.0),
            capprops=dict(color=c),
            whiskerprops=dict(color=c, linewidth=3.0),
            flierprops=dict(color=c, markeredgecolor=c),
            medianprops=dict(color='black', linewidth=3.0))

  for box in bp['boxes']:
      box.set(hatch = '//')

  c = 'black'
  bp = ax[i-1].boxplot(pdata[2], labels = ['Mortality'], positions = [3], patch_artist=True, widths=0.2,
            boxprops=dict(facecolor='lightgray', color=c, linewidth=3.0),
            capprops=dict(color=c),
            whiskerprops=dict(color=c, linewidth=3.0),
            flierprops=dict(color=c, markeredgecolor=c),
            medianprops=dict(color='black', linewidth=3.0))

  # for box in bp['boxes']:
  #     box.set(hatch = '//')

  ax[i-1].text(1+0.12, q2_p[0]-1, str(round(q2_p[0],2)), fontsize = 32)
  ax[i-1].text(2+0.12, q2_i[0]-1, str(round(q2_i[0],2)), fontsize = 32)
  ax[i-1].text(3+0.12, q2_d[0]-1, str(round(q2_d[0],2)), fontsize = 32)

  ax[i-1].tick_params(axis='y', which='major', labelsize=40)
  ax[i-1].tick_params(axis='x', which='major', labelsize=38)

  ax[i-1].set_ylim(0, 102)

  ax[i-1].set_title(racename[i], fontsize = 48)

  print(race[i])
  print('Population Kruskal-Wallis', kruskal(p_data['Population'].tolist(), i_data['Population'].tolist(), d_data['Population'].tolist()))
  print(sp.posthoc_dunn([p_data['Population'].tolist(), i_data['Population'].tolist(), d_data['Population'].tolist()]))

plt.text(-3.8, 110, 'Northeast',
         horizontalalignment='center',
         fontsize=52)
plt.savefig("Northeast.svg")
plt.show()