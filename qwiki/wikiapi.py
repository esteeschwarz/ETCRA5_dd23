import pageviewapi
import datetime

d = datetime.datetime.today()
print(d)
#from datetime import datetime

today = d.strftime("%Y%m%d")


xdf=pageviewapi.per_article('de.wikipedia', 'Gazastreifen', '20231005', today,
  access='all-access', agent='all-agents', granularity='daily')
xdf
# import sys
# 
# #base
# !pip install --upgrade pip
# !pip install "mwparserfromhell>=0.5.0"
# !pip install "wikitextparser>=0.47.5"
# !pip install pywikibot
# !pip install git+https://github.com/Commonists/pageview-api.git
# !pip install pageviewapi

import json
x = json.dumps(xdf)
y = json.loads(x)
x
# the result is a Python dictionary:
# print(y["items"][0]["timestamp"])
# json.dumps(xdf, indent=4, sort_keys=True)
# !pip install matplotlib
# import matplotlib.pyplot as plt
# tsl = [0,0]
# vl = [0,0]
# for k in y["items"]:
#   #for i in k:
#   ts = k["timestamp"]
#   tsl.append(ts)
# #  vl.append(views)
#   views = k["views"]
#   vl.append(views)
#   print(ts,views)
# #print(y)
# #tsl = [0,0]
# #tsl.append(9)
# tsl = tsl[2:len(tsl)]
# #vl = vl[2:8]
# #index = [2,3,4,5,6,7,8]
# #index =int(index)
# #tsls[index]
# 
# vl = vl[2:len(vl)]
# #import matplotlib.dates as mdates
# from datetime import datetime
# 
# tsl
# vl
# dates = [datetime.strptime(d, "%Y%m%d00") for d in tsl]
# #datetime.strptime("2023101500", "%Y%m%d00")
# 
# 
# fig, axs = plt.subplots(3, 1, figsize=(6.4, 7), layout='constrained')
# for ax in axs:
#     ax = plt.plot(tsl,vl)
#     ax.xaxis.set_major_locator(mdates.MonthLocator(bymonth=(1, 7)))
# axs[1]
# ax
# #ax.xaxis.set_major_formatter(cdf)
# for label in ax.get_xticklabels(which='major'):
#     label.set(rotation=30, horizontalalignment='right')
# 
# 
# #cdf = mpl.dates.ConciseDateFormatter(ax.xaxis.get_major_locator())
# #ax.xaxis.set_major_formatter(cdf)
# plt.show()  
# 
# #!pip install np
# import numpy as np
# import matplotlib as mpl
# 
# #!pip install mpl
# fig, ax = plt.subplots(figsize=(5, 2.7), layout='constrained')
# dates = np.arange(np.datetime64('2021-11-15'), np.datetime64('2021-12-25'),
#                   np.timedelta64(1, 'h'))
# data = np.cumsum(np.random.randn(len(dates)))
# ax.plot(dates, data)
# cdf = mpl.dates.ConciseDateFormatter(ax.xaxis.get_major_locator())
# plt.show()
# 
# 
# fig, axs = plt.subplots(3, 1, figsize=(6.4, 7), layout='constrained')
# # common to all three:
# for ax in axs:
#     ax.plot(tsl,vl)
#     # Major ticks every half year, minor ticks every month,
#     ax.xaxis.set_major_locator(mdates.DayLocator())
#     ax.xaxis.set_minor_locator(mdates.MonthLocator())
#     ax.grid(True)
#     ax.set_ylabel(r'Price [\$]')
# 
# # different formats:
# ax = axs[0]
# ax.set_title('DefaultFormatter', loc='left', y=0.85, x=0.02, fontsize='medium')
# 
# ax = axs[1]
# ax.set_title('ConciseFormatter', loc='left', y=0.85, x=0.02, fontsize='medium')
# ax.xaxis.set_major_formatter(
#     mdates.ConciseDateFormatter(ax.xaxis.get_major_locator()))
# 
# ax = axs[2]
# ax.set_title('Manual DateFormatter', loc='left', y=0.85, x=0.02,
#              fontsize='medium')
# # Text in the x-axis will be displayed in 'YYYY-mm' format.
# ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%b'))
# # Rotates and right-aligns the x labels so they don't crowd each other.
# for label in ax.get_xticklabels(which='major'):
#     label.set(rotation=30, horizontalalignment='right')
# 
# plt.show()

f = open("wiki_gaza.json", "a")
f.write(x)
f.close()


