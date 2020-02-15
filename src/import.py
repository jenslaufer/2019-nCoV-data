
import requests
import pandas as pd

url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-{0}.csv"


sheets = ['Confirmed', 'Deaths', 'Recovered']

for sheet in sheets:
    r = requests.get(url.format(sheet))
    with open('data/{0}.csv'.format(sheet), 'wb') as f:
        f.write(r.content)
