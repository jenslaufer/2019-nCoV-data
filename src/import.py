
import requests

url = f"https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/export?format=csv"

sheets = ['Confirmed', 'Deaths', 'Recovered']


for sheet in sheets:
    r = requests.get(url)
    with open(f'data/{sheet}.csv', 'wb') as f:
        f.write(r.content)
