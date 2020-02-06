
import requests
import pandas as pd

url = "https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/export?format=xlsx"

r = requests.get(url)
with open('data/data.xlsx', 'wb') as f:
    f.write(r.content)

sheets = ['Confirmed', 'Death', 'Recovered']

for sheet in sheets:
    pd.read_excel("data/data.xlsx",
                  sheet_name=sheet).to_csv(f"data/{sheet}.csv", index=None)
