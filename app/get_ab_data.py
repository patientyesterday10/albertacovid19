#  Copyright (c) 2021. PatientYesterday10. All Rights Reserved.

import datetime
import json
import os.path
import re
import sys

import pandas as pd
from bs4 import BeautifulSoup
from requests_html import HTMLSession

if __name__ == '__main__':
    url = "https://www.alberta.ca/stats/covid-19-alberta-statistics.htm"
    session = HTMLSession()
    resp = session.get(url)
    resp.html.render(timeout=45)
    soup = BeautifulSoup(resp.html.html, 'html.parser')

    # ----- GET HTML TABLES -----

    # Fix colspan where set to 100% (breaks pd.read_html())
    for td in soup.find_all('td'):
        if td.has_attr('colspan'):
            if td['colspan'] == "100%":
                new_td = td
                new_td['colspan'] = 1
                td.replace_with(new_td)

    dts = pd.read_html(soup.prettify())
    table_names = re.findall('Table [0-9]{2}.+(?=\n)', string=soup.text)
    table_names_short = re.findall('(?<=Table [0-9.]{2}).+(?=\n)', soup.text)
    table_names_clean = [re.sub('[^0-9a-zA-Z ]+', '', string)[0:31] for string in table_names_short]

    if os.path.exists("output/" + str(datetime.date.today()) + "_-_alberta_covid_tables_web.xlsx"):
        os.unlink("output/" + str(datetime.date.today()) + "_-_alberta_covid_tables_web.xlsx")

    with pd.ExcelWriter("output/" + str(datetime.date.today()) + "_-_alberta_covid_tables_web.xlsx") as writer:
        for i in range(len(dts) - 1):
            try:
                dt = dts[i]
                dt.to_excel(writer, sheet_name=table_names_clean[i])
            except:
                print("Unexpected error in "+table_names_clean[i]+":", sys.exc_info()[0])

    # ----- GET FIGURE DATA (e.g. PLOTLY TIME SERIES) -----
    # Get Figure Data (plotly / json)
    figures = soup.find_all('div', class_="figure")
    figure_data = []
    for figure in figures:
        caption = figure.find('p', class_="caption")
        data = figure.find('script').string

        if caption is not None:
            tmp = json.loads(data)
            tmp = tmp.get('x')
            if tmp is not None:
                if tmp.get('data') is not None:
                    figure_data.append({'caption': caption.text, 'data': tmp.get('data')})

    with open("output/" + str(datetime.date.today()) + "_alberta_covid_figure_data.json", 'w', encoding='utf-8') as f:
        json.dump(figure_data, f, ensure_ascii=False, indent=2)

    # ----- GET CSV EXPORTS -----
    # download data exports
    csv_links = soup.find("div", id="data-export").find_all("a", class_="goa-cta", href=True)
    for csv in csv_links:
        url = "https://www.alberta.ca" + csv['href']
        storage_options = {'User-Agent': 'Mozilla/5.0'}
        dt = pd.read_csv(url, storage_options=storage_options)
        dt.to_csv("output/" + str(datetime.date.today()) + "_-_" + os.path.basename(csv['href']))

    # ----- CONVERT HOSPITALIZATIONS (ICU + NON-ICU, and DEATHS) to time-series -----

    # Hospitalizations (ICU and Non-ICU)
    hosp = list(filter(lambda item: "Number of current COVID-19 patients in hospital" in item['caption'], figure_data))

    series1 = hosp[0]['data'][0]['name']
    df1 = pd.DataFrame(data={
        'date': hosp[0]['data'][0]['x'],
        hosp[0]['data'][0]['name']: hosp[0]['data'][0]['y']
    })

    series2 = hosp[0]['data'][1]['name']
    df2 = pd.DataFrame(data={
        'date': hosp[0]['data'][1]['x'],
        hosp[0]['data'][1]['name']: hosp[0]['data'][1]['y']
    })

    hosp = df1.merge(df2, on='date', how='outer')
    hosp.sort_values(by='date', inplace=True)

    # Calculate total hospitalizations
    total_hosp = hosp[series1] + hosp[series2]
    hosp['total_hosp'] = total_hosp
    hosp['date'] = pd.to_datetime(hosp['date'])

    # Deaths
    deaths = list(filter(lambda item: "Daily COVID-19 attributed deaths" in item['caption'], figure_data))
    deaths = pd.DataFrame(data={
        'date': deaths[0]['data'][0]['x'],
        'deaths': deaths[0]['data'][0]['y']
    })
    deaths['date'] = pd.to_datetime(deaths['date'])

    # Positivity Rate
    positivity = list(filter(lambda item: "Cumulative and daily test positivity rate" in item['caption'], figure_data))
    positivity = pd.DataFrame(data={
        'date': positivity[0]['data'][0]['x'],
        'positivity': positivity[0]['data'][0]['y']
    })
    positivity['date'] = pd.to_datetime(positivity['date'], unit='d')

    # Active Cases
    active = list(filter(lambda item: "COVID-19 cases in Alberta by day and case status" in item['caption'], figure_data))
    active = pd.DataFrame(data={
        'date': active[0]['data'][1]['x'],
        'active': active[0]['data'][1]['y']
    })
    active['date'] = pd.to_datetime(active['date'])

    # Load case data:
    cases = pd.read_csv("output/" + str(datetime.date.today()) + "_-_covid-19-alberta-statistics-data.csv")
    cases = cases[['Date reported', 'Case type']].groupby(['Date reported']).agg('count').reset_index()
    cases.columns = ['date', 'new_cases']
    cases['date'] = pd.to_datetime(cases['date'])


    # Combine and write timeseries:
    ab_time_series = cases.merge(
        active.merge(
            hosp.merge(
                deaths.merge(positivity, on="date", how='outer'),
                on="date", how='outer'),
            on="date", how='outer'),
        on="date", how='outer')
    ab_time_series.sort_values(by='date', inplace=True)
    ab_time_series.to_csv("output/" + str(datetime.date.today()) + "_-_ab_covid_time_series.csv", index=False)
    ab_time_series.to_csv("output/ab_covid_time_series.csv", index=False)

