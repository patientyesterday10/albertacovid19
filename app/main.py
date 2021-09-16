
import datetime
import logging
import os
import subprocess

from alberta.munge import get_last_report
from utils.telegram import telegram_send_file, telegram_bot_sendtext

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

mode = os.getenv('TELE_MODE')
if mode == "DEV":
    logger.setLevel(logging.DEBUG)
    logger.info("Running in DEV mode.")
    os.environ['TELE_ID'] = os.getenv('TELE_ID_DEV')
else:
    logger.setLevel(logging.INFO)
    os.environ['TELE_ID'] = os.getenv('TELE_ID_PROD')

# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    date = datetime.date.today().strftime('%Y-%m-%d')

    if os.path.exists(os.path.join('data', 'covid19dataexport.csv')):
        age = (datetime.datetime.now().timestamp() - os.path.getmtime(os.path.join('data', 'covid19dataexport.csv')))
    else:
        age = 3600 * 24

    if age > (3600 * 12.):
        logger.info("Starting fetch.")
        import requests
        url = "https://www.alberta.ca/data/stats/covid-19-alberta-statistics-data.csv"
        r = requests.get(url)
        with open(os.path.join('data', 'covid19dataexport.csv'),'wb') as f:
            f.write(r.content)

    else:
        logger.info("Using cached data.")

    logger.info("Calling R script for plot generation.")
    subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/covid_fit.R"])
    subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/ages.R"])
    subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/ab_vs_on.R"])
    subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/cases_by_location.R"])
    subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/waves.R"])
    subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/yyc_cases.R"])
    subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/yoy.R"])
    try:
        subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/kids.R"])
    except Exception as e:
        print(e)
    try:
        subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/icu.R"])
    except Exception as e:
        print(e)
    try:
        subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/icu_model.R"])
    except Exception as e:
        print(e)
    try:
        subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/hospitalization_model.R"])
    except Exception as e:
        print(e)

    #subprocess.call(["/usr/bin/Rscript", "--vanilla", "R/severe_outcomes.R"])


    # Push results to Telegram:
    last_update = get_last_report()

    telegram_bot_sendtext("COVID19 Report for " + str(datetime.datetime.now().strftime("%B %d, %Y %H:%M")))
    telegram_bot_sendtext("Latest Data for " + last_update['date'] + ": " + str(last_update['cases']) + " new cases.")

    files = {
        'output/new_cases_by_region.png': "Daily New Cases by Region",
        'output/covid_weekly.png': "Daily New Cases, by Weekday",
        'output/covid_weekly_yyc.png': "Daily New Cases, by Weekday",
        'output/covid_age.png': "Daily New Cases by Age Group",
        'output/facet_cases_by_region.png': "Daily New Cases, Faceted by Region",
        'output/covid_on_ab_normalized.png': "Alberta / Ontario Comparison (Normalized)",
        'output/yyc_cases.png': "New Cases in Calgary",
        'output/yyc_case_details.png': "YYC Case Details",
        'output/yyc_cases_by_age.png': "YYC Cases by Age",
        'output/kids_cases.png': "Cases in Kids",
        "output/yyc_fcst.png": "YYC Forecast",
        "output/ab_fcst.png": "AB Fcst",
        "output/yoy.png": "2021 vs. 2020 Snapshot",
        "output/yoy_aligned_facet.png": "2021 vs. 2020 Snapshot Aligned",
        "output/hosp_forecast.png": "Hospitalization Forecast Model",
        "output/icu_forecast.png": "ICU Forecast Model (1/2)",
        "output/icu_forecast_zoom.png": "ICU Forecast Model (2/2)",
        "output/icu_case_deaths.png": "ICU, Cases, Hospitalizations & Deaths",
        "output/" + str(datetime.date.today()) + "_-_ab_covid_time_series.csv": "Timeseries CSV",
        "output/" + str(datetime.date.today()) + "_-_alberta_covid_tables_web.xlsx": "Additional Tables",
    }

    for k,v in files.items():
        try:
            telegram_send_file(k,file_caption=v)
        except Exception as e:
            print(e)

