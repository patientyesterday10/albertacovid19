import datetime
import logging
import os
import shutil
import tempfile
import time

from selenium import webdriver

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)


def fetch_ab_data(date=None):
    if date is None:
        date = datetime.date.today().strftime('%Y-%m-%d')

    with tempfile.TemporaryDirectory() as download_dir:
        logger.info("Download Directory: "+download_dir)
        chrome_options = webdriver.ChromeOptions()
        chrome_options.add_experimental_option('prefs', {'download.default_directory': download_dir})
        chrome_options.add_argument("--no-sandbox")
        chrome_options.add_argument("--headless")
        driver = webdriver.Chrome(options=chrome_options)
        driver.get('https://www.alberta.ca/stats/covid-19-alberta-statistics.htm')
        time.sleep(1)
        data_tab = driver.find_element_by_link_text('Data export')
        data_tab.click()
        time.sleep(1)
        tmp = driver.find_element_by_class_name('dt-buttons')
        csv_button = tmp.find_elements_by_tag_name('button')
        for btn in csv_button:
            print(btn.text)
            if btn.text == "CSV":
                break

        btn.click()
        time.sleep(5)
        driver.close()
        driver.quit()

        try:
            os.mkdir('data')
        except FileExistsError as e:
            logger.debug("Directory data already exists")
            logger.debug(e)
            pass
        except Exception as e:
            logger.error(e)
            print(e)
            raise e

        try:
            for root, dirs, files in os.walk(download_dir):
                for filename in files:
                    print(download_dir)
                    print(dirs)
                    print(filename)
                    shutil.move(os.path.join(download_dir,filename),os.path.join("data",filename))
        except Exception as e:
            logger.error(e)
            print(e)

        return os.path.join(os.getcwd(), 'data', 'covid19dataexport.csv')
