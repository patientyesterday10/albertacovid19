#  Copyright (c) 2020. PatientYesterday10. All Rights Reserved.

import os

import requests


def telegram_bot_sendtext(bot_message):
    send_text = (
            "https://api.telegram.org/bot"
            + os.getenv("TELE_TOKEN")
            + "/sendMessage?chat_id="
            + os.getenv("TELE_ID")
            + "&parse_mode=Markdown&text="
            + bot_message
    )
    response = requests.get(send_text)
    return response.json()


def telegram_send_file(file_path, file_caption=""):
    data = {"chat_id": os.getenv("TELE_ID"), "caption": file_caption}
    url = "https://api.telegram.org/bot%s/sendDocument" % os.getenv("TELE_TOKEN")
    with open(file_path, "rb") as file_data:
        response = requests.post(url, data=data, files={"document": file_data})
    return response.json()
