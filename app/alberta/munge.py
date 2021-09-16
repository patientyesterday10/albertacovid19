#  Copyright (c) 2020. PatientYesterday10. All Rights Reserved.

import os

import pandas as pd


def load_ab_data(wd=os.getcwd()):
    dt = pd.read_csv(os.path.join(wd, "data", "covid19dataexport.csv"))
    return dt

def get_last_report(wd=os.getcwd()):
    dt = load_ab_data(wd=wd)
    last = dt.groupby(by=["Date reported"]).agg({'Gender': 'count'})[-1:]['Gender']
    return {'date': last.index[0], 'cases': last[0]}
