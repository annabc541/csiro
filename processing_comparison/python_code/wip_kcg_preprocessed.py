# -*- coding: utf-8 -*-
"""
Created on Thu Nov 23 14:03:48 2023

@author: anna_
"""

import pandas as pd
from mpire import WorkerPool
import os
import xlrd
from xlrd import xldate
import datetime
import time
import numpy
from operator import attrgetter
import functools
import glob
from scipy import signal
#import remove_spikes
import matplotlib.pyplot as plt
import sys
import numpy as np
import datatable as dt
from mpire import WorkerPool
pd.options.mode.copy_on_write = True

def clean_up_df(df:pd.DataFrame) -> pd.DataFrame:
    rename_dict = {'CH1_Hz': 'NOx_counts','Inlet_BLC': 'NO2_converter','zero_valve_1': 'NOx_zero_k', 'Inlet_NOx': 'NOx_cal_k', 'NO_valve': 'calMFC_k','NOx_cal': 'GPTLamp_k', 'Inlet_ZA': 'NOx_za_k'}
    df = df.rename(columns = rename_dict)
    return df

def fix_key_cols(df:pd.DataFrame,key_cols) -> pd.DataFrame:
    for col in key_cols:
        df[col] = df[col].fillna(method = "bfill").fillna(0) > 0
    return df

def create_new_keys(df:pd.DataFrame)-> pd.DataFrame:
    orig_cols = set(df.columns.values)
    df = df.assign(\
        Zero = (df["NOx_zero_k"] & ~df["NO2_converter"] & ~df["NOx_cal_k"] & ~df["NOx_za_k"]),
        Zero_NO2 = (df["NOx_zero_k"] & df["NO2_converter"] & ~df["NOx_cal_k"] & ~df["NOx_za_k"]),
        NO_Hz = (~df["NOx_zero_k"] & ~df["NO2_converter"] & ~df["NOx_cal_k"] & ~df["NOx_za_k"] & df["Inlet_SS"]),
        NOx_Hz = (~df["NOx_zero_k"] & df["NO2_converter"] & ~df["NOx_cal_k"] & ~df["NOx_za_k"]),
        Cal_zero = (df["NOx_cal_k"] & df["calMFC_k"] & ~df["NO2_converter"] & ~df["NOx_za_k"] & df["NOx_zero_k"]),
        Cal_zero_NO2 = (df["NOx_cal_k"] & df["calMFC_k"] & df["NO2_converter"] & ~df["NOx_za_k"] & df["NOx_zero_k"]),
        NO_u = (df["NOx_cal_k"] & df["calMFC_k"] & ~df["GPTLamp_k"] & ~df["NO2_converter"] & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & df["Inlet_SS"]),
        NO2_u = (df["NOx_cal_k"] & df["calMFC_k"] & ~df["GPTLamp_k"] & df["NO2_converter"] & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & df["Inlet_SS"]),
        NO2_t = (df["NOx_cal_k"] & df["calMFC_k"] & df["GPTLamp_k"] & df["NO2_converter"]  & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & df["Inlet_SS"]),
        NO_t = (df["NOx_cal_k"] & df["calMFC_k"] & df["GPTLamp_k"] & ~df["NO2_converter"]  & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & df["Inlet_SS"]),
        Zero_artefact = (~df["NOx_cal_k"] & df["NOx_zero_k"] & df["NOx_za_k"] & ~df["NO2_converter"]),
        Zero_artefact_NO2 = (~df["NOx_cal_k"] & df["NOx_zero_k"] & df["NOx_za_k"] & df["NO2_converter"]),
        PAG_Zero_NO = (~df["NOx_cal_k"] & ~df["NOx_zero_k"] & df["NOx_za_k"] & ~df["NO2_converter"]),
        PAG_Zero_NO2 = (~df["NOx_cal_k"] & ~df["NOx_zero_k"] & df["NOx_za_k"] & df["NO2_converter"]),
        Zero_Hz = (~df["NOx_cal_k"] & df["NOx_zero_k"] & ~df["NOx_za_k"])\
        )
    iter_cols = list(set(df.columns.values).difference(orig_cols)) + ["NOx_cal_k","NOx_za_k","calMFC_k"]
    for col in iter_cols:
        df[col + "_iter"] = (df[col] != df[col].shift()).cumsum()
    
    return df

curr_folder = os.path.dirname(os.path.realpath(sys.argv[0]))
dates_to_remove = pd.read_csv(os.path.join(curr_folder,"Analysing KCG data\config","remove_date_times.mww"),delimiter = "\t",usecols = ["Start Date","End Date"],parse_dates = ["Start Date","End Date"])
df = pd.read_csv(os.path.join(curr_folder,"ED_CG_AQDNOx_202210_2402031409.csv"),index_col = "Hour_since_1970",on_bad_lines = "skip").sort_index()

df["Timestamp"] = df.index.values
df = df.loc[df.index.notnull()]
df = clean_up_df(df)
df.loc[df["NOx_counts"] < 0,"NOx_counts"] = np.nan


key_cols = ["NOx_zero_k","NOx_cal_k","calMFC_k","GPTLamp_k","NOx_za_k","NO2_converter","Inlet_SS"]
df = fix_key_cols(df,key_cols)
df = create_new_keys(df)

df = drop_dates(df,dates_to_remove)