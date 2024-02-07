# -*- coding: utf-8 -*-
"""
Created on Mon Oct 23 11:02:34 2023

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
import remove_spikes
import matplotlib.pyplot as plt
import sys
import numpy as np
import datatable as dt
from mpire import WorkerPool
pd.options.mode.copy_on_write = True

def filter_flist(flist,year):
    file_year = str(year)[-2:]
    flist = [i for i in flist if int(str(int(file_year)-1) + "12" + "25") < int(i.split("_")[-2]) <  int(str(int(file_year)+1) + "01" + "05")]
    return flist

def read_file(fname):
    required_cols = ["Hour_since_1970","CH1_Hz","Inlet_BLC","NOx_cal","Inlet_NOx","Inlet_ZA","NO_Cal_flow","Inlet_SampleFlow_1","NO_valve","zero_valve_1", "OxygenFlow_1", "PMT_T", "Rxn_Cell_T", "Rxn_Vessel_Pressure","Inlet_SS"]
    df = pd.read_csv(fname,usecols = required_cols,index_col = "Hour_since_1970",on_bad_lines = "skip")
    return df

def clean_up_df(df:pd.DataFrame) -> pd.DataFrame:
    rename_dict = {'Hour_since_1970':'TheTime','CH1_Hz': 'NOx_counts','Inlet_BLC': 'NO2_converter','zero_valve_1': 'NOx_zero_k', 'Inlet_NOx': 'NOx_cal_k', 'NO_valve': 'calMFC_k','NOx_cal': 'GPTLamp_k', 'Inlet_ZA': 'NOx_za_k'}
    df = df.rename(columns = rename_dict)
    return df

def drop_dates(df:pd.DataFrame,dates_to_remove:pd.DataFrame)->pd.DataFrame:
    min_date = df.index.values[0]
    max_date = df.index.values[-1]
    dates_to_remove = dates_to_remove.loc[dates_to_remove["Start Date"].between(min_date,max_date)|dates_to_remove["End Date"].between(min_date,max_date)]
    
    df["drop_me"] = False
    for num,row in dates_to_remove.iterrows():
        df.loc[row["Start Date"]:row["End Date"],"drop_me"] = True
    df = df.loc[~df["drop_me"]].drop("drop_me",axis = 1)
    return df

def drop_dates_dt(df:pd.DataFrame,dates_to_remove:pd.DataFrame) -> pd.DataFrame:
    df = dt.Frame(df)
    for num,row in dates_to_remove.iterrows():
        del df[((dt.f["Timestamp"] < excel_date(row["End Date"]))&(dt.f["Timestamp"] > excel_date(row["Start Date"]))),:]
    del df[:,'NOx_counts.0']
    return df.to_pandas()

def excel_date(input_date):

    components = ('year', 'month', 'day', 'hour', 'minute', 'second')
    frac = input_date.microsecond / (86400 * 10**6)  # divide by microseconds in one day
    return xldate.xldate_from_datetime_tuple(attrgetter(*components)(input_date), 0) + frac

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
        Cal_zero_u = (df["NOx_cal_k"] & df["calMFC_k"] & ~df["GPTLamp_k"] & ~df["NO2_converter"] & ~df["NOx_za_k"] & df["NOx_zero_k"]),
        Cal_zero_t = (df["NOx_cal_k"] & df["calMFC_k"] & df["GPTLamp_k"] & ~df["NO2_converter"] & ~df["NOx_za_k"] & df["NOx_zero_k"]),
        Cal_zero_NO2 = (df["NOx_cal_k"] & df["calMFC_k"] & df["NO2_converter"] & ~df["NOx_za_k"] & df["NOx_zero_k"]),
        NO_u = (df["NOx_cal_k"] & df["calMFC_k"] & ~df["GPTLamp_k"] & ~df["NO2_converter"] & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & df["Inlet_SS"]),
        NO_u_nox = (df["NOx_cal_k"] & df["calMFC_k"] & ~df["GPTLamp_k"] & ~df["NO2_converter"] & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & ~df["Inlet_SS"]),
        NO2_u = (df["NOx_cal_k"] & df["calMFC_k"] & ~df["GPTLamp_k"] & df["NO2_converter"] & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & df["Inlet_SS"]),
        NO2_t = (df["NOx_cal_k"] & df["calMFC_k"] & df["GPTLamp_k"] & df["NO2_converter"]  & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & df["Inlet_SS"]),
        NO_t = (df["NOx_cal_k"] & df["calMFC_k"] & df["GPTLamp_k"] & ~df["NO2_converter"]  & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & df["Inlet_SS"]),
        NO_t_nox = (df["NOx_cal_k"] & df["calMFC_k"] & df["GPTLamp_k"] & ~df["NO2_converter"]  & ~df["NOx_zero_k"] & ~df["NOx_za_k"] & ~df["Inlet_SS"]),
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

def pd_stats(df:pd.DataFrame,key:str,stat_name:str,delay:float,agg_col:str = "NOx_counts",period_key:str = "",take_last_in_period = False) -> pd.DataFrame:
    return df.pipe(lambda a: a if period_key == "" else a.loc[df[period_key] == True])[[x for x in ["TheTime",key + "_iter",agg_col,period_key + "_iter"] if x != "_iter"]]\
                          .pipe(lambda y:y if period_key == "" else y.assign(group_time = y[[period_key + "_iter","TheTime"]].groupby(period_key + "_iter").transform(max)))\
                          .loc[df[key] == True]\
                          .assign(min_time = df.loc[df[key] == True].groupby(key + "_iter",as_index = False)["TheTime"].transform(min))\
                          .pipe(lambda x:x.loc[x["TheTime"] > x["min_time"] + delay])\
                          .groupby(key + "_iter",as_index = False)\
                          .agg(**{stat_name + "_mean":(agg_col,"median"),stat_name + "_sd":(agg_col,"std"),stat_name + "_count":(agg_col,"count"),**({period_key + "_iter":(period_key + "_iter","max"),"TheTime":("group_time","max")} if ((period_key != "") and (take_last_in_period == True)) else {"TheTime":("TheTime","max")})})\
                          .pipe(lambda z: z if take_last_in_period == False else z.groupby(period_key + "_iter",as_index = False).mean())\
                          .set_index("TheTime")

def pd_stats_new(df:pd.DataFrame,key:str,stat_name:str,delay:float,agg_col:str = "NOx_counts",period_key:str = "",take_last_in_period = False) -> pd.DataFrame:
    if period_key == "":
        return df.loc[df[key] == True,[x for x in ["TheTime",key + "_iter",agg_col,period_key + "_iter"] if x != "_iter"]]\
               .assign(min_time = df.loc[df[key] == True].groupby(key + "_iter",as_index = False)["TheTime"].transform(min))\
                .pipe(lambda x:x.loc[x["TheTime"] > x["min_time"] + delay])\
                .groupby(key + "_iter",as_index = False)\
                .agg(**{stat_name + "_mean":(agg_col,"mean"),stat_name + "_sd":(agg_col,"std"),stat_name + "_count":(agg_col,"count"),"TheTime":("TheTime","max")})\
                .set_index("TheTime")

    elif period_key != "" and take_last_in_period == True:
        return df.loc[df[period_key] == True,[x for x in ["TheTime",key + "_iter",agg_col,period_key + "_iter"] if x != "_iter"]]\
                              .assign(group_time = df[[period_key + "_iter","TheTime"]].groupby(period_key + "_iter").transform(max))\
                              .loc[df[key] == True]\
                              .assign(min_time = df.loc[df[key] == True].groupby(key + "_iter",as_index = False)["TheTime"].transform(min))\
                              .pipe(lambda x:x.loc[x["TheTime"] > x["min_time"] + delay])\
                              .groupby(key + "_iter",as_index = False)\
                              .agg(**{stat_name + "_mean":(agg_col,"mean"),stat_name + "_sd":(agg_col,"std"),stat_name + "_count":(agg_col,"count"),**({period_key + "_iter":(period_key + "_iter","max"),"TheTime":("group_time","max")} if ((period_key != "") and (take_last_in_period == True)) else {"TheTime":("TheTime","max")})})\
                             .groupby(period_key + "_iter",as_index = False).nth(-1)\
                             .set_index("TheTime")

    else:
        return df.loc[df[period_key] == True,[x for x in ["TheTime",key + "_iter",agg_col,period_key + "_iter"] if x != "_iter"]]\
                              .assign(group_time = df[[period_key + "_iter","TheTime"]].groupby(period_key + "_iter").transform(max))\
                              .loc[df[key] == True]\
                              .assign(min_time = df.loc[df[key] == True].groupby(key + "_iter",as_index = False)["TheTime"].transform(min))\
                              .pipe(lambda x:x.loc[x["TheTime"] > x["min_time"] + delay])\
                              .groupby(period_key + "_iter",as_index = False)\
                              .agg(**{stat_name + "_mean":(agg_col,"mean"),stat_name + "_sd":(agg_col,"std"),stat_name + "_count":(agg_col,"count"),"TheTime":("TheTime","max")})\
                              .set_index("TheTime")

def get_zero_df(df,delay,key):
    zero_df = df[["NOx_counts",key,key + "_iter","TheTime"]].loc[df[key] == True]\
           .assign(min_time = df.loc[df[key] == True].groupby(key + "_iter",as_index = False)["TheTime"].transform(min))\
           .pipe(lambda x:x.loc[x["TheTime"] > x["min_time"] + delay])
    zero_df["Hourly_mean"] = zero_df["NOx_counts"].groupby(pd.Grouper(freq = "1h")).transform("mean")
    zero_df["Zero_diff"] = zero_df["NOx_counts"] - zero_df["Hourly_mean"]
    return zero_df[["Hourly_mean","Zero_diff","NOx_counts"]]

def get_CE(cal_df):
    cal_df["NO_t"] = cal_df["NO_t_nox_mean"] - cal_df["Cal_zero_t_mean"]
    cal_df["NO_u"] = cal_df["NO_u_mean"] - cal_df["Cal_zero_u_mean"]
    cal_df["CE"] = ((cal_df["NO2_t_mean"]-cal_df["NO_t"])-(cal_df["NO2_u_mean"]-cal_df["NO_u"])) / (cal_df["NO_u_mean"]-cal_df["NO_t_nox_mean"])
    #cal_df["CE"] = 1-((cal_df["NO2_u_mean"]-cal_df["NO2_t_mean"]) / (cal_df["NO_u_mean"]-cal_df["NO_t_mean"]))
    #cal_df.loc[((cal_df["CE"] > 1)|(cal_df["CE"] < 0)),"CE"] = None   
    cal_df["CE_Uncertainty"] = (((((2*cal_df["NO_u_sd"])/(cal_df["NO_u_count"]**0.5))/cal_df["NO_u_mean"])**2)\
                                + ((((2*cal_df["NO2_u_sd"])/(cal_df["NO2_u_count"]**0.5))/cal_df["NO2_u_mean"])**2)\
                                + ((((2*cal_df["NO_t_sd"])/(cal_df["NO_t_count"]**0.5))/cal_df["NO_t_mean"])**2)\
                                + ((((2*cal_df["NO2_t_sd"])/(cal_df["NO2_t_count"]**0.5))/cal_df["NO2_t_mean"])**2)) ** 0.5
    cal_df["Drift_CE"] = cal_df["CE"].dropna().diff().abs()
    cal_df["Drift_CE_Uncertainty"] = cal_df["Drift_CE"] / (3 ** 0.5)
    cal_df["Drift_CE_Uncertainty_percentage"] = cal_df["Drift_CE_Uncertainty"] / cal_df["CE"]
    return cal_df

def spike_removal(df,spike_removal_list):
    for col_name in spike_removal_list:
        df[col_name + "_diff"] = df[col_name + "_mean"].dropna().diff().abs()
        df[col_name + "_percentage"] = (df[col_name + "_sd"]/df[col_name + "_mean"])*100
        if col_name != "Zero":
            df[col_name + "_spikes_removed"] = np.nan
            df.loc[df["Zero_indicator"].interpolate(method = "time") == 0, col_name + "_spikes_removed"] = df.loc[df["Zero_indicator"].interpolate(method = "time") == 0,col_name + "_mean"]
    return df

def do_cal_df_calcs(cal_df,calc_df):
    cal_df = cal_df.copy()
    cal_df["NO_cal_conc"] = 10111
    #cal_df["NO_cal_flow_mean"] = cal_df["NO_cal_flow_mean"].interpolate(method = "time")
    #cal_df["Cylinder_conc"] = 8200000
    #cal_df["NO_cal_conc"] = (cal_df["NO_cal_flow_mean"]/(cal_df["NO_cal_flow_mean"]+cal_df["NOx_sample_flow_mean"]))*cal_df["Cylinder_conc"]
    #cal_df["NO_cal_conc"] = cal_df["NO_cal_conc"].fillna(method = "ffill")
    cal_df["Responsivity"] = cal_df["Sens_cal_Hz"]/cal_df["NO_cal_conc"] #cps/ppt
    cal_df = cal_df.dropna(how = "any", subset = ["Responsivity"])
    cal_df["SENS"] = cal_df["NO_cal_conc"]/cal_df["Sens_cal_Hz"] #ppt/cps
    cal_df = cal_df.dropna(how = "any", subset = ["SENS"])
    cal_df["Uncertainty_SENS"] = (((2*cal_df["NO_u_sd"])/(cal_df["NO_u_count"]**0.5))/cal_df["NO_u_mean"])
    cal_df.loc[:,"Drift_SENS"] = cal_df["SENS"].diff().abs()
    cal_df["Drift_SENS_Uncertainty_cps/pptv"] = cal_df["Drift_SENS"] / (3 ** 0.5)
    cal_df["Drift_SENS_Uncertainty_percentage"] = cal_df["Drift_SENS_Uncertainty_cps/pptv"] / cal_df["SENS"]
    cal_df["Total_calibration_uncertainty_NO"] = ((0.01**2) + (0.01**2) + (cal_df["Uncertainty_SENS"]**2) + (cal_df["Drift_SENS_Uncertainty_percentage"]**2)) ** 0.5
    cal_df["Total_calibration_uncertainty_NO2"] = ((0.01**2) + (0.01**2) + (cal_df["Uncertainty_SENS"]**2) + (cal_df["Drift_SENS_Uncertainty_percentage"]**2) + (cal_df["CE_Uncertainty"]**2) + (cal_df["Drift_CE_Uncertainty_percentage"]**2)) ** 0.5
    cal_df["Previous_zero_cycle"] = calc_df["Zero_mean"].dropna().reindex(cal_df.index,method = "ffill")
    cal_df["Efficiency_zero_volume"] = 1 - ((cal_df["Cal_zero_mean"]-cal_df["Previous_zero_cycle"])/(cal_df["NO_u_mean"]-cal_df["Previous_NO_cycle"]))
    cal_df["NO2_content_cylinder_BLC_pptV"] = (cal_df["NO2_u_mean"] - cal_df["NO_u_mean"])/(cal_df["CE"]*cal_df["SENS"])
    cal_df["NO2_content_cylinder_BLC_percentage"] = cal_df["NO2_content_cylinder_BLC_pptV"]/(cal_df["NO2_content_cylinder_BLC_pptV"] + cal_df["NO_cal_conc"])
    return cal_df

def do_calc_df_calcs(calc_df):
    calc_df["NO_Conc"] = calc_df["NO_signal"] * calc_df["SENS"].interpolate(method = "time")
    calc_df["NO2_Conc"] = calc_df["NO2_signal"] * (calc_df["SENS"].interpolate(method = "time")*calc_df["CE"].interpolate(method = "time"))

    calc_df["Night_indicator"] = (calc_df.index.hour < 4)|(calc_df.index.hour > 21)
    calc_df ["Night_indicator_iter"] = (calc_df["Night_indicator"] != calc_df["Night_indicator"].shift()).cumsum()
    calc_df = calc_df.join(\
        calc_df.assign(new_time = calc_df.index.values)\
        .loc[calc_df["Night_indicator"] == True]\
        .groupby("Night_indicator_iter")\
        .agg(NO_night_mean = ("NO_Conc",np.mean),NO_night_sd = ("NO_Conc",np.std),NO_night_count = ("NO_Conc","count"),TheTime = ("new_time",max))\
        .set_index("TheTime"))
    calc_df["NO_night_uncertainty"] =((2*calc_df["NO_night_sd"])/(calc_df["NO_night_count"]**0.5))
    calc_df = calc_df.join(calc_df["NO_night_mean"].dropna().diff().rename("NO_night_diff_between_nights"),how = "outer")
    calc_df["NO_night_drift_uncertainty"] = calc_df["NO_night_diff_between_nights"].abs() / (3 ** 0.5)
    calc_df["NO_art_total_uncertainty"] = ((calc_df["NO_night_uncertainty"]**2) + (calc_df["NO_night_drift_uncertainty"]**2)) ** 0.5
    return calc_df

def do_art_df_calcs(art_df):
    art_df["PAG_Zero_NO_signal"] = art_df["PAG_Zero_NO_mean"] -art_df["Zero_artefact_mean"].interpolate(method = "time")
    art_df["PAG_Zero_NO2_signal"] = art_df["PAG_Zero_NO2_mean"]-art_df["PAG_Zero_NO_mean"].interpolate(method = "time")        
    
    art_df["PAG_Zero_NO_Conc"] = art_df["PAG_Zero_NO_signal"] * art_df["SENS"].interpolate(method = "time")
    art_df["PAG_Zero_NO_Conc"] = art_df[["PAG_Zero_NO_Conc","NOx_za_k_iter"]].dropna(subset = ["PAG_Zero_NO_Conc"]).groupby("NOx_za_k_iter",as_index = False).nth[-3:]["PAG_Zero_NO_Conc"]
    art_df["PAG_Zero_NO_Conc_mean"] = art_df["PAG_Zero_NO_Conc"].groupby(art_df["NOx_za_k_iter"]).transform("mean").drop_duplicates(keep = "last")
    art_df["PAG_Zero_NO2_Conc"] = art_df["PAG_Zero_NO2_signal"] * (art_df["SENS"].interpolate(method = "time")*art_df["CE"].interpolate(method = "time"))
    art_df["PAG_Zero_NO2_Conc_corr"] = art_df["PAG_Zero_NO2_Conc"]
    art_df.loc[art_df["PAG_Zero_NO2_Conc_corr"] < 0,"PAG_Zero_NO2_Conc_corr"] = 0
    art_df["PAG_Zero_NO2_Conc_corr"] = art_df[["PAG_Zero_NO2_Conc_corr","NOx_za_k_iter"]].dropna(subset = ["PAG_Zero_NO2_Conc_corr"]).groupby("NOx_za_k_iter",as_index = False).nth[-3:]["PAG_Zero_NO2_Conc_corr"]
    
    grouper = art_df[["PAG_Zero_NO2_Conc_corr","NOx_za_k_iter"]].groupby("NOx_za_k_iter")
    art_df = art_df.assign(PAG_Zero_NO2_Conc_mean=grouper["PAG_Zero_NO2_Conc_corr"].transform("mean").drop_duplicates(keep = "last"),
                           PAG_Zero_NO2_Conc_std=grouper["PAG_Zero_NO2_Conc_corr"].transform("std").drop_duplicates(keep = "last"),
                           PAG_Zero_NO2_Conc_min=grouper["PAG_Zero_NO2_Conc_corr"].transform("min").drop_duplicates(keep = "last"),
                           PAG_Zero_NO2_Conc_max=grouper["PAG_Zero_NO2_Conc_corr"].transform("max").drop_duplicates(keep = "last"),
                           PAG_Zero_NO2_Conc_range=grouper["PAG_Zero_NO2_Conc_corr"].transform(lambda x:x.max() - x.min()).drop_duplicates(keep = "last"))

    #The range of measurements is used to calculate the uncertainty of the NO2 artefact
    art_df["PAG_Zero_NO2_Conc_uncertainty"] = art_df["PAG_Zero_NO2_Conc_range"] / (3**0.5)
    art_df["PAG_Zero_NO2_Conc_diff"] = art_df["PAG_Zero_NO2_Conc_mean"].dropna().diff()
    
    return art_df

def run_me(curr_folder,data_folder,output_folder,year):
    NOx_delay = 18/86400
    NO_delay = 18/86400
    Zero_delay = 18/86400
    key_cols = ["NOx_zero_k","NOx_cal_k","calMFC_k","GPTLamp_k","NOx_za_k","NO2_converter","Inlet_SS"]
    
    initial_stats = [("Zero","Zero",NO_delay,"NOx_counts"),
                     ("Zero_NO2","Zero_NO2",NO_delay,"NOx_counts"),
                     ("NO_Hz","NO_Hz",NO_delay,"NOx_counts"),
                     ("NOx_Hz","NOx_Hz",NOx_delay,"NOx_counts"),
                     ("calMFC_k","NO_cal_flow",NOx_delay,"NO_Cal_flow"),
                     ("calMFC_k","NOx_sample_flow",NOx_delay,"Inlet_SampleFlow_1")]
    
    cal_stats = [("Cal_zero",Zero_delay,"NOx_counts","NOx_cal_k",False),
                 ("Cal_zero_NO2",Zero_delay,"NOx_counts","NOx_cal_k",False),
                 ("Cal_zero_u",Zero_delay,"NOx_counts","NOx_cal_k",True),
                 ("Cal_zero_t",Zero_delay,"NOx_counts","NOx_cal_k",True),
                 ("NO_u",NO_delay,"NOx_counts","NOx_cal_k",True),
                 ("NO2_u",NOx_delay,"NOx_counts","NOx_cal_k",True),
                 ("NO_u_nox",NOx_delay,"NOx_counts","NOx_cal_k",True),
                 ("NO2_t",NOx_delay,"NOx_counts","NOx_cal_k",True),
                 ("NO_t",NO_delay,"NOx_counts","NOx_cal_k",True),
                 ("NO_t_nox",NOx_delay,"NOx_counts","NOx_cal_k",True)]
    
    artifact_stats = [("Zero_artefact",NO_delay),
                      ("Zero_artefact_NO2",NO_delay),
                      ("PAG_Zero_NO",NO_delay),
                      ("PAG_Zero_NO2",NOx_delay)]
    
    spike_removal_list = ["Zero","NO_Hz","NOx_Hz"]
    
    
    dates_to_remove = pd.read_csv(os.path.join(curr_folder,"config","remove_date_times.mww"),delimiter = "\t",usecols = ["Start Date","End Date"],parse_dates = ["Start Date","End Date"])
    flist = glob.glob(os.path.join(data_folder,"*"))
    #flist = filter_flist(flist,year)
    with WorkerPool(n_jobs = 4) as wp:
        df = pd.concat(wp.map(read_file,flist)).sort_index()
    
    df["Hour_since_1970"] = df.index.values
    df.index = pd.DatetimeIndex(pd.to_datetime(df.index /  24, unit='D', origin='unix'))
    df = df.loc[df.index.notnull()]
    df = clean_up_df(df)
    df.loc[df["NOx_counts"] < 0,"NOx_counts"] = np.nan
    df = fix_key_cols(df,key_cols)
    df = create_new_keys(df)

    df = drop_dates(df,dates_to_remove)

    df = df.set_index("TheTime",drop = False)
    df.index = pd.DatetimeIndex(pd.to_datetime(df.index /  24, unit='D', origin='unix'))
    calc_df = pd.concat([pd_stats(df,key,stat_name,delay,agg_col) for key,stat_name,delay,agg_col in initial_stats],axis = 1).sort_index()
    calc_df.index = pd.DatetimeIndex(pd.to_datetime(calc_df.index /  24, unit='D', origin='unix'))
    
    cal_df = pd.concat([pd_stats(df,key,key,delay,agg_col,period,last_in_period) for key,delay,agg_col,period,last_in_period in cal_stats],axis = 1).sort_index()
    #cal_df = pd.concat([pd_stats(df,key,stat_name,delay,agg_col) for key,stat_name,delay,agg_col in cal_stats],axis = 1).sort_index()
    cal_df.index = pd.DatetimeIndex(pd.to_datetime(cal_df.index /  24, unit='D', origin='unix'))

    art_df = pd.concat([pd_stats(df,key,key,delay) for key,delay in artifact_stats],axis = 1).sort_index()
    art_df.index = pd.DatetimeIndex(pd.to_datetime(art_df.index /  24, unit='D', origin='unix'))
    art_df = art_df.join(df["NOx_za_k_iter"])
    
    zero_df = get_zero_df(df,NO_delay,"Zero_Hz")
    
    cal_df = get_CE(cal_df)
    
    calc_df = remove_spikes.remove_spikes(calc_df,"Zero_mean","Zero_spikes_removed","Zero_indicator")
    calc_df = spike_removal(calc_df,spike_removal_list)
    calc_df["NO_signal"] = calc_df["NO_Hz_spikes_removed"] - calc_df["Zero_spikes_removed"].interpolate(method = "time")
    calc_df["NO2_signal"] = calc_df["NOx_Hz_spikes_removed"] - calc_df["NO_Hz_spikes_removed"].interpolate(method = "time")
    
    #get the previous NO cycle from the calculation dataframe for use in the calibration dataframe - this is what we do at CVAO
    #at KCG sens is calculated by subtracting zero cal NO counts from NO cal counts
    cal_df["Previous_NO_cycle"] = calc_df["NO_Hz_mean"].dropna().reindex(cal_df.index,method = "ffill")
    #cal_df["Sens_cal_Hz"] = cal_df["NO_u_mean"] - cal_df["Previous_NO_cycle"]
    cal_df["Sens_cal_Hz"] = cal_df["NO_u_mean"] - cal_df["Cal_zero_mean"].interpolate(method = "time")
    #join the calibration and sample flows to the calibration dataframe

    cal_df = cal_df.join(calc_df[["NO_cal_flow_mean","NOx_sample_flow_mean"]],how = "outer")

    #carry out the calculations necessary on the calibration dataframe

    cal_df = do_cal_df_calcs(cal_df,calc_df)

    #join the calibration factors to the calculation dataframe and then apply them to give concentrations
    calc_df = calc_df.join(cal_df[["SENS","CE"]],how = "outer")
    calc_df = do_calc_df_calcs(calc_df)
    
    art_df = art_df.join(cal_df[["SENS","CE"]],how = "outer")
    art_df = do_art_df_calcs(art_df)
    calc_df = calc_df.join(art_df[["PAG_Zero_NO2_Conc_mean","PAG_Zero_NO_Conc_mean"]].dropna(),how = "outer")
    calc_df["NO_Conc_art_corrected_night"] = calc_df["NO_Conc"] - calc_df["NO_night_mean"].interpolate(method = "time")
    calc_df["NO_Conc_art_corrected_PAG"] = calc_df["NO_Conc"] - calc_df["PAG_Zero_NO_Conc_mean"].interpolate(method = "time")
    calc_df["NO2_Conc_art_corrected"] = calc_df["NO2_Conc"] - calc_df["PAG_Zero_NO2_Conc_mean"].interpolate(method = "time")
    
    calc_df.to_csv(output_folder + os.path.sep + "NOx_kcg_" + str(year) + "_calc_df.csv")
    cal_df.to_csv(output_folder + os.path.sep + "NOx_kcg_" + str(year) + "_cal_df.csv")
    art_df.to_csv(output_folder + os.path.sep + "NOx_kcg_" + str(year) + "_art_df.csv")



if __name__ == "__main__":
    curr_folder = os.path.dirname(os.path.realpath(sys.argv[0]))
    data_folder = os.path.join(curr_folder,"nox_raw_data/oct_to_dec_22")
    output_folder = os.path.join(curr_folder,"processed_data/oct_to_dec")
    #years = [2022]
    run_me(curr_folder,data_folder,output_folder,2022)
    
    #for year in years:
     #   print("Calculating for year",year)
      #  run_me(curr_folder,data_folder,output_folder,year)
    #run_me(curr_folder)
