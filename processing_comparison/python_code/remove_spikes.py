import pandas as pd
from scipy import signal


def remove_spikes(calc_df,column_to_despike,output_column_name,indicator_column_name,filter_width=25,diff_to_median = 100):
    indices = calc_df.dropna(subset = column_to_despike).index.values
    vals = signal.medfilt(calc_df[column_to_despike].dropna().values, filter_width)
    calc_df = pd.concat([calc_df,pd.DataFrame(data = vals,index = indices,columns = [output_column_name])],axis = 1)
    
    calc_df.loc[abs(calc_df[column_to_despike] - calc_df[output_column_name]) < diff_to_median,output_column_name] = calc_df.loc[abs(calc_df[column_to_despike] - calc_df[output_column_name]) < diff_to_median,column_to_despike]
    calc_df.loc[abs(calc_df[column_to_despike] - calc_df[output_column_name]) < diff_to_median,indicator_column_name] = 0
    calc_df.loc[abs(calc_df[column_to_despike] - calc_df[output_column_name]) >= diff_to_median,indicator_column_name] = 1
    return calc_df
