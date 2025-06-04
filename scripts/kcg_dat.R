library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(openair)
library(ggh4x)
library(ggpubr)
library(plotly)

Sys.setenv(TZ='UTC')

#definitive KCG code for plotting and analysis
#for code comparing KCG and CVAO processing, look at work_done_at_csiro or thesis plots for chapter 5

# Functions ---------------------------------------------------------------

tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}
ppt_to_molecules_cm3 <- function(x){y = x * 2.46 * 10^19 * 10^-12}
molecules_cm3_to_ppt <- function(x){y = x / ( 2.46 * 10^19 * 10^-12)}

# Reading in data ---------------------------------------------------------

#met contains jno2, which is the same as jno2 in jno2 dataset
met_kcg = read.csv("data/kcg_data/KCG_LR_Supp_Data_2022_240506.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date))

#rainfall measured at 09:00am for the previous 24 hrs
rainfall_kcg = read.csv("data/kcg_data/kcg_rainfall.csv") %>% 
  clean_names() %>% 
  remove_constant() %>% 
  mutate(date = as.POSIXct(paste(2022,month,day,09,00),
                           format = "%Y %m %d %H %M")) %>% 
  select(date, rainfall_mm = rainfall_amount_millimetres,quality) %>% 
  timeAverage("1 hour") %>% 
  fill(rainfall_mm,.direction = "up")

radon_kcg = read.csv("data/kcg_data/CG_radon_2022_startofhour.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date),
         radon = ifelse(radon < 0,NA_real_,radon)) %>% 
  select(date,radon)

cn_kcg = read.table("data/kcg_data/condensation_nuclei.nas",skip = 84,header = TRUE) %>% 
  mutate(date = as.POSIXct(round(starttime / 0.041667) *3600,
                           origin = "2022-01-01 00:00:00"),
         ccnc = ifelse(flag == 0,ccnc,NA_real_)) %>% 
  select(date,ccnc)

#bl_flag is the baseline flag, 0 is baseline, 1 is non-baseline and 9 is baseline not determined
co_kcg = read.table("data/kcg_data/cgo_picarro_1_60min_co_all_blflagged.dat",skip = 6,header = TRUE) %>% 
  clean_names() %>% 
  mutate(date = paste(yyyy,mm,dd),
         date = gsub(" ","-",date),
         date = paste(date,hh),
         date = ymd_h(date)) %>% 
  select(date,co_ppb:bl_flag) %>% 
  filter(date >= "2022-01-01" & date < "2023-01-01") %>% 
  rename_with(.fn = function(.x){paste0("co_",.x)},
              .cols = c(sd_ppb:bl_flag))

ch4_kcg = read.table("data/kcg_data/cgo_picarro_1_60min_ch4_all_blflagged.dat",skip = 6,header = TRUE) %>% 
  clean_names() %>% 
  mutate(date = paste(yyyy,mm,dd),
         date = gsub(" ","-",date),
         date = paste(date,hh),
         date = ymd_h(date)) %>% 
  select(date,ch4_ppb:bl_flag) %>% 
  filter(date >= "2022-01-01" & date < "2023-01-01")%>% 
  rename_with(.fn = function(.x){paste0("ch4_",.x)},
              .cols = c(sd_ppb:bl_flag))

co2_kcg = read.table("data/kcg_data/cgo_picarro_1_60min_ch4_co2_2022.txt",skip = 2) %>% 
  mutate(date = glue::glue("{V2}-{V3}-{V4} {V5}:{V6}"),
         date = ymd_hm(date),
         co2_dry = V23) %>% 
  select(date,co2_dry) %>% 
  timeAverage("1 hour")


#uncorrected NO and NO2
nox_kcg_uncorr = read.csv("data/kcg_data/Full_Ambient data_NOx_2405071232.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date_time)) %>% 
  select(date,no_ppt = no,no2_ppt = no2,nox_ppt = n_ox)

#for no two different calculations to determine inlet losses (ozone correction)
#no_ppt1 refers to data corrected in the same way as York data
#no_ppt2 refers to data corrected by doubling it (using NO cal gas - not fully sure how this is done yet)
#no offset corrections still off
#no2 offset corrections done with monthly minima (Ian happy with these)
nox_kcg_corr = read.csv("data/kcg_data/2022KCGAmbient_NO2&NO_V9_York_2411051919.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(timestamp)) %>% 
  select(date,o3_ppb = o3,no_ppt_corr1 = no_corr_il1,no_ppt_corr2 = no_corr_il2,no2_ppt_corr = no2ilc)

df_list = list(nox_kcg_uncorr,nox_kcg_corr,met_kcg,rainfall_kcg,radon_kcg,ch4_kcg,co_kcg,co2_kcg,cn_kcg)

kcg_dat = df_list %>% reduce(left_join,by = "date") %>% 
  mutate(jno2_albedo = (1 + 0.07) * jno2_mean,
         temp_k = mean_dbt + 273.15,
         k = 1.4 * 10^-12 * exp(-1310/temp_k),
         month = month(date),
         wd = ifelse(scalar_mean_wd10 < 0, NA_real_,scalar_mean_wd10),
         ws = ifelse(scalar_mean_ws10 < 0, NA_real_,scalar_mean_ws10))

# remove(df_list,radon_kcg,met_kcg,ch4_kcg,co_kcg,cn_kcg,co2_kcg,rainfall_kcg)

# Baseline flags ---------------------------------------------------------

#can change the amount of data to remove based on baseline definition

kcg_dat_bl = kcg_dat %>% 
  #baseline definitions from Ian and Suzie's work
  mutate(wind_bl_flag = ifelse(between(wd,190,280) & ws >= 20,0,1),
         ccnc_bl_flag = case_when(ccnc <= 855 & month == 1 ~ 0,
                                  ccnc <= 820 & month == 2 ~ 0,
                                  ccnc <= 745 & month == 3 ~ 0,
                                  ccnc <= 560 & month == 4 ~ 0,
                                  ccnc <= 310 & month == 5 ~ 0,
                                  ccnc <= 265 & month == 6 ~ 0,
                                  ccnc <= 215 & month == 7 ~ 0,
                                  ccnc <= 305 & month == 8 ~ 0,
                                  ccnc <= 465 & month == 9 ~ 0,
                                  ccnc <= 635 & month == 10 ~ 0,
                                  ccnc <= 850 & month == 11 ~ 0,
                                  ccnc <= 820 & month == 12 ~ 0,
                                  TRUE ~ 1),
         radon_bl_flag = ifelse(radon <= 100,0,1),
         #most stringent baseline definition requiring all three flags to be baseline
         bl_flag = case_when(ccnc_bl_flag == 0 & wind_bl_flag == 0 & radon_bl_flag == 0 ~ "Baseline",
                             TRUE ~ "Not baseline"),
         #extra baseline based on joint flags for CO, WS and radon
         joint_flag = case_when(co_ppb > 60 & ws < 45 & radon > 60 & bl_flag == "Baseline" ~ "Joint flag",
                                TRUE ~ "Not joint flag"),
         no_ppt_bl = case_when(is.na(no_ppt_corr1) == T ~ NA_real_, #to remove problematic data that Ian has filtered
                               radon > 100 ~ NA_real_, #general radon baseline
                               # bl_flag == "Not baseline" ~ NA_real_,
                               # joint_flag == "Joint flag" ~ NA_real_,
                               #other data filtered based on dates with problems
                               date >= "2022-02-01" & date < "2022-02-03" ~ NA_real_,
                               date >= "2022-03-28" & date < "2022-03-30" ~ NA_real_,
                               date >= "2022-04-28" & date < "2022-04-30" ~ NA_real_,
                               date >= "2022-05-13" & date < "2022-05-15" ~ NA_real_,#diesel generator,low confidence cal
                               date >= "2022-05-30" & date < "2022-06-07" ~ NA_real_,
                               date >= "2022-07-19" & date < "2022-07-26" ~ NA_real_,#unknown
                               date >= "2022-08-10" & date < "2022-08-11" ~ NA_real_,#inlet blockage
                               date >= "2022-09-24" & date < "2022-09-25" ~ NA_real_,#inlet blockage
                               date >= "2022-09-27" & date < "2022-09-28" ~ NA_real_,#inlet blockage
                               date >= "2022-11-16" & date < "2022-11-30" ~ NA_real_,
                               TRUE ~ no_ppt),
         no_ppt_bl_all = case_when(bl_flag == "Not baseline" ~ NA_real_,
                                   TRUE ~ no_ppt_bl),
         no2_ppt_bl = case_when(# bl_flag == "Not baseline" ~ NA_real_,
                                # joint_flag == "Joint flag" ~ NA_real_,
                                radon > 100 ~ NA_real_,
                                date >= "2022-02-01" & date < "2022-02-03" ~ NA_real_,
                                date >= "2022-03-28" & date < "2022-03-30" ~ NA_real_,
                                date >= "2022-04-28" & date < "2022-04-30" ~ NA_real_,
                                date >= "2022-05-13" & date < "2022-05-15" ~ NA_real_,#diesel generator,low confidence cal
                                date >= "2022-05-30" & date < "2022-06-07" ~ NA_real_,
                                date >= "2022-07-19" & date < "2022-07-26" ~ NA_real_,#unknown
                                date >= "2022-08-10" & date < "2022-08-11" ~ NA_real_,#inlet blockage
                                date >= "2022-09-24" & date < "2022-09-25" ~ NA_real_,#inlet blockage
                                date >= "2022-09-27" & date < "2022-09-28" ~ NA_real_,#inlet blockage
                                date >= "2022-11-16" & date < "2022-11-30" ~ NA_real_,
                                TRUE ~ no2_ppt_corr))

#looking for relationships between baseline NO and other factors
kcg_dat_bl %>% 
  mutate(hour = hour(date)) %>% 
  filter(is.na(no_ppt_corr1) == F,
         radon <= 100,
         # bl_flag == "Baseline",
         hour >= 22 | hour <= 4
         ) %>%
  rename(`RH (%)` = mean_rh,
         `Rainfall (mm)` = rainfall_mm,
         `Radon (mBq/m3)` = radon,
         `Methane (ppb)` = ch4_ppb,
         `Carbon monoxide (ppb)` = co_ppb,
         `CCN (counts/m3)` = ccnc,
         WD = wd,
         `WS (km/h)` = ws,
         `Full baseline` = no_ppt_bl_all,
         `Radon baseline` = no_ppt_bl) %>% 
  pivot_longer(c(`Radon baseline`,`Full baseline`),names_to = "no_names",values_to = "no_values") %>% 
  pivot_longer(c(`WS (km/h)`,WD,`Rainfall (mm)`,`Carbon monoxide (ppb)`,`Radon (mBq/m3)`,`CCN (counts/m3)`)) %>%
  ggplot(aes(no_values,value)) +
  geom_point() +
  facet_nested_wrap(vars(name,no_names),
                    scales = "free",
                    ncol = 6,
                    axes = "margins") +
  labs(x = "Baseline nighttime NO (ppt)",
       y = NULL,
       col = "Baseline definition") +
  theme(legend.position = "top") +
  NULL

ggsave("nighttime_baseline_definition_correlation.png",
       path = "output/paper_plots",
       height = 12,
       width = 29,
       units = "cm")

# Calculating nighttime NO offset -----------------------------------------

night_zeroing = kcg_dat_bl %>% 
  mutate(hour = hour(date),
         night_flag = ifelse(hour >= 22 | hour < 4,1,0))

nights = rle(night_zeroing$night_flag) %>%
  tidy_rle() %>% 
  filter(values == 1) %>% 
  mutate(id = 1:nrow(.)) %>% 
  as.list() %>% 
  purrr::pmap_df(~data.frame(idx = ..3:..4,id = ..5)) %>% #df with row numbers and corresponding groups
  tibble() 

night_flagged = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(nights, "idx") %>% #joins two dfs by their row number
  mutate(id = ifelse(is.na(id), 0, id)) #makes id (group) = 0 during the day

#NO data already filtered for baseline, extra filter here to get accurate extra measurements for later comparisons
night_avg = night_flagged %>% 
  filter(id != 0,
         radon <= 100
         # bl_flag == "Baseline",
         # joint_flag == "Not joint flag"
         ) %>% 
  group_by(id) %>% 
  summarise(no_night_bl = mean(no_ppt_bl,na.rm = T),
            no_night_bl_all = mean(no_ppt_bl_all,na.rm = T),
            co_night = mean(co_ppb,na.rm = T),
            ccnc_night = mean(ccnc,na.rm = T),
            radon_night = mean(radon,na.rm = T),
            rainfall_mm_night = mean(rainfall_mm,na.rm = T),
            no_night_count = sum(!is.na(no_ppt_bl)),
            idx = median(idx)) %>% 
  mutate(idx = floor(idx)) %>% 
  ungroup()

night_zeroed = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_avg) %>% 
  mutate(no_night_inter_bl = na.approx(no_night_bl,na.rm = F),
         no_night_inter_bl_all = na.approx(no_night_bl_all,na.rm = F)) %>% 
  fill(no_night_inter_bl_all,no_night_inter_bl,.direction = "up") %>% 
  mutate(no_night_corr_bl = no_ppt_bl - no_night_inter_bl,
         no_night_corr_bl_all = no_ppt_bl_all - no_night_inter_bl_all) 
  # select(date:no2_ppt_corr,no_ppt_bl,no2_ppt_bl,no_night,no_night_inter,no_night_corrected,bl_flag,wd,ws,mean_rh,jno2_mean,rainfall_mm:ch4_ppb,co_ppb,co2_dry,ccnc,jno2_albedo,temp_k)

#remove(night_avg,night_flagged,night_zeroing,nights)

# NO nighttime offset plotting --------------------------------------------

#seeing if nighttime NO correlates with anything
night_zeroed %>% 
  filter(is.na(no_night_bl) == F) %>%
  rename(`RH (%)` = mean_rh,
         `Rainfall (mm)` = rainfall_mm_night,
         `Radon (mBq/m3)` = radon_night,
         `Methane (ppb)` = ch4_ppb,
         `Carbon monoxide (ppb)` = co_night,
         `CCN (counts/m3)` = ccnc_night,
         WD = wd,
         `WS (km/h)` = ws,
         `Full baseline` = no_night_bl_all,
         `Radon baseline` = no_night_bl) %>% 
  pivot_longer(c(`Radon baseline`,`Full baseline`),names_to = "no_names",values_to = "no_values") %>% 
  pivot_longer(c(`WS (km/h)`,WD,`Rainfall (mm)`,`Carbon monoxide (ppb)`,`Radon (mBq/m3)`,`CCN (counts/m3)`)) %>%
  ggplot(aes(no_values,value)) +
  geom_point() +
  facet_nested_wrap(vars(name,no_names),
                    scales = "free",
                    ncol = 6,
                    axes = "margins") +
  labs(x = "Baseline nighttime NO (ppt)",
       y = NULL,
       col = "Baseline definition") +
  theme(legend.position = "top") +
  NULL

# ggsave("nighttime_no_baseline.png",
#        path = "output/paper_plots",
#        height = 13,
#        width = 20,
#        units = "cm")

#comparing uncorrected and nighttime corrected NO
night_zeroed %>% 
  filter(no_ppt < 200) %>%
  rename(`Full baseline` = no_night_corr_bl_all,
         `Radon baseline` = no_night_corr_bl) %>%
  pivot_longer(c(`Full baseline`,`Radon baseline`),names_to = "corr_names",values_to = "corr_values") %>%
  pivot_longer(c(no_night_bl_all,no_night_bl),names_to = "night_names",values_to = "night_values") %>% 
  pivot_longer(c(no_ppt_bl,no_ppt_bl_all),names_to = "uncorr_names",values_to = "uncorr_values") %>% 
  pivot_longer(c(no_night_inter_bl,no_night_inter_bl_all),names_to = "inter_names",values_to = "inter_values") %>% 
  mutate(flag = case_when(corr_names == "Radon baseline" & night_names == "no_night_bl" & uncorr_names == "no_ppt_bl" & inter_names == "no_night_inter_bl" ~ "bl",
                          corr_names == "Full baseline" & night_names == "no_night_bl_all" & uncorr_names == "no_ppt_bl_all" & inter_names == "no_night_inter_bl_all" ~ "all")) %>% 
  filter(is.na(flag) == F) %>% 
  ggplot() +
  theme_bw() +
  geom_point(aes(date,corr_values,col = "Corrected")) +
  geom_point(aes(date,uncorr_values,col = "Uncorrected")) +
  # geom_point(aes(date,night_values),col = "black") +
  geom_path(aes(date,inter_values)) +
  facet_grid(rows = vars(corr_names),scales = "free") +
  # geom_path(aes(date,no_night_inter)) +
  labs(x = NULL,
       y = "NO (ppt)",
       col = NULL) +
  theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %Y")

ggsave("corr_vs_uncorr_radon_baseline_definitions.png",
       path = "output/paper_plots",
       height = 12,
       width = 29,
       units = "cm")

#diurnals
diurnal_dat = night_zeroed %>% 
  # rename(no_uncorr = no_ppt_bl,
  #        no_corr = no_night_corrected) %>% 
  mutate(hour = hour(date)) %>% 
  group_by(hour) %>% 
  summarise(across(c(no_ppt_bl,no_ppt_bl_all,no_night_corr_bl,no_night_corr_bl_all),
                   list(mean = ~mean(.,na.rm = T),
                        count = ~sum(!is.na(.))))) %>% 
  ungroup()

diurnal_dat %>% 
  rename(`Full baseline` = no_night_corr_bl_all_mean,
         `Radon baseline` = no_night_corr_bl_mean) %>% 
  pivot_longer(c(no_ppt_bl_mean,no_ppt_bl_all_mean),names_to = "names_uncorr",values_to = "values_uncorr") %>%  
  pivot_longer(c(`Full baseline`,`Radon baseline`),names_to = "names_corr",values_to = "values_corr") %>%  
  mutate(flag = case_when(names_uncorr == "no_ppt_bl_mean" & names_corr == "Radon baseline" ~ "bl",
                          names_uncorr == "no_ppt_bl_all_mean" & names_corr == "Full baseline" ~ "all")) %>% 
  filter(!is.na(flag)) %>% 
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,values_corr,col = "Corrected"),size = 1) +
  geom_path(aes(hour,values_uncorr,col = "Uncorrected"),size = 1) +
  facet_wrap(~names_corr) +
  labs(x = "Hour of day",
       y = "NO (ppt)",
       col = NULL,
       fill = NULL) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  NULL

ggsave("corr_vs_uncorr_diurnal_baseline_def.png",
       path = "output/paper_plots",
       height = 12,
       width = 29,
       units = "cm")

# Nighttime NO and minima NO2 ---------------------------------------------

#checking the relationship between NO and NO2 under baseline conditions
test = night_zeroed %>% 
  rename(no_uncorr = no_ppt_bl) %>%
  mutate(flag_nox = ifelse(no_night_corrected > no2_ppt_bl,"NO > NO2","NO2> NO"))

percentages = test %>%
  filter(is.na(flag_nox) == F) %>% 
  count(flag_nox) %>%
  mutate(percent = (n / sum(n)) * 100)

test %>% 
  filter(is.na(flag_nox) == F) %>% 
  pivot_longer(c(no_night_corrected,no2_ppt_corr,rainfall_mm)) %>% 
  ggplot(aes(date,value,col = flag_nox)) +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free") +
  labs(col = NULL) +
  theme(legend.position = "top")

# ggsave("baseline_corr_no_no2.png",
#        path = "output/paper_plots",
#        height = 10,
#        width = 29,
#        units = "cm")

#diurnals
night_zeroed %>% 
  mutate(baseline = ifelse(radon <= 100,"Baseline","Not baseline"),
         no_night_corrected = case_when(date >= "2022-02-01" & date < "2022-02-03" ~ NA_real_,
                                        date >= "2022-03-28" & date < "2022-03-30" ~ NA_real_,
                                        date >= "2022-04-28" & date < "2022-04-30" ~ NA_real_,
                                        date >= "2022-05-13" & date < "2022-05-15" ~ NA_real_,#diesel generator,low confidence cal
                                        date >= "2022-05-30" & date < "2022-06-07" ~ NA_real_,
                                        date >= "2022-07-19" & date < "2022-07-26" ~ NA_real_,#unknown
                                        date >= "2022-08-10" & date < "2022-08-11" ~ NA_real_,#inlet blockage
                                        date >= "2022-09-24" & date < "2022-09-25" ~ NA_real_,#inlet blockage
                                        date >= "2022-09-27" & date < "2022-09-28" ~ NA_real_,#inlet blockage
                                        date >= "2022-11-16" & date < "2022-11-30" ~ NA_real_,
                                        # no_night_corrected > 100 ~ NA_real_,
                                        TRUE ~ no_night_corrected),
         hour = hour(date)) %>% 
  filter(radon <= 100) %>% 
  group_by(hour) %>% 
  summarise(NO = mean(no_night_corrected,na.rm = T),
            `NO[2]` = mean(no2_ppt_corr,na.rm = T)) %>% 
  pivot_longer(c(NO,`NO[2]`)) %>% 
  ggplot(aes(hour,value,col = name)) +
  geom_path(linewidth = 1) +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  theme_bw() +
  labs(x = "Hour of day",
       y = expression(NO[x]~mixing~ratio~(ppt)),
       col = NULL) +
  theme(legend.position = "None")

# ggsave("nox_2022_baseline_diurnals.png",
#        path = "output/paper_plots",
#        height = 14,
#        width = 28,
#        units = "cm")

# KCG PSS -----------------------------------------------------------------

kcg_pss = test %>% 
  mutate(o3_molecule_cm3 = ppt_to_molecules_cm3(o3_ppb * 1000),
         hour = hour(date),
         no_molecule_cm3_uncorr = ppt_to_molecules_cm3(no_ppt),
         no_molecule_cm3_corr = ppt_to_molecules_cm3(no_ppt_corr1),
         no_molecule_cm3_night_corr = ppt_to_molecules_cm3(no_night_corrected)) %>% 
  filter(hour >= 10 & hour <= 14,
         radon < 100) %>%
  mutate(no2_pss_uncorr = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3_uncorr*k)/jno2_albedo),
         no2_pss_corr = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3_corr*k)/jno2_albedo),
         no2_pss_night_corr = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3_night_corr*k)/jno2_albedo))

kcg_pss %>% 
  # filter(no2_ppt_corr>no_night_corrected) %>% 
  # pivot_longer(c(no2_pss_corr,no2_pss_night_corr,no2_pss_uncorr)) %>%
  ggplot(aes(no2_ppt_corr,no2_pss_night_corr)) +
  theme_bw() +
  geom_point() +
  # stat_poly_line(col = "steelblue1") +
  geom_abline(slope = 1,intercept = 0,col = "darkorange",size = 1,linetype = "dashed") +
  # stat_poly_eq(use_label(c("eq"))) +
  labs(x = expression(Uncorrected~NO[2]~(ppt)),
       y = expression(PSS~NO[2]~(ppt)),
       col = NULL) +
  theme(legend.position = "top") +
  ylim(0,125) +
  xlim(0,125) +
  NULL

ggsave("uncorrected_no2_pss.png",
       path = "output/paper_plots",
       height = 12,
       width = 30,
       units = "cm")

# Bodged ozone correction testing? ----------------------------------------

tc = 1.32 #time in converter 1s
t = 1.1 #time in sample line (between inlet and reaction cell) 4.3s
k = 1.8*10^-14 #rate constant for NO + O3 -> NO2 + O2

night_zeroed_ozone = night_zeroed %>% 
  mutate(ko3 = (1.8*10^-14)*o3_ppb*(10^-9)*(2.48*10^19), 
         no_corr = no_night_corrected * exp(ko3 * t))
