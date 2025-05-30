library(tidyverse)
library(janitor)
library(openair)
library(zoo)
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

radon_kcg = read.csv("data/kcg_data/CG_radon_2022_startofhour.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date),
         radon = ifelse(radon < 0,NA_real_,radon)) %>% 
  select(date,radon)

cn_kcg = read.table("data/kcg_data/condensation_nuclei.nas",skip = 84,header = TRUE) %>% 
  mutate(date = as.POSIXct(round(starttime / 0.041667) *3600,
                           origin = "2022-01-01 00:00:00"),
         cloud_condensation_nuclei_number_concentration = ifelse(flag == 0,ccnc,NA_real_)) %>% 
  select(date,cloud_condensation_nuclei_number_concentration)

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

df_list = list(nox_kcg_uncorr,nox_kcg_corr,nox_kcg_cvao_code,met_kcg,radon_kcg,ch4_kcg,co_kcg,co2_kcg,cn_kcg)

kcg_dat = df_list %>% reduce(left_join,by = "date") %>% 
  mutate(radon_flag = ifelse(radon <= 100,"Baseline","Not baseline"),
         jno2_albedo = (1 + 0.07) * jno2_mean,
         temp_k = mean_dbt + 273.15,
         k = 1.4 * 10^-12 * exp(-1310/temp_k))

remove(df_list,radon_kcg,met_kcg,ch4_kcg,co_kcg,cn_kcg,co2_kcg)

# Calculating nighttime NO offset -----------------------------------------

night_zeroing = kcg_dat %>% 
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

night_avg = night_flagged %>% 
  filter(id != 0) %>% 
  group_by(id) %>% 
  summarise(no_night = mean(no_ppt,na.rm = T),
            no_night_cvao = mean(no_conc_cvao,na.rm = T),
            idx = min(idx)) %>% 
  mutate(idx = floor(idx)) %>% 
  ungroup()

night_zeroed = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_avg) %>% 
  mutate(no_night = case_when(radon > 100 ~ NA_real_,
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
                              TRUE ~ no_night),
         no_night_cvao = case_when(radon > 100 ~ NA_real_,
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
                              TRUE ~ no_night_cvao),
         no_night_inter = na.approx(no_night,na.rm = F),
         no_night_inter_cvao = na.approx(no_night_cvao,na.rm = F),
         no_night_corrected = no_ppt - no_night_inter,
         no_night_corrected_cvao = no_conc_cvao - no_night_inter_cvao) %>% 
  select(date:no2_conc_sub_30_days_cvao,no_night:no_night_corrected_cvao,scalar_mean_wd10,scalar_mean_ws10,mean_rh,jno2_mean,radon,ch4_ppb,ch4_bl_flag,co_ppb,co_bl_flag,co2_dry,cloud_condensation_nuclei_number_concentration,jno2_albedo:k)

remove(night_avg,night_flagged,night_zeroing,nights)

# NO nighttime offset plotting --------------------------------------------

#seeing if nighttime no correlates with anything
night_zeroed %>% 
  filter(radon <= 100,
         is.na(no_night) == F) %>% 
  mutate(month = month(date),
         WD = ifelse(scalar_mean_wd10 < 0, NA_real_,scalar_mean_wd10),
         `WS (km/h)` = ifelse(scalar_mean_ws10 < 0, NA_real_,scalar_mean_ws10),
         wind_bl_flag = ifelse(between(WD,190,280) & `WS (km/h)` >= 20,0,1),
         ccn_bl_flag = case_when(cloud_condensation_nuclei_number_concentration <= 855 & month == 1 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 820 & month == 2 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 745 & month == 3 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 560 & month == 4 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 310 & month == 5 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 265 & month == 6 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 215 & month == 7 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 305 & month == 8 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 465 & month == 9 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 635 & month == 10 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 850 & month == 11 ~ 0,
                              cloud_condensation_nuclei_number_concentration <= 820 & month == 12 ~ 0,
                              TRUE ~ 1),
         flag = case_when(ccn_bl_flag == 0 & wind_bl_flag == 0 ~ "All",
                          ccn_bl_flag == 0 ~ "Radon & CCN",
                          wind_bl_flag == 0 ~ "Radon & wind",
                          TRUE ~ "Radon")) %>% 
  rename(`RH (%)` = mean_rh,
         `Radon (mBq/m3)` = radon,
         `Methane (ppb)` = ch4_ppb,
         `Carbon monoxide (ppb)` = co_ppb,
         `CCN (counts/m3)` = cloud_condensation_nuclei_number_concentration) %>% 
  pivot_longer(c(`WS (km/h)`,WD,`RH (%)`,`Carbon monoxide (ppb)`,`Radon (mBq/m3)`,`CCN (counts/m3)`)) %>% 
  ggplot(aes(no_night,value,col = flag)) +
  theme_bw() +
  geom_point(size = 2) +
  labs(y = NULL,
       x = "Baseline nighttime NO (ppt)",
       col = NULL) +
  facet_wrap(~name,scales = "free") +
  theme(legend.position = "top",
        text = element_text(size = 16)) +
  NULL

# ggsave("nighttime_no_baseline.png",
#        path = "output/paper_plots",
#        height = 13,
#        width = 20,
#        units = "cm")

#comparing uncorrected and nighttime corrected NO
night_zeroed %>% 
  filter(no_ppt < 200,
         is.na(no_ppt_corr1) == F) %>% 
  mutate(baseline = ifelse(radon <= 100,0,1),
         no_uncorr = ifelse(baseline == 0,no_ppt,NA_real_),
         no_corr = ifelse(baseline == 0,no_night_corrected,NA_real_)) %>% 
  pivot_longer(c(no_uncorr,no_corr)) %>% 
  ggplot() +
  theme_bw() +
  geom_point(aes(date,value,col = name)) +
  geom_point(aes(date,no_night)) +
  geom_path(aes(date,no_night_inter)) +
  labs(x = NULL,
       y = "NO (ppt)",
       col = NULL) +
  theme(legend.position = "top") +
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %Y")

# ggsave("corr_vs_uncorr.png",
#        path = "output/paper_plots",
#        height = 13,
#        width = 27,
#        units = "cm")

#diurnals
night_zeroed %>% 
  filter(no_ppt < 200) %>% 
  mutate(baseline = ifelse(radon <= 100,0,1),
         no_uncorr = ifelse(baseline == 0,no_ppt,NA_real_),
         no_corr = ifelse(baseline == 0 & is.na(no_ppt_corr1) == F,no_night_corrected,NA_real_),
         hour = hour(date)) %>% 
  group_by(hour) %>% 
  summarise(across(c(no_corr,no_uncorr,no2_ppt),list(mean = ~mean(.,na.rm = T),se = ~sd(., na.rm = TRUE) / sqrt(length(.))))) %>% 
  ungroup() %>% 
  add_row(hour = c(1,2,3,4,5)) %>% 
  arrange(hour) %>% 
  mutate(uncorr_max = no_uncorr_mean + no_uncorr_se,
         uncorr_min = no_uncorr_mean - no_uncorr_se,
         corr_max = no_corr_mean + no_corr_se,
         corr_min = no_corr_mean - no_corr_se,) %>% 
  pivot_longer(c(no_uncorr_mean,no_corr_mean)) %>% 
  pivot_longer(cols = c(uncorr_max,corr_max),values_to = "max_err_v",names_to = "max_err_n") %>% 
  pivot_longer(cols = c(uncorr_min,corr_min),values_to = "min_err_v",names_to = "min_err_n") %>% 
  mutate(flag = case_when(name == "no_uncorr_mean" & min_err_n == "uncorr_min" & max_err_n == "uncorr_max" ~ "uncorr",
                          name == "no_corr_mean" & min_err_n == "corr_min" & max_err_n == "corr_max" ~ "corr")) %>% 
  filter(is.na(flag) == F) %>% 
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,value,col = name)) +
  geom_ribbon(aes(hour,ymin = min_err_v,ymax = max_err_v,fill = name),alpha = 0.25) +
  labs(x = "Hour of day",
       y = "NO (ppt)",
       col = NULL) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  NULL

# ggsave("corr_vs_uncorr_diurnal.png",
#        path = "output/paper_plots",
#        height = 13,
#        width = 27,
#        units = "cm")


# Nighttime NO and minima NO2 ---------------------------------------------

test = night_zeroed %>% 
  filter(radon <= 100) %>% 
  mutate(no_corr = ifelse(no_night_corrected < 200 & is.na(no_ppt_corr1) == F,no_night_corrected,NA_real_),
         flag = ifelse(no_night_corrected > no2_ppt_corr,"NO > NO2","NO2> NO"))

percentages <- test %>%
  filter(is.na(flag) == F) %>% 
  count(flag) %>%
  mutate(percent = (n / sum(n)) * 100)

test %>% 
  filter(is.na(flag) == F) %>% 
  pivot_longer(c(no_corr,no2_ppt_corr)) %>% 
  ggplot(aes(date,value,col = flag)) +
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
                                        no_night_corrected > 100 ~ NA_real_,
                                        TRUE ~ no_night_corrected),
         hour = hour(date)) %>% 
  filter(baseline == "Baseline") %>% 
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
  ggplot(aes(no2_ppt,no2_pss_night_corr)) +
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
