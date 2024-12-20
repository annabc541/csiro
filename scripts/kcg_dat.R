library(tidyverse)
library(janitor)
library(openair)
library(zoo)

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

#older ozone, submitted to WDC but still waiting to undergo QA/QC
#ozone included in corrected nox data
# ozone_kcg = read.csv("data/kcg_data/CapeGrimOzone_Hourly_BiPM_2022.csv") %>% 
#   mutate(date = dmy_hm(Timestamps),
#          o3_ppb = ifelse(Status == 0, NA_real_,Mean..ppb.)) %>% 
#   select(date,o3_ppb)

#met contains jno2, which is the same as jno2 in jno2 dataset
met_kcg = read.csv("data/kcg_data/KCG_LR_Supp_Data_2022_240506.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date))

radon_kcg = read.csv("data/kcg_data/CG_radon_2022_startofhour.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date),
         radon = ifelse(radon < 0,NA_real_,radon)) %>% 
  select(date,radon)

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

df_list = list(nox_kcg_uncorr,nox_kcg_corr,met_kcg,radon_kcg,ch4_kcg,co_kcg)

kcg_dat = df_list %>% reduce(left_join,by = "date") %>% 
  mutate(radon_flag = ifelse(radon <= 100,"Baseline","Not baseline"),
         jno2_albedo = (1 + 0.07) * jno2_mean,
         temp_k = mean_dbt + 273.15,
         k = 1.4 * 10^-12 * exp(-1310/temp_k))

remove(df_list,radon_kcg,met_kcg,ch4_kcg,co_kcg)


# Calculating nighttime NO offset -----------------------------------------

night_zeroing = kcg_dat %>% 
  mutate(hour = hour(date),
         night_flag = ifelse(hour > 21 | hour < 4,1,0))

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
  filter(id != 0,
         radon < 100) %>% 
  group_by(id) %>% 
  summarise(no_night = median(no_ppt,na.rm = T),
            idx = mean(idx)) %>% 
  mutate(idx = round(idx)) %>% 
  ungroup()

night_zeroed = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_avg) %>% 
  mutate(no_night_inter = na.approx(no_night,na.rm = F),
         no_night_corrected = no_ppt - no_night_inter) %>% 
  select(date:nox_ppt,no_night_corrected,no_night,no_ppt_corr1:no2_ppt_corr,o3_ppb,scalar_mean_wd10:k)

remove(night_avg,night_flagged,night_zeroing,nights)


# Plotting ----------------------------------------------------------------

night_zeroed %>% 
  filter(radon < 100) %>%
  ggplot(aes(radon,no_night)) +
  geom_point()

#comparing night correction to kcg correction and uncorrected data
night_zeroed %>%
  pivot_longer(c(no_uncorrected,no_corr1,no_night_corrected)) %>% 
  filter(radon < 100) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()
