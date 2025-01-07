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
         night_flag = ifelse(hour > 22 | hour < 4,1,0))

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
            idx = min(idx)) %>% 
  mutate(idx = floor(idx)) %>% 
  ungroup()

night_zeroed = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_avg) %>% 
  mutate(no_night_inter = na.approx(no_night,na.rm = F),
         no_night_corrected = no_ppt - no_night_inter) %>% 
  select(date:nox_ppt,no_night_corrected,no_night,no_ppt_corr1:no2_ppt_corr,o3_ppb,scalar_mean_wd10:k)

remove(night_avg,night_flagged,night_zeroing,nights)

# Plotting ----------------------------------------------------------------

#seeing if nighttime no correlates with anything
night_zeroed %>% 
  filter(is.na(no_night) == F,
         scalar_mean_wd10 > 190 & scalar_mean_wd10 < 280,
         scalar_mean_ws10 >= 20,
         # no2_ppt_corr < 6
         ) %>%
  mutate(no_night = case_when(date >= "2022-02-01" & date < "2022-02-03" ~ NA_real_,
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
         flag = case_when(scalar_mean_wd10 > 190 & scalar_mean_wd10 < 280 ~ "Radon and wind filter",
                          scalar_mean_ws10 >= 20 ~ "Radon and wind filter",
                          TRUE ~ "Radon filter"),
         WD = ifelse(scalar_mean_wd10 < 0, NA_real_,scalar_mean_wd10),
         `WS (km/h)` = ifelse(scalar_mean_ws10 < 0, NA_real_,scalar_mean_ws10)) %>% 
  rename(`Nitrogen dioxide (ppt)` = no2_ppt_corr,
         `Ozone (ppb)` = o3_ppb,
         # WS = scalar_mean_ws10,
         # WD = scalar_mean_wd10,
         `RH (%)` = mean_rh,
         `Radon (mBq/m3)` = radon,
         `Methane (ppb)` = ch4_ppb,
         `Carbon monoxide (ppb)` = co_ppb) %>% 
  pivot_longer(c(`Nitrogen dioxide (ppt)`,`Ozone (ppb)`,`WS (km/h)`,WD,`RH (%)`,
                 `Carbon monoxide (ppb)`,`Methane (ppb)`,`Radon (mBq/m3)`)) %>% 
  ggplot(aes(value,no_night,col = name)) +
  theme_bw() +
  geom_point(size = 2,col = "springgreen4") +
  # scale_colour_manual(values = c("springgreen4","goldenrod1")) +
  geom_smooth(method = "lm",col = "darkorange",se = F,alpha = 0.2) +
  # scale_colour_manual(values = c("darkorange","firebrick4","navy","steelblue1","springgreen4",
                                 # "goldenrod1","deepskyblue3","darkolivegreen3")) +
  labs(x = NULL,
       y = "Baseline nighttime NO (ppt)",
       col = NULL) +
  # scale_x_datetime(date_breaks = "1 month",date_labels = "%b") +
  facet_wrap(~name,scales = "free") +
  theme(legend.position = "None",
        text = element_text(size = 16)) +
  # xlim(40,100) +
  NULL

# ggsave("nighttime_no_correlations.png",
#        path = "~/Writing/Thesis/Chapter 5 (NOx KCG)/Images",
#        height = 15.09,
#        width = 29.21,
#        units = "cm")

night_zeroed %>% 
  filter(radon < 100) %>%
  ggplot(aes(date,no_night)) +
  geom_point()

#comparing night correction to kcg correction and uncorrected data
night_zeroed %>%
  pivot_longer(c(no_ppt,no_ppt_corr1,no_night_corrected)) %>% 
  filter(radon < 100) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()


# KCG PSS -----------------------------------------------------------------

kcg_pss = night_zeroed %>% 
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
  pivot_longer(c(no2_pss_corr,no2_pss_night_corr,no2_pss_uncorr)) %>%
  ggplot(aes(no2_ppt_corr,value,col = name)) +
  theme_bw() +
  geom_point() +
  geom_abline(col = "red") +
  labs(x = expression(Measured~NO[2]~(ppt)),
       y = expression(PSS~NO[2]~(ppt)),
       col = NULL) +
  theme(legend.position = "top") +
  ylim(0,200) +
  xlim(0,200) +
  NULL

# ggsave("xy_plot_pss_larger.png",
#        path = "output/new_kcg_data_plots",
#        height = 12,
#        width = 30,
#        units = "cm")
