library(tidyverse)
library(janitor)
library(openair)
library(zoo)

#using baseline nighttime NO for KCG offset calculations

Sys.setenv(TZ='UTC')
k = 1.8*10^-14 #rate constant for NO + O3 from Atkinson in cm3 molecule-1 s-1

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

#data I was sent in May 2024 - hasn't been offset corrected at all
nox_kcg_uncorrected = read.csv("data/kcg_data/Full_Ambient data_NOx_2405071232.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date_time)) %>% 
  select(date,no_uncorrected = no,no2_uncorrected = no2,nox_ppt = n_ox)

#data from November 2024 - ozone and offset corrected
nox_kcg_corrected = read.csv("data/kcg_data/2022KCGAmbient_NO2&NO_V9_York_2411051919.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(timestamp)) %>% 
  select(date,o3_ppb = o3,no_corr1 = no_corr_il1,no_corr2 = no_corr_il2,no2_corr = no2ilc)

#joining corrected and uncorrected nox to remove dates when there were issues

nox_kcg = nox_kcg_uncorrected %>% 
  left_join(nox_kcg_corrected) %>% 
  mutate(no_uncorrected = ifelse(is.na(no_corr1),NA_real_,no_uncorrected),
         no2_uncorrected = ifelse(is.na(no2_corr),NA_real_,no2_uncorrected))

radon_kcg = read.csv("data/kcg_data/CG_radon_2022_startofhour.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date),
         radon = ifelse(radon < 0,NA_real_,radon)) %>% 
  select(date,radon)

# Nighttime zero ----------------------------------------------------------

nox_kcg_night_correction = nox_kcg %>% 
  left_join(radon_kcg,by = "date")

night_zeroing = nox_kcg_night_correction %>% 
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
  summarise(no_night = median(no_uncorrected,na.rm = T),
            idx = mean(idx)) %>% 
  mutate(idx = round(idx)) %>% 
  ungroup()

night_zeroed = night_zeroing %>% 
  mutate(idx = 1:nrow(.)) %>% 
  left_join(night_avg) %>% 
  mutate(no_night = na.approx(no_night,na.rm = F),
         no_night_corrected = no_uncorrected - no_night)


# Plotting ----------------------------------------------------------------

night_zeroed %>%
  pivot_longer(c(no_uncorrected,no_corr1,no_night_corrected)) %>% 
  filter(radon < 100) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()


# NO2 PSS with different NO -----------------------------------------------

met_kcg = read.csv("data/kcg_data/KCG_LR_Supp_Data_2022_240506.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date))

kcg_pss = night_zeroed %>% 
  select(date,no_uncorrected,no2_uncorrected,o3_ppb,no_corr1:radon,no_night_corrected) %>% 
  left_join(met_kcg) %>% 
  mutate(jno2_albedo = (1 + 0.07) * jno2_mean,
         temp_k = mean_dbt + 273.15,
         k = 1.4 * 10^-12 * exp(-1310/temp_k),
         o3_molecule_cm3 = ppt_to_molecules_cm3(o3_ppb * 1000),
         hour = hour(date),
         no_molecule_cm3_uncorr = ppt_to_molecules_cm3(no_uncorrected),
         no_molecule_cm3_corr = ppt_to_molecules_cm3(no_corr1),
         no_molecule_cm3_night_corr = ppt_to_molecules_cm3(no_night_corrected)) %>% 
  filter(hour >= 10 & hour <= 14,
         radon < 100) %>%
  mutate(no2_pss_uncorr = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3_uncorr*k)/jno2_albedo),
         no2_pss_corr = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3_corr*k)/jno2_albedo),
         no2_pss_night_corr = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3_night_corr*k)/jno2_albedo))

kcg_pss %>% 
  pivot_longer(c(no2_pss_corr,no2_pss_night_corr,no2_pss_uncorr)) %>%
  ggplot(aes(no2_corr,value,col = name)) +
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
