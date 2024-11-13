library(tidyverse)
library(janitor)
library(openair)
library(zoo)

Sys.setenv(TZ='UTC')

# k_298 = 1.8*10^-14 #rate constant for NO + O3 from Atkinson in cm3 molecule-1 s-1
#n/V =  p/RT = 1atm / (0.08206 L atm K-1 mol-1 * 298 K) = 0.0409 mol L-1 = 0.0409 * 10^-3 mol cm-3
#nmol mol-1 * 10^-12 *  6.022 * 10^23 molecules mol-1 * 0.0409 * 10^-3 mol cm-3
#2.46 * 10^7 molecules cm-3 conversion factor
ppt_to_molecules_cm3 <- function(x){y = x * 2.46 * 10^19 * 10^-12}
molecules_cm3_to_ppt <- function(x){y = x / ( 2.46 * 10^19 * 10^-12)}
tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}

# Reading in data ---------------------------------------------------------

#older ozone, submitted to WDC but still waiting to undergo QAA/QC
ozone_kcg = read.csv("data/kcg_data/CapeGrimOzone_Hourly_BiPM_2022.csv") %>% 
  mutate(date = dmy_hm(Timestamps),
         o3_ppb = ifelse(Status == 0, NA_real_,Mean..ppb.)) %>% 
  select(date,o3_ppb)

#met contains jno2, which is the same as jno2 in jno2 dataset
met_kcg = read.csv("data/kcg_data/KCG_LR_Supp_Data_2022_240506.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date))

radon_kcg = read.csv("data/kcg_data/CG_radon_2022_startofhour.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date),
         radon = ifelse(radon < 0,NA_real_,radon)) %>% 
  select(date,radon)

#uncorrected NO and NO2
nox_kcg1 = read.csv("data/kcg_data/Full_Ambient data_NOx_2405071232.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date_time)) %>% 
  select(date,no_ppt = no,no2_ppt = no2,nox_ppt = n_ox)

#for no two different calculations to determine inlet losses (ozone correction)
#no_ppt1 refers to data corrected in the same way as York data
#no_ppt2 refers to data corrected by doubling it (using NO cal gas - not fully sure how this is done yet)
nox_kcg2 = read.csv("data/kcg_data/2022KCGAmbient_NO2&NO_V9_York_2411051919.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(timestamp)) %>% 
  select(date,o3_ppb_nox = o3,no_ppt_corr1 = no_corr_il1,no_ppt_corr2 = no_corr_il2,no2_ppt_corr = no2ilc)

df_list = list(nox_kcg1,nox_kcg2,ozone_kcg,met_kcg,radon_kcg)

kcg_dat = df_list %>% reduce(left_join,by = "date") %>% 
  mutate(radon_flag = ifelse(radon <= 100,"Baseline","Not baseline"),
         jno2_albedo = (1 + 0.07) * jno2_mean,
         temp_k = mean_dbt + 273.15,
         k = 1.4 * 10^-12 * exp(-1310/temp_k))

remove(df_list,radon_kcg,met_kcg)

# Ozone comparisons ----------------------------------------------------

#two different ozones
kcg_dat %>% 
  # pivot_longer(c(o3_ppb_nox,o3_ppb)) %>% 
  ggplot(aes(o3_ppb_nox,o3_ppb)) +
  theme_bw() +
  geom_point() +
  geom_abline(col = "red",size = 0.8) +
  labs(x = expression(O[3]~with~NO[x]~data),
       y = expression(O[3]~shared~previously))

# ggsave("o3_comparison.png",
#        path = "output/new_kcg_data_plots",
#        height = 12,
#        width = 30,
#        units = "cm")


# NOx baseline timeseries -------------------------------------------------

kcg_dat %>% 
  mutate(radon_flag = ifelse(radon <= 100,"Baseline","Not baseline"),
         flag = ifelse(no_ppt_corr1 > no2_ppt_corr,"NO > NO2","NO < NO2"),
         `NO[2]~corrected` = ifelse(radon_flag == "Baseline",no2_ppt_corr,NA_real_),
         `NO~corrected~(IL1)` = ifelse(radon_flag == "Baseline",no_ppt_corr1,NA_real_),
         `NO~corrected~(IL2)` = ifelse(radon_flag == "Baseline",no_ppt_corr2,NA_real_)) %>% 
  pivot_longer(c(`NO[2]~corrected`,`NO~corrected~(IL2)`)) %>% 
  ggplot(aes(date,value,col = flag)) +
  theme_bw() +
  theme(legend.position = "top") +
  geom_point(group = 1) +
  labs(x = NULL,
       y = expression(NO[x]~mixing~ratios~(ppt)),
       col = NULL) +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  NULL
  
# ggsave("nox_il2_baseline_timeseries.png",
#        path = "output/new_kcg_data_plots",
#        height = 12,
#        width = 30,
#        units = "cm")

# PSS ---------------------------------------------------------------------

kcg_pss = kcg_dat %>% 
  mutate(radon_flag = ifelse(radon <= 100,"Radon","Not baseline"),
         o3_molecule_cm3 = ppt_to_molecules_cm3(o3_ppb * 1000),
         hour = hour(date),
         no_molecule_cm3_corr1 = ppt_to_molecules_cm3(no_ppt_corr1),
         no_molecule_cm3_corr2 = ppt_to_molecules_cm3(no_ppt_corr2)) %>% 
  filter(hour >= 10 & hour <= 14,
         radon_flag == "Radon") %>%
  mutate(no2_pss_corr1 = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3_corr1*k)/jno2_albedo),
         no2_pss_corr2 = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3_corr2*k)/jno2_albedo))

kcg_pss %>% 
  # rename("Using NO (IL1)" = no2_pss_corr1,
  #        "Using NO (IL2)" = no2_pss_corr2) %>% 
  # pivot_longer(c("Using NO (IL1)","Using NO (IL2)")) %>%
  mutate(flag = ifelse(no_ppt_corr1 > no2_ppt_corr,"NO > NO2","NO < NO2")) %>%
  ggplot(aes(no2_ppt_corr,no2_pss_corr1,col = flag)) +
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

ggsave("xy_plot_pss_larger.png",
       path = "output/new_kcg_data_plots",
       height = 12,
       width = 30,
       units = "cm")
