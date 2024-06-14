library(tidyverse)
library(lubridate)
library(janitor)
library(openair)
library(zoo)

Sys.setenv(TZ='UTC')

k = 1.8*10^-14 #rate constant for NO + O3 from Atkinson in cm3 molecule-1 s-1
#n/V =  p/RT = 1atm / (0.08206 L atm K-1 mol-1 * 298 K) = 0.0409 mol L-1 = 0.0409 * 10^-3 mol cm-3
#nmol mol-1 * 10^-12 *  6.022 * 10^23 molecules mol-1 * 0.0409 * 10^-3 mol cm-3
#2.46 * 10^7 molecules cm-3 conversion factor
ppt_to_molecules_cm3 <- function(x){y = x * 2.46 * 10^19 * 10^-12}
molecules_cm3_to_ppt <- function(x){y = x / ( 2.46 * 10^19 * 10^-12)}

# KCG data ----------------------------------------------------------------

#in ppb - new ozone that has been submitted to WDC but hasn't yet been through QA/QC, so may still change
#have checked this against old ozone dataset I was sent, this ozone is the same as teco6
ozone_kcg = read.csv("data/CapeGrimOzone_Hourly_BiPM_2022.csv") %>% 
  mutate(date = dmy_hm(Timestamps),
         o3_ppb = ifelse(Status == 0, NA_real_,Mean..ppb.)) %>% 
  select(date,o3_ppb)

#met contains jno2, which is the same as jno2 in jno2 dataset
met_kcg = read.csv("data/KCG_LR_Supp_Data_2022_240506.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date))

#bl_flag is the baseline flag, 0 is baseline, 1 is non-baseline and 9 is baseline not determined
co_kcg = read.table("data/cgo_picarro_1_60min_co_all_blflagged.dat",skip = 6,header = TRUE) %>% 
  clean_names() %>% 
  mutate(date = paste(yyyy,mm,dd),
         date = gsub(" ","-",date),
         date = paste(date,hh),
         date = ymd_h(date)) %>% 
  select(date,co_ppb:bl_flag) %>% 
  filter(date >= "2022-01-01" & date < "2023-01-01") %>% 
  rename_with(.fn = function(.x){paste0("co_",.x)},
              .cols = c(sd_ppb:bl_flag))

ch4_kcg = read.table("data/cgo_picarro_1_60min_ch4_all_blflagged.dat",skip = 6,header = TRUE) %>% 
  clean_names() %>% 
  mutate(date = paste(yyyy,mm,dd),
         date = gsub(" ","-",date),
         date = paste(date,hh),
         date = ymd_h(date)) %>% 
  select(date,ch4_ppb:bl_flag) %>% 
  filter(date >= "2022-01-01" & date < "2023-01-01")%>% 
  rename_with(.fn = function(.x){paste0("ch4_",.x)},
              .cols = c(sd_ppb:bl_flag))

radon_kcg = read.csv("data/CG_radon_2022_startofhour.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date),
         radon = ifelse(radon < 0,NA_real_,radon)) %>% 
  select(date,radon)

nox_kcg = read.csv("data/Full_Ambient data_NOx_2405071232.csv") %>% 
  clean_names() %>% 
  mutate(date = dmy_hm(date_time)) %>% 
  select(date,no_ppt = no,no2_ppt = no2,nox_ppt = n_ox)

# jno2_kcg = read.csv("data/jno2_22.csv") %>% 
#   mutate(date = as.POSIXct(date, format="%Y%m%d %H:%M:%S"))
  
df_list = list(nox_kcg,ch4_kcg,co_kcg,met_kcg,ozone_kcg,radon_kcg)

kcg_dat = df_list %>% reduce(left_join,by = "date") %>% 
  #it looks like there was some instrumental issue in November
  mutate(no_ppt = ifelse(date > "2022-11-16" & date < "2022-11-24",NA_real_,no_ppt),
         no2_ppt = ifelse(date > "2022-11-16" & date < "2022-11-24",NA_real_,no2_ppt),
         nox_ppt = ifelse(date > "2022-11-16" & date < "2022-11-24",NA_real_,nox_ppt),
         jno2_albedo = (1+ 0.07) * jno2_mean)

# KCG baseline investigation ----------------------------------------------

#use radon < 100 mBq/m3
#wind speed and direction can also be used (ws > 20 km hr-1 and wd between 190 and 280)

kcg_dat_flagged = kcg_dat %>% 
  mutate(radon_wind_flag = case_when(radon <= 100 
                                     & scalar_mean_ws10 >= 20 
                                     & vector_mean_wd10 >= 190 & vector_mean_wd10 <= 280
                                     ~ "Radon & wind",
                                     TRUE ~ "Not baseline"),
         radon_flag = ifelse(radon <= 100,"Radon","Not baseline"))

#for filtering data as baseline only if the whole day has been baseline
kcg_dat_baseline_days = kcg_dat_flagged %>% 
  mutate(doy = yday(date),
         radon_wind_flag = ifelse(radon_wind_flag == "Radon & wind",1,0)) %>% 
  group_by(doy) %>% 
  summarise(daily_baseline_flag = mean(radon_wind_flag))

kcg_dat_day_flag = kcg_dat_flagged %>% 
  mutate(doy = yday(date)) %>% 
  left_join(kcg_dat_baseline_days,by ="doy") %>% 
  mutate(super_baseline = case_when(daily_baseline_flag == 1 ~ "Baseline",
                                    daily_baseline_flag == 0 ~ "Not baseline",
                                    TRUE ~ "Partly baseline"))

# KCG timeseries ----------------------------------------------------------

#plotting data with different flags
kcg_dat_day_flag %>% 
  mutate(wind_flag = ifelse(scalar_mean_ws10 >= 20 & vector_mean_wd10 >= 190 & vector_mean_wd10 <= 280,
                            "Wind & radon","Just radon"),
         hour = hour(date)) %>% 
  rename(NO = no_ppt,
         "NO[2]" = no2_ppt) %>% 
  filter(radon_flag == "Radon",
         hour >=10 & hour <= 14) %>% 
  pivot_longer(c(NO,"NO[2]")) %>% 
  ggplot(aes(date,value,col = wind_flag)) +
  theme_bw() +
  labs(x = NULL,
       y = expression(NO[x]~(ppt)),
       col = NULL) +
  theme(legend.position = "top") +
  geom_point() +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed)

# ggsave("kcg_nox_baseline.svg",
#        path = "output/kcg_baseline",
#        height = 12.09,
#        width = 29.21,
#        units = "cm")

#timeseries while baseline and non-baseline in differnt colours
kcg_dat_day_flag %>% 
  filter(radon_flag == "Radon") %>% 
  # filter(date > "2022-09-15" ) %>%
  # mutate(no_ppt = ifelse(no_ppt < 250,no_ppt,NA_real_),
  #        no2_ppt = ifelse(no2_ppt < 500,no2_ppt,NA_real_)) %>%
  rename(NO = no_ppt,
         "NO[2]" = no2_ppt) %>% 
  pivot_longer(c(NO,"NO[2]")) %>%
  arrange(date) %>% 
  ggplot(aes(date,value,col = super_baseline)) +
  # ylim(0,500) +
  theme_bw() +
  geom_point() +
  scale_x_datetime(breaks = "1 month",date_labels = "%b %y") +
  # scale_colour_manual(values = c("Baseline" = "darkorange","Not baseline" = "steelblue1")) +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  labs(x = NULL,
       y = expression(NO[x]~(ppt)),
       col = NULL) +
  theme(legend.position = "top") +
  NULL

# ggsave("kcg_nox22.svg",
#        path = "actris_plots",
#        height = 15.47,
#        width = 40.55,
#        units = "cm")

# KCG baseline diurnals ---------------------------------------------------

kcg_dat_day_flag %>% 
  filter(radon_flag == "Radon") %>% 
  ggplot(aes(date,no_ppt)) +
  geom_point()

diurnal_kcg = kcg_dat_day_flag %>% 
  rename(NO_kcg = no_ppt,
         NO2_kcg = no2_ppt) %>% 
  filter(radon_flag == "Radon") %>% 
  # mutate(NO_corr = NO_kcg - 8) %>% #quick correction using nighttime value
  timeVariation(pollutant = c("NO_kcg"))

diurnal_kcg_dat = diurnal_kcg$data$hour

diurnal_kcg_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  # facet_grid(rows = vars(variable),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "NO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-1,13) +
  theme(legend.position = "top")


# CVAO data ---------------------------------------------------------------

#still need to remove negative spikes for both NO and NO2
nox_cvao = read.csv("data/cvao_nox2022.csv") %>% 
  mutate(date = ymd_hms(date),
         no2_flag = case_when(date > "2022-05-17 06:00:00" & date < "2022-05-17 18:00" ~ 0.456,
                              date > "2022-06-28 09:00:00" & date < "2022-06-29 06:00" ~ 0.456,
                              date > "2022-09-01 09:00:00" & date < "2022-09-23" ~ 0.456,
                             TRUE ~ no2_flag),
         no_flag = case_when(date > "2022-05-01 12:00:00" & date < "2022-05-02" ~ 0.456,
                             date > "2022-05-16 23:00:00" & date < "2022-05-17 18:00" ~ 0.456,
                             date > "2022-04-14 18:00:00" & date < "2022-04-15 02:00" ~ 0.456,
                             date > "2022-06-28 09:00:00" & date < "2022-06-29 06:00" ~ 0.456,
                             date > "2022-09-01 09:00:00" & date < "2022-09-23" ~ 0.456,
                             date > "2022-10-29 09:00:00" & date < "2022-10-30" ~ 0.456,
                             TRUE ~ no_flag))

cvao_dat = read.csv("data/20240507_CV_merge.csv") %>% 
  clean_names() %>% 
  filter(year == 2022) %>% 
  mutate(date = ymd_hms(date)) %>% 
  rename_with(~str_remove(.,".v$")) %>% 
  select(date,sahara:south_atlantic,o3_ppb,co_ppb,ch4_ppb = ch4_revised_ppb,co2_ppm = co2_revised_ppm,
         jno2_calc,j_no2,temp_10m_deg_c) %>% 
  left_join(nox_cvao,by = "date") %>% 
  select(date,no_ppt:no2_lod_ppt,no_flag:no2_u_ppt,everything(),-o3)

cvao_dat %>% 
  pivot_longer(c(no_ppt,j_no2)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()

# CVAO timeseries ---------------------------------------------------------

#plotting cvao timeseries, colour-coded to represent different flags
cvao_dat %>% 
  filter(date < "2022-11-01") %>%
  mutate(local_pollution = ifelse(no_flag == 0.599,"Local pollution","Clean air"),
         no_ppt = ifelse(no_flag < 0.8,no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag < 0.8,no2_ppt,NA_real_),
         no_flag = case_when(no_flag == 0 ~ "Good data",
                             no_flag == 0.147 ~ "Good data",
                             no_flag == 0.391 ~ "Data coverage < 50%",
                             no_flag == 0.456 ~ "Invalidated by data submitter",
                             no_flag == 0.459 ~ "Extreme value",
                             no_flag == 0.599 ~ "Local pollution"),
         no2_flag = case_when(no2_flag == 0 ~ "Good data",
                             no2_flag == 0.147 ~ "Good data",
                             no2_flag == 0.391 ~ "Data coverage < 50%",
                             no2_flag == 0.456 ~ "Invalidated by data submitter",
                             no2_flag == 0.459 ~ "Extreme value",
                             no2_flag == 0.599 ~ "Local pollution")) %>% 
  # rename(NO = no_ppt,
  #        "NO[2]" = no2_ppt) %>% 
  # pivot_longer(c(NO,"NO[2]")) %>% 
  ggplot(aes(date,no2_ppt,col = no2_flag)) +
  theme_bw() +
  geom_point(group = 1,size = 2) +
  # facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  labs(x = NULL,
       y = expression(NO[2]~(ppt)),
       col = NULL) +
  scale_x_datetime(breaks = "1 month",date_labels = "%b %y") +
  scale_colour_manual(values = c("Good data" = "gray18",
                                 "Extreme value" = "red",
                                 "Data coverage < 50%" = "springgreen",
                                 "Local pollution" = "royalblue2",
                                 "Invalidated by data submitter" = "gray")) +
  theme(legend.position = "top") +
  NULL

# ggsave("cvao_no2_flagged.svg",
#        path = "output/kcg_cvao_measurements",
#        height = 12.09,
#        width = 29.21,
#        units = "cm")

cvao_dat %>% 
  mutate(local_pollution = ifelse(no_flag == 0.599,"Local pollution","Clean air"),
         no_ppt = ifelse(no_flag < 0.2,no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag < 0.2,no2_ppt,NA_real_),
         hour = hour(date)) %>% 
  filter(date < "2022-11-01",
         # hour >= 11 & hour <= 15
         ) %>%
  rename(NO = no_ppt,
         "NO[2]" = no2_ppt) %>%
  pivot_longer(c(NO,"NO[2]")) %>%
  ggplot(aes(date,value)) +
  theme_bw() +
  geom_point(group = 1,size = 2) +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  labs(x = NULL,
       y = expression(NO[x]~(ppt))) +
  scale_x_datetime(breaks = "1 month",date_labels = "%b %y") +
  theme(legend.position = "top") +
  NULL

# CVAO diurnals -----------------------------------------------------------

diurnal_cvao = cvao_dat %>% 
  mutate(NO_cvao = ifelse(no_flag < 0.15,no_ppt,NA_real_),
         NO2_cvao = ifelse(no2_flag < 0.15,no2_ppt,NA_real_)) %>% 
  timeVariation(pollutant = c("NO_cvao"))

diurnal_cvao_dat = diurnal_cvao$data$hour %>% 
  mutate(hour = hour + 1,
         hour = ifelse(hour == 24,0,hour)) %>%
  arrange(hour)

diurnal_cvao_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  # facet_grid(rows = vars(variable),scales = "free_y",labeller = label_parsed) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Hour of day (UTC)",
       y = "NO (ppt)",
       color = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-1,13) +
  theme(legend.position = "top")


# KCG and CVAO diurnals together ------------------------------------------

diurnal_dat_no = diurnal_cvao_dat %>% 
  rename_with( .fn = function(.x){paste0(.x,"_cvao")},
               .cols=-hour) %>% 
  left_join(diurnal_kcg_dat,by = "hour")

diurnal_dat_no %>% 
  rename(KCG = Mean,
         CVAO = Mean_cvao) %>% 
  pivot_longer(c(KCG,CVAO)) %>% 
  ggplot(aes(hour,value,col = name)) +
  geom_path(size = 2) +
  theme_bw() +
  labs(x = "Hour of day",
       y = expression(NO[x]~(ppt)),
       color = NULL) +
  scale_colour_manual(values = c("CVAO" = "darkorange","KCG" = "steelblue1")) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  theme(axis.title = element_text(size = 28),
        strip.text = element_text(size = 28),
        axis.text = element_text(size = 20),
        legend.position = "top",
        legend.text = element_text(size = 20)) +
  NULL

# ggsave("no_diurnal_corr.svg",
#        path = "actris_plots",
#        height = 15,
#        width = 15,
#        units = "cm")

# CVAO PSS ----------------------------------------------------------------

cvao_pss = cvao_dat %>% 
  mutate(o3_molecule_cm3 = ppt_to_molecules_cm3(o3_ppb * 1000),
         hour = hour(date),
         no_ppt = ifelse(no_flag < 0.15,no_ppt,NA_real_),
         no_molecule_cm3 = ppt_to_molecules_cm3(no_ppt),
         no2_molecule_cm3 = ppt_to_molecules_cm3(no2_ppt),
         no2_lifetime = (1/j_no2)/60,
         temp_k = temp_10m_deg_c + 273.15,
         k1 = 1.4 *10-12 * exp(-1310/temp_k)) %>% 
  filter(hour >= 11 & hour <= 15 ) %>% 
  mutate(no2_pss = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3*k)/j_no2),
         leighton_ratio = (j_no2*no2_molecule_cm3)/(k*o3_molecule_cm3*no_molecule_cm3))

#no2 obs vs no2 pps
cvao_pss %>% 
  ggplot(aes(no2_ppt,no2_pss)) +
  theme_bw() +
  geom_point() +
  xlim(0,50) +
  ylim(0,50) +
  labs(x = expression(NO[2~Obs]~(ppt)),
       y = expression(NO[2~PSS]~(ppt))) +
  geom_abline(slope = 1,intercept = 0,col = "steelblue1",size = 1) +
  geom_abline(slope = 0.49,intercept = -2.84,col = "darkorange",size = 1) +
  NULL

model = lm(no2_pss ~ no2_ppt,cvao_pss)
summary(model)

#coloured by no2 photolysis lifetime
cvao_pss %>% 
  mutate(no2_lifetime = (1/j_no2)/60) %>% 
  filter(no2_lifetime <= 10) %>% 
  ggplot(aes(no2_ppt,no2_pss,col = no2_lifetime)) +
  theme_bw() +
  geom_point() +
  xlim(0,50) +
  ylim(0,50) +
  labs(x = expression(NO[2~Obs]~(ppt)),
       y = expression(NO[2~PSS]~(ppt)),
       col = expression(NO[2]~photolysis~lifetime~(min))
       ) +
  geom_abline(slope = 1,intercept = 0,col = "steelblue1",size = 1) +
  geom_abline(slope = 0.49,intercept = -2.84,col = "darkorange",size = 1) +
  scale_colour_viridis_c() +
  NULL

ggsave("cvao_no2_pss_lifetime.svg",
       path = "output/cvao_no2_pss",
       height = 12.09,
       width = 16.43,
       units = "cm")

# KCG PSS -----------------------------------------------------------------

kcg_pss = kcg_dat_day_flag %>% 
  mutate(o3_molecule_cm3 = ppt_to_molecules_cm3(o3_ppb * 1000),
         hour = hour(date),
         no_molecule_cm3 = ppt_to_molecules_cm3(no_ppt),
         no2_molecule_cm3 = ppt_to_molecules_cm3(no2_ppt),
         no2_lifetime = (1/jno2_albedo)/60) %>% 
  filter(hour >= 10 & hour <= 14,
         # no_ppt >0
  ) %>%
  mutate(no2_pss_albedo = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3*k)/jno2_albedo),
         no2_pss = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3*k)/jno2_median),
         leighton_ratio = (jno2_albedo*no2_molecule_cm3)/(k*o3_molecule_cm3*no_molecule_cm3))

kcg_pss %>% 
  mutate(wind_flag = ifelse(scalar_mean_ws10 >= 20 & vector_mean_wd10 >= 190 & vector_mean_wd10 <= 280,
                            "Wind & radon","Just radon")) %>% 
  filter(radon_flag == "Radon",
         # no2_ppt < 200
         ) %>% 
  ggplot(aes(no2_ppt,no2_pss_albedo,col = wind_flag)) +
  geom_point() +
  theme_bw() +
  labs(x = expression(NO[2~Obs]~(ppt)),
       y = expression(NO[2~PSS]~(ppt)),
       col = NULL) +
  geom_abline(intercept = 0, slope = 1) +
  theme(legend.position = "top") +
  # scale_colour_viridis_c() +
  NULL

#colour coded by lifetime
kcg_pss %>% 
  mutate(no2_lifetime = (1/jno2_albedo)/60,
         doy = yday(date),
         month = month(date),
         season = case_when(month >= 3 & month <= 5 ~ "Autumn (MAM)",
                            month >= 6 & month <= 8 ~ "Winter (JJA)",
                            month >= 9 & month <= 11 ~ "Spring (SON)",
                            TRUE ~ "Summer (DJF)")) %>% 
  filter(radon_flag == "Radon",
         no2_ppt < 200,
         no2_lifetime <= 10) %>% 
  ggplot(aes(no2_ppt,no2_pss_albedo,col = season)) +
  geom_point() +
  theme_bw() +
  labs(x = expression(NO[2~Obs]~(ppt)),
       y = expression(NO[2~PSS]~(ppt)),
       col = NULL
       ) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~season) +
  # theme(legend.position = "top") +
  # scale_colour_viridis_c() +
  scale_colour_manual(values = c("Autumn (MAM)" = "darkorange",
                                 "Winter (JJA)" = "steelblue1",
                                 "Spring (SON)" = "springgreen4",
                                 "Summer (DJF)" = "gold"
                                 )) +
  theme(legend.position = "none")
  NULL

model = lm(no2_pss ~ no2_ppt,cvao_pss)
summary(model)

ggsave("kcg_no2_pss_seasonal.svg",
       path = "output/kcg_no2_pss",
       height = 12.09,
       width = 29.21,
       units = "cm")


# Comparing jno2 at the two sites -----------------------------------------

#have some data gaps for 2022, katie hasn't run code to calculate jno2 to fill those gaps yet
#currently just looking at jno2 for all years available in the merge
jno2_cvao = read.csv("data/20240507_CV_merge.csv") %>% 
  clean_names() %>% 
  mutate(date = ymd_hms(date)) %>% 
  rename_with(~str_remove(.,".v$")) %>% 
  select(date,jno2_calc,j_no2)

#going to plot jno2 values from both sites on the same plot, going to just use local time for both sites
jno2 = jno2_cvao %>% 
  #cvao date is usually in utc, changing it here to be lt (utc-1)
  mutate(date = date - 3600,
         year = year(date)) %>% 
  #jno2 before 2020 looks suspiciously low
  filter(year > 2020) %>% 
  select(date,jno2_cvao = j_no2,jno2_calc_cvao = jno2_calc) %>% 
  left_join(kcg_dat,by = "date") %>% 
  select(date,jno2_cvao,jno2_calc_cvao,jno2_kcg = jno2_mean,KCG = jno2_albedo)

jno2 %>% 
  mutate(hour = hour(date)) %>% 
  filter(hour >= 10 & hour <= 14) %>% 
  pivot_longer(c(jno2_kcg,KCG,jno2_cvao,jno2_calc_cvao)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()

diurnal_jno2 = jno2 %>% 
  mutate(hour = hour(date)) %>% 
  filter(hour >= 10 & hour <= 14) %>%
  rename(KCG = jno2_albedo_kcg,CVAO = jno2_cvao) %>% 
  timeVariation(pollutant = c("KCG","CVAO"),type = "season")

diurnal_jno2_dat = diurnal_jno2$data$hour %>% 
  mutate(season = case_when(variable == "KCG" & season == "spring (MAM)" ~ "Autumn",
                            variable == "KCG" & season == "summer (JJA)" ~ "Winter",
                            variable == "KCG" & season == "autumn (SON)" ~ "Spring",
                            variable == "KCG" & season == "winter (DJF)" ~ "Summer",
                            variable == "CVAO" & season == "spring (MAM)" ~ "Spring",
                            variable == "CVAO" & season == "summer (JJA)" ~ "Summer",
                            variable == "CVAO" & season == "autumn (SON)" ~ "Autumn",
                            variable == "CVAO" & season == "winter (DJF)" ~ "Winter"))

diurnal_jno2_dat %>% 
  ggplot(aes(hour,Mean,col = variable)) +
  geom_line(size = 1) +
  facet_wrap(~season) +
  theme_bw() +
  labs(x = "Hour of day",
       y = expression(j[NO[2]]~s^{-1}),
       color = NULL) +
  # scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  # ylim(-1,13) +
  theme(legend.position = "top")

ggsave("jno2_diurnals.svg",
       path = "output/kcg_cvao_measurements",
       height = 12.09,
       width = 29.21,
       units = "cm")
