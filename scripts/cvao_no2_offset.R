library(tidyverse)
library(openair)
library(zoo)

Sys.setenv(TZ = "UTC")
setwd("~/Cape Verde/nox")

# Reading in data ---------------------------------------------------------

nox2017 = read.csv("processing/processed_data/NOx_2017_calc_df_new_altered_night(21-03)_v17.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>%
  filter(date > "2017-01-01 00:00" & date < "2018-01-01 00:00") %>%
  select(date,
         no_uncorrected = NO_Conc,
         no2_blc_uncorrected = NO2_Conc,
         no2_diodes = NO2_Conc_diode,
         no_night = NO_night_average,
         no_corrected = NO_Conc_art_corrected,
         no2_blc_corrected = NO2_Conc_art_corrected_using_diode,
         CE_diode) %>% 
  timeAverage("5 min")

nox2018 = read.csv("processing/processed_data/NOx_2018_calc_df_new_altered_night(21-03)_v17.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>%
  filter(date > "2018-01-01 00:00" & date < "2019-01-01 00:00") %>%
  select(date,
         no_uncorrected = NO_Conc,
         no2_blc_uncorrected = NO2_Conc,
         no2_diodes = NO2_Conc_diode,
         no_night = NO_night_average,
         no_corrected = NO_Conc_art_corrected,
         no2_blc_corrected = NO2_Conc_art_corrected_using_diode,
         CE_diode) %>% 
  timeAverage("5 min")

nox2019 = read.csv("processing/processed_data/NOx_2019_calc_df_new_altered_night(21-03)_v17.csv") %>%
  tibble() %>%
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>%
  filter(date > "2019-01-01 00:00" & date < "2020-01-01 00:00") %>%
  select(date,
         no_uncorrected = NO_Conc,
         no2_blc_uncorrected = NO2_Conc,
         no2_diodes = NO2_Conc_diode,
         no_night = NO_night_average,
         no_corrected = NO_Conc_art_corrected,
         no2_blc_corrected = NO2_Conc_art_corrected_using_diode,
         CE_diode) %>%
  timeAverage("5 min")

nox2020 = read.csv("processing/processed_data/NOx_2020_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2020-01-01 00:00" & date < "2021-01-01 00:00") %>% 
  select(date,
         no_uncorrected = NO_Conc,
         no2_blc_uncorrected = NO2_Conc,
         no2_diodes = NO2_Conc_diode,
         no_night = NO_night_mean,
         no_corrected = NO_Conc_art_corrected,
         no2_blc_corrected = NO2_Conc_art_corrected,
         CE_diode) %>%  
  timeAverage("5 min")

nox2021 = read.csv("processing/processed_data/NOx_2021_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2021-01-01 00:00" & date < "2022-01-01 00:00") %>% 
  select(date,
         no_uncorrected = NO_Conc,
         no2_blc_uncorrected = NO2_Conc,
         no2_diodes = NO2_Conc_diode,
         no_night = NO_night_mean,
         no_corrected = NO_Conc_art_corrected,
         no2_blc_corrected = NO2_Conc_art_corrected,
         CE_diode) %>% 
  timeAverage("5 min")

nox2022 = read.csv("processing/processed_data/NOx_2022_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2022-01-01 00:00" & date < "2023-01-01 00:00") %>% 
  select(date,
         no_uncorrected = NO_Conc,
         no2_blc_uncorrected = NO2_Conc,
         no2_diodes = NO2_Conc_diode,
         no_night = NO_night_mean,
         no_corrected = NO_Conc_art_corrected,
         no2_blc_corrected = NO2_Conc_art_corrected,
         CE_diode) %>% 
  timeAverage("5 min")

nox2023 = read.csv("processing/processed_data/NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2023-01-01 00:00" & date < "2024-01-01 00:00") %>% 
  select(date,
         no_uncorrected = NO_Conc,
         no2_blc_uncorrected = NO2_Conc,
         no2_diodes = NO2_Conc_diode,
         no_night = NO_night_mean,
         no_corrected = NO_Conc_art_corrected,
         no2_blc_corrected = NO2_Conc_art_corrected,
         CE_diode) %>% 
  timeAverage("5 min")

nox2024 = read.csv("processing/processed_data/NOx_2024_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2024-01-01 00:00" & date < "2025-01-01 00:00") %>% 
  select(date,
         no_uncorrected = NO_Conc,
         no2_blc_uncorrected = NO2_Conc,
         no2_diodes = NO2_Conc_diode,
         no_night = NO_night_mean,
         no_corrected = NO_Conc_art_corrected,
         no2_blc_corrected = NO2_Conc_art_corrected,
         CE_diode) %>% 
  timeAverage("5 min")

# NO2 offset based on monthly minimum -------------------------------------

nox_only = bind_rows(nox2017,nox2018,nox2019,nox2020,nox2021,nox2022,nox2023,nox2024) %>% 
  arrange(date) %>% timeAverage("1 hour")

nox_min_day = nox_only %>%
  mutate(no2_diodes = ifelse(no2_diodes <= 0, NA_real_,no2_diodes)) %>% 
  timeAverage("1 day",statistic = "min") %>% 
  mutate(no2_diodes_min = case_when(no2_diodes == Inf ~ NA_real_,
                                    TRUE ~ no2_diodes)) %>% 
  select(date,no2_diodes_min)

nox_min_month = nox_min_day %>%
  timeAverage("1 month",statistic = "min") %>% 
  mutate(no2_diodes_min_month = case_when(no2_diodes_min == Inf ~ NA_real_,
                                    TRUE ~ no2_diodes_min)) %>% 
  select(date,no2_diodes_min_month)

nox_min_month %>% 
  ggplot(aes(date,no2_diodes_min_month)) +
  theme_bw() +
  geom_point(col = "navy",size = 2.5) +
  labs(x = NULL,
       y = expression(NO[2]~monthly~minima~(ppt))) +
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y") +
  theme(text = element_text(size = 20))

# ggsave('no2_monthly_minima.png',
#        path = "~/Writing/Thesis/Chapter 2 (NOx processing)/Images",
#        width = 29,
#        height = 12,
#        units = 'cm')

nox_with_min = nox_only %>% 
  left_join(nox_min_month) %>% 
  mutate(no2_diodes_min_inter = na.approx(no2_diodes_min_month,na.rm = F)) %>% 
  # fill(no2_diodes_min,.direction = "downup") %>% 
  mutate(no2_diodes_corr_inter = no2_diodes - no2_diodes_min_inter,
         no2_diodes_corr = no2_diodes - no2_diodes_min_month) %>% 
  filter(date < "2024-11-01")

nox_to_save = nox_with_min %>% 
  select(date,no = no_corrected,no2_uncorr = no2_diodes,no2_corr = no2_diodes_corr_inter,no2_diodes_min,CE_diode)

# write.csv(nox_to_save,"cvao_nox_corr.csv",row.names = F)

nox_with_min %>% 
  ggplot(aes(x = date)) +
  geom_path(aes(y = no2_diodes)) +
  geom_point(aes(y = no2_diodes_min_month),col = "red") +
  ylim(0,200)

nox_with_min %>% 
  mutate(year = year(date)) %>% 
  # timeAverage("1 day") %>%
  filter(year == 2024) %>% 
  pivot_longer(c(no2_diodes,no2_diodes_corr_inter)) %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_path() +
  ylim(-30,200) +
  labs(x = NULL,
       y = expression(NO[2]~(ppt)),
       col = NULL) +
  theme(legend.position = "top")

# ggsave("no2_corr_with_hourly2024.png",
#        path = "~/csiro/output/cvao_no2_offsets",
#        height = 12,
#        width = 30,
#        units = "cm")

nox_with_min %>% 
  mutate(year = year(date)) %>% 
  pivot_longer(c(no2_diodes,no2_diodes_corr_inter)) %>% 
  filter(year == 2020) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  ylim(-5,200)

# Reading in offsets ------------------------------------------------------

offsets2017 = read.csv("processing/processed_data/NOx_2017_NOx_art_df_new_altered_night(21-03)_v17.csv") %>%  
  tibble() %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2017-01-01 00:00" & date < "2018-01-01 00:00") %>% 
  select(date,
         no2_blc_pag = PAG_Zero_NO2_Conc_mean,
         no2_diode_pag = PAG_Zero_NO2_Conc_diode_mean,
         blc_pag_offsets = PAG_Zero_NO2_Conc_corrected)

offsets2018 = read.csv("processing/processed_data/NOx_2018_NOx_art_df_new_altered_night(21-03)_v17.csv") %>%  
  tibble() %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2018-01-01 00:00" & date < "2019-01-01 00:00") %>% 
  select(date,
         no2_blc_pag = PAG_Zero_NO2_Conc_mean,
         no2_diode_pag = PAG_Zero_NO2_Conc_diode_mean,
         blc_pag_offsets = PAG_Zero_NO2_Conc_corrected)

offsets2019 = read.csv("processing/processed_data/NOx_2019_NOx_art_df_new_altered_night(21-03)_v17.csv") %>%  
  tibble() %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2019-01-01 00:00" & date < "2020-01-01 00:00") %>% 
  select(date,
         no2_blc_pag = PAG_Zero_NO2_Conc_mean,
         no2_diode_pag = PAG_Zero_NO2_Conc_diode_mean,
         blc_pag_offsets = PAG_Zero_NO2_Conc_corrected)

offsets2020 = read.csv("processing/processed_data/NOx_2020_art_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2020-01-01 00:00" & date < "2021-01-01 00:00") %>% 
  select(date,
         no2_blc_pag = PAG_Zero_NO2_Conc_last,
         no2_diode_pag = PAG_Zero_NO2_Conc_diode_last,
         blc_pag_offsets = PAG_Zero_NO2_BLC_art)

offsets2021 = read.csv("processing/processed_data/NOx_2021_art_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2021-01-01 00:00" & date < "2022-01-01 00:00") %>% 
  select(date,
         no2_blc_pag = PAG_Zero_NO2_Conc_last,
         no2_diode_pag = PAG_Zero_NO2_Conc_diode_last,
         blc_pag_offsets = PAG_Zero_NO2_BLC_art)

offsets2022 = read.csv("processing/processed_data/NOx_2022_art_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2022-01-01 00:00" & date < "2023-01-01 00:00") %>% 
  select(date,
         no2_blc_pag = PAG_Zero_NO2_Conc_last,
         no2_diode_pag = PAG_Zero_NO2_Conc_diode_last,
         blc_pag_offsets = PAG_Zero_NO2_BLC_art)

offsets2023 = read.csv("processing/processed_data/NOx_2023_art_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2023-01-01 00:00" & date < "2024-01-01 00:00") %>% 
  select(date,
         no2_blc_pag = PAG_Zero_NO2_Conc_last,
         no2_diode_pag = PAG_Zero_NO2_Conc_diode_last,
         blc_pag_offsets = PAG_Zero_NO2_BLC_art)

offsets2024 = read.csv("processing/processed_data/NOx_2024_art_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date,"5 min")) %>% 
  filter(date > "2024-01-01 00:00" & date < "2025-01-01 00:00") %>% 
  select(date,
         no2_blc_pag = PAG_Zero_NO2_Conc_last,
         no2_diode_pag = PAG_Zero_NO2_Conc_diode_last,
         blc_pag_offsets = PAG_Zero_NO2_BLC_art)

offsets = bind_rows(offsets2017,offsets2018,offsets2019,offsets2020,offsets2021,offsets2022,offsets2023,offsets2024) %>% 
  arrange(date)

offsets %>% 
  pivot_longer(c(no2_blc_pag,no2_diode_pag,blc_pag_offsets)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point()

# Binding rows and selecting columns --------------------------------------

nox = bind_rows(nox2017,nox2018,nox2019,nox2020,nox2021,nox2022,nox2023,nox2024) %>% 
  arrange(date) %>% 
  left_join(offsets,by = "date")

nox_hourly = nox %>% timeAverage("1 hour")
nox_daily = nox_hourly %>% timeAverage("1 day")
nox_min_month = nox_hourly %>%
  filter(no2_blc_uncorrected > 0) %>%
  timeAverage("1 month",statistic = "min")

nox_min_month %>% 
  rename(`BLC minimum` = no2_blc_uncorrected,
         # `Diodes minimum` = no2_diodes,
         `Diodes PAG measurement (minimum)` = no2_diode_pag,
         `BLC PAG measurement (corrected)` = blc_pag_offsets) %>% 
  pivot_longer(c(`BLC minimum`,
                 `BLC PAG measurement (corrected)`)) %>% 
  filter(value != Inf) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point(size = 2.25) +
  theme_bw() +
  # ylim(0,40) +
  scale_colour_manual(values = c("steelblue1","darkorange1")) +
  labs(x = NULL,
       y = expression(NO[2]~(ppt)),
       col = NULL) +
  theme(legend.position = "top")

# ggsave("no2_blc_offsets_pag_min.png",
#        path = "~/csiro/output/cvao_no2_offsets",
#        height = 12,
#        width = 30,
#        units = "cm")


# For calculating different statistics for different variables ------------

#getting the count soo days and months when less than half the values are available are discarded
nox_min_day1 = nox_only %>% 
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         no2_diodes = ifelse(no2_diodes <= 0, NA_real_,no2_diodes)) %>% 
  group_by(day,month,year) %>% 
  summarise(no2_diodes_min = min(no2_diodes,na.rm = T),
            no2_diodes_count = sum(!is.na(no2_diodes))) %>% 
  arrange(year,month,day) %>% 
  ungroup() %>% 
  mutate(date = glue::glue("{year}-{month}-{day}"),
         date = ymd(date),
         no2_diodes_min = case_when(no2_diodes_min == Inf ~ NA_real_,
                                    no2_diodes_count < 12 ~ NA_real_,
                                    TRUE ~ no2_diodes_min)) %>% 
  select(date,no2_diodes_min)

nox_min_day1 %>% 
  ggplot(aes(date,no2_diodes_min)) +
  geom_point() +
  theme(legend.position = "top")

nox_min_month1 = nox_min_day1 %>% 
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(month,year) %>% 
  summarise(no2_diodes_min_month = min(no2_diodes_min,na.rm = T),
            no2_diodes_count = sum(!is.na(no2_diodes_min)),
            no2_diodes_offset_var = var(no2_diodes_min,na.rm = T)) %>% 
  arrange(year,month) %>% 
  mutate(day = 1,
         date = glue::glue("{year}-{month}-{day}"),
         date = ymd(date),
         no2_diodes_min_month = case_when(no2_diodes_min_month == Inf ~ NA_real_,
                                          no2_diodes_count <= 14 ~ NA_real_,
                                          TRUE ~ no2_diodes_min_month)) %>% 
  ungroup() %>% 
  select(date,everything(),-c(month,year,day))


# Looking at higher nox in 2024? ------------------------------------------

#read in merge to see what the wind data looks like
cv_merge = read.csv("~/Cape Verde/20240507_CV_merge.csv") %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date,"1 hour")) %>% 
  select(date:year,ws,wd)

nox_hourly_met = nox_hourly %>% 
  left_join(cv_merge,by = "date") %>% 
  mutate(local_pollution = case_when(ws < 2 ~ 1,
                                     wd > 100 & wd < 340 ~ 1,
                                     TRUE ~ 0),
         no2_clean = ifelse(local_pollution == 0
                            & no2_diodes < 200 & no2_diodes > - 5,
                            no2_diodes,NA_real_)) %>% 
  filter(date < "2024-10-01")


nox_hourly_met %>% 
  timeAverage("1 day") %>% 
  ggplot(aes(date,no2_blc_corrected)) +
  geom_path()

nox_monthly_mean = nox %>% timeAverage("1 month",na.rm = F)
nox_monthly_sd = nox %>% timeAverage("1 month",na.rm = F,statistic = sd)
nox_monthly = nox_monthly_sd %>% 
  rename_with(.fn = function(.x){paste0(.x,"_sd")},
                             .cols = -date) %>% 
  left_join(nox_monthly_mean,by = "date")

nox_monthly %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  # pivot_longer(c(no2_blc_corrected,no2_diodes,no_corrected)) %>% 
  ggplot(aes(doy,no2_diodes,col = as.character(year))) +
  theme_bw() +
  geom_ribbon(aes(ymin = no2_diodes - no2_diodes_sd,ymax = no2_diodes + no2_diodes_sd,
                  fill = as.character(year)),alpha = 0.2) +
  geom_path(linewidth = 1) +
  theme(legend.position = "top") +
  labs(x = NULL,
       y = expression(NO[2]~BLC~(ppt)),
       col = NULL) +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  scale_x_continuous(breaks = c(1, 32, 60,91,121,152,182,213,244,274,305,335),
                     labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  # facet_grid(rows = vars(year),scales = "free") +
  NULL

myOutput = timeVariation(nox_with_min,pollutant = c("no_corrected"),group = "year")

dat_uncorr = myOutput$data$month%>% 
  select(mnth,no2_uncorr = Mean,variable) %>% 
  ungroup()
dat_no = myOutput$data$month%>% 
  select(mnth,no = Mean,variable) %>% 
  ungroup()

dat_corr = myOutput$data$month %>% 
  select(mnth,no2_corr = Mean,variable) %>% 
  ungroup()

dat_both = dat_uncorr %>% left_join(dat_no,by = c("mnth","variable"))

dat_both %>%
  rename(`NO[2]~uncorrected` = no2_uncorr,
         NO = no) %>% 
  pivot_longer(c(`NO[2]~uncorrected`,NO)) %>% 
  ggplot(aes(mnth,value,col = variable)) +
  theme_bw() +
  geom_path(linewidth = 1) +
  facet_grid(rows = vars(name),labeller = label_parsed,scales = "free_y") +
  theme(legend.position = "top",
        text = element_text(size = 20)) +
  labs(x = NULL,
       y = expression(NO[x]~(ppt)),
       col = NULL) +
  scale_colour_viridis_d() +
  # scale_colour_manual(values = c("darkseagreen4","darkolivegreen","springgreen4","navy","steelblue1","firebrick","darkorange","goldenrod1")) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

ggsave('nox_monthly_means.png',
       path = "~/Writing/Thesis/Chapter 2 (NOx processing)/Images",
       width = 29,
       height = 14,
       units = 'cm')
