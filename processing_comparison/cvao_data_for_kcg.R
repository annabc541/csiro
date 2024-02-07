library(tidyverse)
library(lubridate)
library(openair)
library(janitor)
library(zoo)

Sys.setenv(TZ = 'UTC')

# Making plots for meeting 25/09 ------------------------------------------

#hectic plots thrown together for the Australia meeting 25/09/23

setwd("~/Cape Verde/nox/processing/data/no_pmt_filter")

dat = read.csv("NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") 

art = read.csv("NOx_2023_art_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") 


cal = read.csv("NOx_2023_cal_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("1 hour") 

cal %>% 
  filter(date > "2023-02-02") %>% 
  ggplot(aes(date,PAG_Zero_NO2_Conc_mean)) +
  geom_point() +
  scale_x_datetime(breaks = "2 weeks",date_labels = "%y/%m/%d")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

ggsave('hourly_no2_blc_pag_art.svg',
       path = "output/aussie_meeting",
       width = 30,
       height = 12,
       units = 'cm')

# Reading data from 2022 and making one dataset ---------------------------

setwd("~/Cape Verde/nox/processing/data/processed_data")

dat = read.csv("NOx_2022_calc_df.csv") %>% 
  tibble() %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 sec")) %>% 
  remove_empty() %>% 
  remove_constant() %>%
  arrange(date) %>% 
  timeAverage("5 min") 

#2022 raw data
setwd('D:/Cape Verde/data/nox_raw_data')

files = list.files(pattern = "z_22", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    timeAverage("5 min") %>% 
    tibble()
  
}

raw_dat22 = bind_rows(datList) %>%
  # mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant() 

all_dat = left_join(dat,raw_dat22,by = "date")

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")
write.csv(all_dat,"output/processed_and_raw_data22.csv",row.names =  FALSE)
write.csv(raw_dat22,"output/raw_data22.csv",row.names =  FALSE)

# Finding good month for data comparison excerise -------------------------

setwd("~/Cape Verde/nox/processing/initial_processing/nox_r")

dat22 = read.csv("output/processed_and_raw_data22.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  rename(NO_Conc = NO_Conc.x) %>% 
  select(-c(NO_Conc.y))

dat22 %>% 
  mutate(month = month(date)) %>% 
  filter(month == 2,
         NOx_cal == 0,
         CH1_Hz > 0,
         CH1_zero > 0) %>%
  pivot_longer(c(NO2_Conc_art_corrected,NO2_Conc_diode,CH1_Hz,CH1_zero,NO_Conc_art_corrected)) %>%
  ggplot(aes(date,NO_night_mean)) +
  geom_point() +
  scale_x_datetime(date_breaks = "4 days",date_labels = "%d/%m") +
  # facet_grid(rows = vars(name),scales = "free") +
  scale_colour_viridis_c() +
  NULL

#february 2022 looks like a good month - PAG NO2 artefact is really high, but other than that no problems

# Reading in Feb 22 data --------------------------------------------------

setwd("~/cape grim/kcg/cvao_feb22")

files = list.files()
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    # timeAverage("5 min") %>%
    tibble()
  
}

raw_dat = bind_rows(datList) %>%
  # mutate(date = round_date(date, "1 sec")) %>%
  remove_empty() %>%
  remove_constant() 

raw_dat %>% 
  ggplot(aes(date,CH1_Hz)) +
  geom_point()
