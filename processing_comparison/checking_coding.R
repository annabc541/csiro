library(tidyverse)
library(lubridate)
library(janitor)
library(openair)

setwd("~/cape grim/")

#used for both looking at raw kcg data to understand how nox cycle behaves
#and for comparing different processing codes after they have been run in python

# Functions ---------------------------------------------------------------

perc_diff <- function(new_value,old_value){
  x = abs((new_value - old_value)/old_value)
  
}

# Reading in KCG raw data -------------------------------------------------

#data in tidied dataset
dat_kcg_pre_processed = read.csv("Analysing KCG data/CG_AQDNOx_202202.csv") %>% 
  # clean_names() %>%
  mutate(Timestamp = dmy_hms(Timestamp)) %>% 
  rename(date = Timestamp)

#data in raw daily files
files = list.files("Analysing KCG data/nox_raw_data/raw", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}
dat_kcg_raw = bind_rows(datList) %>% 
  arrange(date)


# Plotting with KCG raw data ----------------------------------------------

dat_kcg_raw %>% 
  filter(zero_valve_1 == 0,
         Inlet_BLC == 1,
         Inlet_NOx == 0,
         Inlet_ZA == 0) %>% 
  ggplot(aes(date,CH1_Hz)) +
  geom_point()

# Looking at data post-python analysis ------------------------------------

cvao_code = read.csv("Analysing KCG data/processed_data/kcg_data_cvao_processing.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,NO_Conc_art_corrected_night,NO_Conc_art_corrected_PAG,NO2_Conc_art_corrected,CE_cvao = CE,sens_cvao = SENS)

sens_eq = read.csv("Analysing KCG data/processed_data/kcg_data_cvao_processing_kcg_sens.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,NO_Conc_art_corrected_night,NO_Conc_art_corrected_PAG,NO2_Conc_art_corrected,CE_cvao = CE,sens_cvao = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_eq")},
               .cols = -date)

m_o_m = read.csv("Analysing KCG data/processed_data/sens_ce/mean_of_mean.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,NO_Conc_art_corrected_night,NO_Conc_art_corrected_PAG,NO2_Conc_art_corrected,CE_cvao = CE,sens_cvao = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_mom")},
               .cols = -date)

other = read.csv("Analysing KCG data/processed_data/NOx_kcg_2022_calc_df.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,NO_Conc_art_corrected_night,NO_Conc_art_corrected_PAG,NO2_Conc_art_corrected,CE_cvao = CE,sens_cvao = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_new")},
               .cols = -date)

med_kcg = read.csv("Analysing KCG data/processed_data/NOx_kcg_2022_calc_df_median.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,NO_Conc_art_corrected_night,NO_Conc_art_corrected_PAG,NO2_Conc_art_corrected,CE_cvao = CE,sens_cvao = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_med")},
               .cols = -date)

# write.csv(dat_kcg_processed_cvao_code,"Analysing KCG data/processed_data/kcg_data_cvao_processing.csv")  
  
kcg_code = read.csv("Analysing KCG data/processed_data/kcg_processing.csv") %>% 
  mutate(date = dmy_hm(DateTime1),
         date = round_date(date,"1 hour")) %>% 
  select(date,NO,NO2,NOx,CE_kcg = Ob_NO2_CE_Cal_AVE,sens_kcg = Ob_GAW_Sens_NO_Cal_AVE)

df_list = list(kcg_code,m_o_m)

comp = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date)

#comparing data visually
comp %>% 
  rename("KCG code" = NO2,
         "CVAO code" = NO2_Conc_art_corrected_mom,
         # "CVAO code eq" = sens_cvao_eq,
         # "CVAO code mom" = sens_cvao_mom,
         # "CVAO code new" = sens_cvao_new,
         # "CVAO code medians" = sens_cvao_med
         ) %>%
  pivot_longer(c("KCG code"
                 ,"CVAO code"
                 )) %>%
  mutate(value = case_when(value > 200 ~ NA_real_,
                           value < -100 ~ NA_real_,
                           TRUE ~ value)) %>%
  # filter(value < 1) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  # geom_point() +
  geom_path(linewidth = 0.8) +
  # facet_grid(rows = vars(name),scales = "free") +
  labs(x = "Datetime",
       y = expression(NO[2]),
       col = NULL) +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name)) +
  scale_x_datetime(date_breaks = "7 day",date_labels = "%d %b") +
  scale_color_viridis_d() +
  NULL

ggsave("no2_comp.png",
       path = "plots/new_comparison",
       height = 12,
       width = 30,
       units = "cm")

#looking at % difference
comp %>% 
  # timeAverage("1 day") %>% 
  rename(kcg = NO,
         cvao_old = NO_Conc_art_corrected_night,
         cvao_new = NO_Conc_art_corrected_night_new) %>%
  mutate(diff_old = perc_diff(kcg,cvao_old),
         diff_new = perc_diff(kcg,cvao_new)) %>% 
  pivot_longer(c(diff_old,diff_new)) %>% 
  filter(value < 10) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_point() +
  # facet_grid(rows = vars(name),scales = "free") +
  labs(x = "Datetime",
       y = "NO2 percent difference") +
  scale_x_datetime(date_breaks = "7 day",date_labels = "%d %b") +
  NULL

ggsave("ce_new_comp.png",
       path = "plots/new_comparison",
       height = 12,
       width = 30,
       units = "cm")

# Cal problem solving -----------------------------------------------------

sample_cal = dat_kcg_raw %>% 
  filter(date > "2022-02-03 03:00" & date < "2022-02-03 06:00",
         NO_cal == 1)

dat_kcg_raw %>% 
  filter(date >"2022-02-03 03:00" & date < "2022-02-03 06:00") %>%
  # filter(Inlet_NOx == 1,
  #        NO_valve == 1,
  #        Inlet_BLC == 0,
  #        NOx_cal == 1,
  #        Inlet_ZA == 0,
  #        zero_valve_1 == 0,
  #        Inlet_SS == 1) %>%
  ggplot(aes(date,CH1_Hz, col = NOx_cal)) +
  geom_point() +
  # scale_x_datetime(date_breaks = "1 day",date_labels = "%d") +
  NULL

dat_kcg_pre_processed %>% 
  filter(date > "2022-02-03" & date < "2022-02-05") %>%
  filter(status == 166) %>% 
  # filter(Inlet_NOx == 1,
  #        # NO_valve == 1,
  #        # Inlet_BLC == 0,
  #        NOx_cal == 1,
  #        # Inlet_ZA == 0,
  #        # zero_valve_1 == 0,
  #        # Inlet_SS == 1
  #        ) %>%
  ggplot(aes(date,CH1_Hz,col = as.character(status))) +
  geom_point() +
  # scale_x_datetime(date_breaks = "1 day",date_labels = "%d") +
  NULL

dat_cvao %>% 
  filter(date > "2022-02-02 08:30" & date < "2022-02-02 09:15") %>% 
  ggplot(aes(date,CH1_Hz,col = NO_valve)) +
  geom_point()


dat_kcg_cal %>% 
  filter(date > "2022-02-01" & date < "2022-02-02") %>%
  ggplot(aes(date,Previous_NO_cycle)) +
  geom_point()

dat_kcg_processed %>% 
  filter(NO_Conc < 5000) %>% 
  # filter(date > "2022-02-01" & date < "2022-02-02") %>%
  ggplot(aes(date,NO_Conc)) +
  geom_point()

dat_cvao_processed %>% 
  # filter(date > "2022-02-02 00:00" & date < "2022-02-02 12:00") %>%
  ggplot(aes(date,NO_Conc)) +
  geom_point()

dat_cvao_cal %>% 
  filter(date > "2022-02-01" & date < "2022-02-03") %>%
  ggplot(aes(date,Previous_NO_cycle)) +
  geom_point()


# Reading in CVAO data ------------------------------------------------

#cape verde raw data - for seeing column names and such
files = list.files("kcg/cvao_feb22", full.names=TRUE)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],header=TRUE,sep = ",", na.strings= c('NA','missing'))%>%
    mutate(TheTime=waclr::parse_excel_date(TheTime)) %>%
    rename(date = TheTime) %>%
    tibble()
  
}
dat_cvao = bind_rows(datList)

dat_cvao_processed = read.csv("~/Cape Verde/nox/processing/data/processed_data/NOx_2022_calc_df.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(date > "2022-02-01" & date < "2022-03-01")

dat_cvao_cal = read.csv("~/Cape Verde/nox/processing/data/processed_data/NOx_2022_cal_df.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(date > "2022-02-01" & date < "2022-03-01")
