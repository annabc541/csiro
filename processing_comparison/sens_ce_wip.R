library(tidyverse)
library(lubridate)
library(janitor)
library(openair)

setwd("~/cape grim/")

# Functions ---------------------------------------------------------------

perc_diff <- function(new_value,old_value){
  x = abs((new_value - old_value)/old_value)
  
}
# Reading in data ---------------------------------------------------------

kcg_code = read.csv("Analysing KCG data/processed_data/kcg_processing.csv") %>% 
  mutate(date = dmy_hm(DateTime1),
         date = round_date(date,"1 hour")) %>% 
  select(date,everything()) %>% 
  select(date,no = NO,no2 = NO2,ce = Ob_NO2_CE_Cal_AVE,sens = Ob_GAW_Sens_NO_Cal_AVE) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_kcg")},
               .cols = -date)

cvao_code = read.csv("Analysing KCG data/processed_data/sens_ce/cvao_processing.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,no_night = NO_Conc_art_corrected_night,no_pag = NO_Conc_art_corrected_PAG,
         no2 = NO2_Conc_art_corrected,ce = CE,sens = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_og")},
               .cols = -date)

mean_means = read.csv("Analysing KCG data/processed_data/sens_ce/mean_of_mean.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,no_night = NO_Conc_art_corrected_night,no_pag = NO_Conc_art_corrected_PAG,
         no2 = NO2_Conc_art_corrected,ce = CE,sens = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_mean_mean")},
               .cols = -date)

mean_medians = read.csv("Analysing KCG data/processed_data/sens_ce/mean_of_med.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,no_night = NO_Conc_art_corrected_night,no_pag = NO_Conc_art_corrected_PAG,
         no2 = NO2_Conc_art_corrected,ce = CE,sens = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_mean_med")},
               .cols = -date)

mean_all_cal = read.csv("Analysing KCG data/processed_data/sens_ce/mean_whole_cal.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,no_night = NO_Conc_art_corrected_night,no_pag = NO_Conc_art_corrected_PAG,
         no2 = NO2_Conc_art_corrected,ce = CE,sens = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_mean_whole_cal")},
               .cols = -date)

different_zeroes = read.csv("Analysing KCG data/processed_data/sens_ce/NOx_kcg_2022_calc_df.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,no_night = NO_Conc_art_corrected_night,no_pag = NO_Conc_art_corrected_PAG,
         no2 = NO2_Conc_art_corrected,ce = CE,sens = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_zeroes")},
               .cols = -date)

df_list = list(kcg_code,cvao_code,mean_means,mean_all_cal,different_zeroes)

comp = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date)

# Comparing sens and ce ---------------------------------------------------

#comparing data visually
comp %>% 
  rename("KCG code" = ce_kcg,
         "CVAO code" = ce_og,
         "CVAO code adjusted" = ce_zeroes) %>%
  # pivot_longer(c("KCG code","CVAO code mean of means")) %>% 
  pivot_longer(c("KCG code","CVAO code","CVAO code adjusted")) %>%
  filter(value > 0.5) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_point() +
  geom_path(linewidth = 0.8) +
  # facet_grid(rows = vars(name),scales = "free") +
  labs(x = "Datetime",
       y = "CE",
       col = NULL) +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name)) +
  scale_x_datetime(date_breaks = "7 day",date_labels = "%d %b") +
  scale_color_viridis_d() +
  NULL

ggsave("ce_comp.png",
       path = "Analysing KCG data/plots",
       height = 12,
       width = 30,
       units = "cm")

#looking at % difference
comp %>% 
  timeAverage("1 day") %>%
  mutate(diff_sens = perc_diff(sens_kcg,sens_mean_whole_cal),
         diff_ce = perc_diff(ce_kcg,ce_mean_mean)) %>% 
  pivot_longer(c(diff_sens,diff_ce)) %>% 
  # filter(value < 10) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_point() +
  # facet_grid(rows = vars(name),scales = "free") +
  labs(x = "Datetime",
       y = "NO2 percent difference") +
  scale_x_datetime(date_breaks = "7 day",date_labels = "%d %b") +
  NULL


# Looking at ambient data -------------------------------------------------

#comparing data visually
comp %>% 
  rename("KCG code" = no2_kcg,
         # "CVAO code PAG artefact" = no_pag_mean_whole_cal,
         "CVAO code mean of means" = no2_mean_mean,
         "CVAO code mean all cal" = no2_mean_whole_cal,) %>%
  pivot_longer(c("KCG code","CVAO code mean of means","CVAO code mean all cal")) %>%
  # mutate(value = case_when(value > 200 ~ NA_real_,
  #                          value < -100 ~ NA_real_,
  #                          TRUE ~ value)) %>%
  mutate(value = case_when(value > 1000 ~ NA_real_,
                           value < -25 ~ NA_real_,
                           TRUE ~ value)) %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  # geom_point() +
  geom_path(linewidth = 0.8) +
  # facet_grid(rows = vars(name),scales = "free") +
  labs(x = "Datetime",
       y = "NO2 (ppt)",
       col = NULL) +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name)) +
  scale_x_datetime(date_breaks = "7 day",date_labels = "%d %b") +
  scale_color_viridis_d() +
  NULL

ggsave("no2.png",
       path = "Analysing KCG data/plots",
       height = 12,
       width = 30,
       units = "cm")


# Looking at artefacts ----------------------------------------------------

kcg_code = read.csv("Analysing KCG data/processed_data/kcg_processing.csv") %>% 
  mutate(date = dmy_hm(DateTime1),
         date = round_date(date,"1 hour")) %>% 
  select(date,no = NO,no2 = NO2,ce = Ob_NO2_CE_Cal_AVE,sens = Ob_GAW_Sens_NO_Cal_AVE) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_kcg")},
               .cols = -date)

art_no_night_zero = read.csv("Analysing KCG data/processed_data/NO_zero_night2022.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  timeAverage("1 hour") %>% 
  filter(date > "2022-02-01" & date < "2022-03-01") %>% 
  select(date,no_night = NO_Conc_art_corrected_night,no_pag = NO_Conc_art_corrected_PAG,
         no2 = NO2_Conc_art_corrected,ce = CE,sens = SENS) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_new_no_art")},
               .cols = -date)

df_list = list(kcg_code,mean_all_cal,art_no_night_zero)

comp = df_list %>% reduce(full_join,by = "date") %>% 
  arrange(date)

comp %>% 
  rename("KCG code" = no_kcg,
         "CVAO code PAG artefact" = no_pag_mean_whole_cal,
         "CVAO code night NO" = no_night_mean_whole_cal,
         "CVAO code night zero" = no_night_new_no_art,) %>%
  pivot_longer(c("KCG code","CVAO code PAG artefact","CVAO code night NO","CVAO code night zero")) %>%
  mutate(value = case_when(value > 1000 ~ NA_real_,
                           # value < -100 ~ NA_real_,
                           TRUE ~ value)) %>%
  # mutate(value = case_when(value > 1000 ~ NA_real_,
  #                          value < -25 ~ NA_real_,
  #                          TRUE ~ value)) %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_point() +
  # geom_path(linewidth = 0.8) +
  # facet_grid(rows = vars(name),scales = "free") +
  labs(x = "Datetime",
       y = "NO (ppt)",
       col = NULL) +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name)) +
  scale_x_datetime(date_breaks = "7 day",date_labels = "%d %b") +
  scale_color_viridis_d() +
  NULL

ggsave("no2.png",
       path = "Analysing KCG data/plots",
       height = 12,
       width = 30,
       units = "cm")

# NO2 artefacts -----------------------------------------------------------

kcg_code = read.csv("Analysing KCG data/processed_data/kcg_processing.csv") %>% 
  mutate(date = dmy_hm(DateTime1),
         date = round_date(date,"1 hour")) %>% 
  timeAverage("1 day") %>%
  select(date,no = NO,no2 = NO2,ce = Ob_NO2_CE_Cal_AVE,sens = Ob_GAW_Sens_NO_Cal_AVE,
         nox_artefact = Ob_NOx_Artifact_GAW_AVE,za_nox = ZA_NO2,nox_artefact2 = NOXART_GAW_RM7) 
  # rename_with( .fn = function(.x){paste0(.x,"_kcg")},
  #              .cols = -date)

art = read.csv("Analysing KCG data/processed_data/NOx_kcg_2022_art_df.csv") %>% 
  mutate(date = ymd_hms(X)) %>% 
  timeAverage("1 day") %>%
  select(date,PAG_Zero_NO2_Conc,PAG_Zero_NO2_Conc_corr,PAG_Zero_NO2_Conc_mean)

art_comp = kcg_code %>% full_join(art,by = "date")

art_comp %>% 
  arrange(dat) %>% 
  rename("KCG artefact" = nox_artefact,
         "KCG artefact 2" = nox_artefact2,
         "KCG za NOx" = za_nox,
         "CVAO artefact final" = PAG_Zero_NO2_Conc_mean) %>%
  pivot_longer(c("KCG artefact","CVAO artefact final","KCG artefact 2","KCG za NOx")) %>%
  # filter(value > 10) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  geom_point() +
  geom_path()
  # facet_grid(rows = vars(name),scales = "free") +
  labs(x = "Datetime",
       y = "NOx artefact",
       col = NULL) +
  theme(legend.position = "top") +
  # facet_grid(rows = vars(name)) +
  scale_x_datetime(date_breaks = "7 day",date_labels = "%d %b") +
  scale_color_viridis_d() +
  NULL

ggsave("sens_best.png",
       path = "Analysing KCG data/plots/testing_sens_ce",
       height = 12,
       width = 30,
       units = "cm")