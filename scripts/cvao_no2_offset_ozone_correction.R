library(tidyverse)
library(zoo)

Sys.setenv(TZ = "UTC")
setwd("~/Cape Verde/nox")

#for ozone correcting (and flagging) hourly data that I've applied no2 diode monthly minimum correction to

# Reading in data ---------------------------------------------------------

nox = read.csv("cvao_nox_corr.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  rename(ce = CE_diode)

met_data = read.csv("~/Cape Verde/20240507_CV_merge.csv") %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date > "2016-12-31 23:59",
         date < "2024-11-01") %>% 
  select(date,ws,wd,o3_ppb = O3_ppbV)

# Ozone correction --------------------------------------------------------

#NO can react with ozone in the line to give NO2, but is then not photolysed back to NO
#-> measured NO not representative of what's at the inlet
#NO2 produced in the line (see above), as well as by ozone and NO reaction inside converter
#-> measured NO2 not representative of what's at the inlet

tc = 1 #time in converter 1s
t = 4.3 #time in sample line (between inlet and reaction cell) 4.3s
k = 1.8*10^-14 #rate constant for NO + O3 -> NO2 + O2

corr_calc_dat = met_data %>%
  left_join(nox) %>%
  fill(ce,.direction = "updown") %>% 
  #calculations for NO and NO2 ozone correction (derivations in SI of Andersen et al. 2021)
  mutate(ko3 = (1.8*10^-14)*o3_ppb*(10^-9)*(2.48*10^19), #ko3 = kno+o3 * o3 (ppb), kno+o3 = 1.8*10^-14, 10^-9 and 2.48*10^-19 for units
         no_corr = no * exp(ko3 * 4.3), #4.3 is the time the sample gas spends in the sample line
         j = -log(1 - ce)/tc, #j = -ln(1-ce)/time_in_converter
         no2_corr_o3_corr = ((j + ko3)/j) *
           (((no + no2_corr * ce) - no * exp(-j)) /
              (1-exp(-ko3 - j))) - no_corr,
         no2_o3_corr = ((j + ko3)/j) *
           (((no + no2_uncorr * ce) - no * exp(-j)) /
              (1-exp(-ko3 - j))) - no_corr) %>% 
  select(-c(ce,ko3,j))

dat_corr_flagged = corr_calc_dat %>% 
  mutate(local_pollution = case_when(wd > 100 & wd < 340 | ws < 2 ~ 1,
                             TRUE ~ 0),
         diff = abs(no2_uncorr-no2_corr))

dat_corr_flagged %>%
  timeAverage("1 day") %>% 
  pivot_longer(c(no2_corr,no2_corr_o3_corr)) %>%
  mutate(year = year(date),
         value = ifelse(local_pollution == 0,value,NA_real_)) %>%
  ggplot(aes(date,value,col = name)) +
  geom_path() +
  # facet_grid(rows = vars(name),scales = "free") +
  ylim(-30,200)


myOutput = timeVariation(dat_corr_flagged,pollutant = c("diff"),group = "year")

dat = myOutput$data$month

dat %>% 
  ungroup() %>% 
  ggplot(aes(mnth,Mean,col = variable)) +
  theme_bw() +
  geom_path(linewidth = 1) +
  theme(legend.position = "top") +
  labs(x = NULL,
       y = expression(NO[2]~diodes~(ppt)),
       col = NULL) +
  scale_colour_viridis_d() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

ggsave("no2_corr_diff.png",
       path = "~/csiro/output/cvao_no2_offsets",
       height = 12,
       width = 30,
       units = "cm")
