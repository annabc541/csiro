library(tidyverse)
library(lubridate)
library(openair)

Sys.setenv(TZ = 'UTC')

#calculating what the artifact from HONO would be using higher values that were measured in Feb 2023
#method and CE for HONO in NO2 converter from Simone's ACP paper

#found HONO's absoprtion cross section

hono_acs = read.csv("~/Cape Verde/photolytic_interferences/hono_acs.csv")
hono_acs_filtered = hono_acs %>%
  mutate(absorption_cross_section = absorption_cross_section * 10^20,
         interval = floor(wavelength/5)*5) %>% 
  group_by(interval) %>% 
  summarise(mean_acs = mean(absorption_cross_section))
hono_acs_filtered2 = hono_acs %>%
  mutate(absorption_cross_section = absorption_cross_section * 10^20,
         interval = ceiling(wavelength/5)*5) %>% 
  group_by(interval) %>% 
  summarise(mean_acs = mean(absorption_cross_section))

hono_acs %>% 
  # filter(wavelength > 385 & wavelength < 400) %>% 
  ggplot(aes(wavelength,absorption_cross_section)) +
  geom_path()

hono = read.csv("~/Cape Verde/peroxy_campaign/output/data/hono23_hourly_utc.csv") %>% 
  mutate(date = ymd_hms(date),
         hour = hour(date))

midday = hono %>% 
  filter(hour >= 12 & hour <= 15) %>% 
  timeAverage("1 hour")

midday_hono = mean(midday$hono,na.rm = TRUE)
midday_hono_max = max(midday$hono,na.rm = TRUE)
midday_hono_min = min(midday$hono,na.rm = TRUE)
ce_low = 6.3/100
ce_high = 11.3/100

artifact_max_low = midday_hono_max * ce_low *2 #hono photolises as NO, which is detected as 2 NO2 if NO2 ce = 50%
artifact_min_low = midday_hono_min * ce_low *2

artifact_max_high = midday_hono_max * ce_high *2 #hono photolises as NO, which is detected as 2 NO2 if NO2 ce = 50%
artifact_min_high = midday_hono_min * ce_high *2
