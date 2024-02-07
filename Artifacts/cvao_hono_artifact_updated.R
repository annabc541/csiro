library(tidyverse)
library(lubridate)
library(openair)

Sys.setenv(TZ = 'UTC')

#calculating what the artifact from HONO would be using higher values that were measured in Feb 2023
#method and CE for HONO in NO2 converter from Simone's ACP paper

hono = read.csv("~/Cape Verde/peroxy_campaign/output/data/processed_in_r4.csv") %>% 
  mutate(date = ymd_hms(date),
         hour = hour(date))

midday = hono %>% 
  filter(hour >= 12 & hour <= 15) %>% 
  timeAverage("1 hour")

midday_hono = mean(midday$hono,na.rm = TRUE)
midday_hono_max = max(midday$hono,na.rm = TRUE)
midday_hono_min = min(midday$hono,na.rm = TRUE)
ce = 6.3/100

artifact_max = midday_hono_max * ce *2 #hono photolises as NO, which is detected as 2 NO2 if NO2 ce = 50%
artifact_min = midday_hono_min * ce *2
