library(tidyverse)
library(zoo)
library(openair)
library(janitor)

Sys.setenv(TZ = "UTC")
setwd("D:/Documents/Cape Verde")

#need to save o3 corrected no2 minima offset corrected data and read it in here

# Reading in data ---------------------------------------------------------

nox_dat_corr = corr_calc_dat %>%
  select(date,no_corr,no2_corr = no2_corr_o3_corr)

met_data = read.csv("20240507_CV_merge.csv") %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date > "2016-12-31 23:59",
         date < "2024-11-01") %>% 
  clean_names() %>% 
  select(date,sahara:south_atlantic,ws:rh_10m,o3_ppb_v,co_ppb_v,ch4_all_ppb_v:co2_with_mpi_flasks_ppm_v,
         jno2_calc,jo1d_calc,j_o1d,j_no2)

dat = left_join(nox_dat_corr,met_data,by = "date") %>% 
  mutate(local_pollution = case_when(wd > 100 & wd < 340 | ws < 2 ~ 1,
                                     TRUE ~ 0),
         african = central_africa + sahel + west_africa + sahara,
         air_masses = case_when(african >= 10 ~ "African",
                                europe >= 10 ~ "European aged pollution",
                                north_america >= 10 ~ "North American aged pollution",
                                upwelling > 0 & african > 0 ~ "Coastal African",
                                south_atlantic > 0 | south_america > 0 ~ "Southern hemisphere",
                                north_atlantic >= 95 | north_atlantic >= 90 & upwelling > 0 ~ "Clean North Atlantic",
                                TRUE ~ "Other"))

air_masses2 = dat %>% 
  select(date,sahara:wd) %>% 
  timeAverage("1 day") %>% 
  mutate(local_pollution = case_when(wd > 100 & wd < 340 | ws < 2 ~ 1,
                                     TRUE ~ 0),
         african = central_africa + sahel + west_africa + sahara,
         air_masses = case_when(african >= 10 ~ "African",
                                europe >= 10 ~ "European aged pollution",
                                north_america >= 10 ~ "North American aged pollution",
                                upwelling > 0 & african > 0 ~ "Coastal African",
                                south_atlantic > 0 | south_america > 0 ~ "Southern hemisphere",
                                north_atlantic >= 95 | north_atlantic >= 90 & upwelling > 0 ~ "Clean North Atlantic",
                                TRUE ~ "Other"),
         year = year(date))


# Exploratory plots? ------------------------------------------------------

air_masses2 %>% 
  filter(local_pollution == 0,
         is.na(north_atlantic) == F,
         year >= 2021) %>%
  # pivot_longer(c(sahara:south_atlantic)) %>% 
  ggplot(aes(air_masses)) +
  geom_histogram(stat = "count") +
  # scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
  # scale_fill_viridis_d() +
  # facet_wrap(~name) + 
  NULL

air_masses2 %>% 
  pivot_longer(cols = c(african,upwelling:south_atlantic)) %>% 
  mutate(value = ifelse(air_masses == "Other",value,NA_real_),
         value = ifelse(local_pollution == 0,value,NA_real_)) %>% 
  mutate(doy = yday(date)) %>% 
  filter(year >= 2021) %>% 
  ggplot(aes(doy,value,fill = name)) +
  theme_bw() +
  geom_area() +
  facet_grid(rows = vars(year)) +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,0.1,0.1, 0.1), "cm"),
        text = element_text(size = 16)
  ) +
  labs(x = NULL,
       y = "Air mass composition (%)",
       col = NULL,
       fill = NULL) +
  scale_x_continuous(breaks = c(1, 32, 60,91,121,152,182,213,244,274,305,335),
                     labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_fill_manual(values = c("north_atlantic" = "navy",
                               "south_atlantic" = "steelblue1",
                               "african" = "goldenrod1",
                               "south_america" = "darkseagreen1",
                               "north_america" = "springgreen4",
                               "europe" = "darkolivegreen3",
                               "upwelling" = "deepskyblue3"))
