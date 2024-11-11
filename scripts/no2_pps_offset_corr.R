

# Functions ---------------------------------------------------------------


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

# Getting data ------------------------------------------------------------


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

cvao_dat = read.csv("~/Cape Verde/20240507_CV_merge.csv") %>% 
  clean_names() %>% 
  # filter(year == 2022) %>% 
  mutate(date = ymd_hms(date),
         temp_k = temp_10m_deg_c + 273.15,
         k = 1.4 * 10^-12 * exp(-1310/temp_k)) %>% 
  rename_with(~str_remove(.,".v$")) %>% 
  select(date,sahara:south_atlantic,co_ppb,ch4_ppb = ch4_revised_ppb,co2_ppm = co2_revised_ppm,
         jno2_calc,j_no2,temp_10m_deg_c,temp_k,k) %>% 
  left_join(dat_corr_flagged,by = "date") %>% 
  rename(no_ppt = no_corr,
         no2_uncorrected = no2_o3_corr,
         no2_corrected = no2_corr_o3_corr) %>% 
  select(-c(no:no2_diodes_min,diff))


# PSS ---------------------------------------------------------------------

cvao_pss = cvao_dat %>% 
  mutate(o3_molecule_cm3 = ppt_to_molecules_cm3(o3_ppb * 1000),
         hour = hour(date),
         no_molecule_cm3 = ppt_to_molecules_cm3(no_ppt),
         no2_molecule_cm3 = ppt_to_molecules_cm3(no2_uncorrected),
         no2_molecule_cm3_corr = ppt_to_molecules_cm3(no2_corrected),
         no2_lifetime = (1/j_no2)/60) %>% 
  filter(hour >= 11 & hour <= 15 ) %>% 
  mutate(no2_pss = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3*k)/j_no2),
         # no2_pps_298 = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3*k_298)/j_no2),
         leighton_ratio = (j_no2*no2_molecule_cm3)/(k*o3_molecule_cm3*no_molecule_cm3),
         year = year(date)) %>% 
  filter(year == 2022)

#no2 obs vs no2 pps
cvao_pss %>% 
  mutate(year = year(date)) %>% 
  # filter(year == 2022) %>%
  pivot_longer(c(no2_uncorrected,no2_corrected)) %>% 
  ggplot(aes(value,no2_pss)) +
  theme_bw() +
  # geom_point(aes(no2_uncorrected,no2_pss)) +
  geom_point() +
  xlim(0,60) +
  ylim(0,60) +
  labs(x = expression(NO[2~Obs]~(ppt)),
       y = expression(NO[2~PSS]~(ppt))) +
  geom_abline(slope = 1,intercept = 0,col = "steelblue1",size = 1) +
    facet_grid(cols = vars(name)) +
  geom_abline(slope = 0.4,intercept = -1.87,col = "darkorange",size = 1) +
  NULL

model = lm(no2_pss ~ no2_uncorrected,cvao_pss)
summary(model)

ggsave("cvao_no2_pss.png",
       path = "~/csiro/output/cvao_no2_offsets",
       height = 12,
       width = 30,
       units = "cm")
