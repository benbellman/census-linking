library(here)
library(rio)
library(tibble)
library(dplyr)

old <- import(here("data", "analysis", "links-full-info-2.csv")) %>% 
  select(uniqueid1, ed_mean_sei1) %>% 
  rename(ed_sei_z = ed_mean_sei1)

#old <- select(old, year1, ed1, ed_pct_black1, ed_pct_frnbrn1, ed_mean_sei1) %>% unique()

new <- import(here("data", "analysis", "links-full-info-3.csv")) %>% 
  select(uniqueid1, ed_mean_sei1) %>% 
  rename(ed_sei_nz = ed_mean_sei1)

both <- inner_join(old, new) %>% 
  filter(is.na(both$ed_sei_nz) == F)

cor(both$ed_sei_z, both$ed_sei_nz)

