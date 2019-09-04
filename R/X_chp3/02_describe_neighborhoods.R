library(here)
library(sf)
library(dplyr)
library(rio)
library(ggplot2)
library(purrr)

# load ED data and attach neighborhood classes
ed10 <- st_read(here("data", "merged_eds", "Philadelphia_1910.shp"))
ed20 <- st_read(here("data", "merged_eds", "Philadelphia_1920.shp"))
ed30 <- st_read(here("data", "merged_eds", "Philadelphia_1930.shp"))
ed40 <- st_read(here("data", "merged_eds", "Philadelphia_1940.shp")) %>% st_transform(crs = st_crs(ed30))
ed10$year <- 1910
ed20$year <- 1920
ed30$year <- 1930
ed40$year <- 1940

eds <- rbind(ed10, ed20, ed30, ed40) %>% 
  filter(is.na(ed_pct_b) == F | is.na(ed_pct_f) == F) %>% 
  mutate(ed = as.character(ED)) %>% 
  select(-ED)

ed_classes <- import(here("data", "ed_data", "custom_classes.csv"))

eds <- left_join(eds, ed_classes)
