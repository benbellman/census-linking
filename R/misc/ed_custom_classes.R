library(here)
library(sf)
library(dplyr)
library(readr)

# load and combine all four ED files
ed10 <- st_read(here("data", "merged_eds", "Philadelphia_1910.shp"))
ed20 <- st_read(here("data", "merged_eds", "Philadelphia_1920.shp"))
ed30 <- st_read(here("data", "merged_eds", "Philadelphia_1930.shp"))
ed40 <- st_read(here("data", "merged_eds", "Philadelphia_1940.shp")) %>% st_transform(crs = st_crs(ed30))
ed10$year <- 1910
ed20$year <- 1920
ed30$year <- 1930
ed40$year <- 1940

eds <- rbind(ed10, ed20, ed30, ed40) %>% 
  filter(is.na(ed_pct_b) == F | is.na(ed_pct_f) == F)


# set custom class variable
eds$custom <- case_when(
  eds$ed_pct_b >= 25 ~ "Black",
  eds$ed_pct_b < 25 & eds$ed_pct_f >= 25 ~ "Immigrant",
  eds$ed_pct_b < 25 & eds$ed_pct_f < 25 ~ "U.S. White"
)
#eds$custom <- factor(eds$custom, levels = c("Black", "Mixed - Imm.","Mixed - U.S.", "Immigrant", "U.S. White"))


# limit to only year, ED, and class and save as csv
eds %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(ed = as.character(ED)) %>% 
  select(ed, year, custom) %>% 
  write_csv(here("data", "ed_data", "custom_classes.csv"))
