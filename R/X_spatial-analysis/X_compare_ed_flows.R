library(rio)
library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(tigris)
library(magrittr)
library(cowplot)
library(useful)
library(purrr)
library(gganimate)
library(gifski)

source(here("R", "functions", "aggregate_microdata.R"))

# set years for time points
t1 <- 20
t2 <- t1 + 10

# load linked data
linked <- import(here("data", "linked", paste0("linked_", t1, "_", t2, ".csv"))) %>% 
  as_tibble() %>% 
  mutate(ed1 = as.character(ed1),
         ed2 = as.character(ed2),
         black = if_else(race_grp1 == "Black" | race_grp2 == "Black", 1, 0),
         white = if_else(race_grp1 == "White" & race_grp2 == "White", 1, 0))


# load ED data
ed1 <- st_read(here("data", "merged_eds", paste0("ed_19", t1, ".shp"))) %>% 
  rename(pct_black = pct_blc,
         pct_frnbrn = pct_frn,
         ed = ED) %>% 
  st_transform(4326)

ed2 <- st_read(here("data", "merged_eds", paste0("ed_19", t1, ".shp"))) %>% 
  rename(pct_black = pct_blc,
         pct_frnbrn = pct_frn,
         ed = ED) %>% 
  st_transform(4326)

# 

# try a map!
bivar_ed_race_flow_map(linked, 50, ed1, ed2)


#### start animation ####

# origin EDs for each animation
south <- st_read(here("data", "merged_eds", "south-philly-20.shp"))
north <- st_read(here("data", "merged_eds", "north-philly-20.shp"))
west <- st_read(here("data", "merged_eds", "west-philly-20.shp"))


