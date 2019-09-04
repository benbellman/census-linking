library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(rio)
library(here)
library(sf)
library(tmap)
library(ggmap)
library(tidyr)
library(gifski)

source(here("R", "functions", "mapping", "flow_map_by_race.R"))

#### Prepare Data ####

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


# load 1920 polygons
phl_ed_t1 <- st_read(here("data", "merged_eds", paste0("ed_19", t1, ".shp")))
phl_ed_t2 <- st_read(here("data", "merged_eds", paste0("ed_19", t1, ".shp")))

# origin EDs for each animation
south <- st_read(here("data", "merged_eds", "south-philly-20.shp"))
north <- st_read(here("data", "merged_eds", "north-philly-20.shp"))
west <- st_read(here("data", "merged_eds", "west-philly-20.shp"))

# create ED flows from linked data
flows <- linked %>% 
  filter(black == 1 | white == 1) %>% 
  group_by(ed1, ed2) %>% 
  summarize(count = n()) %>% 
  ungroup()

race_flows <- linked %>% 
  filter(black == 1 | white == 1) %>% 
  group_by(ed1, ed2, black) %>% 
  summarize(count = n()) %>% 
  ungroup()

# get centroid coordinates from ED polygons
ed1_cent <- st_centroid(phl_ed_t1)
ed1_cent$lon1 <- st_coordinates(ed1_cent)[,1]
ed1_cent$lat1 <- st_coordinates(ed1_cent)[,2]
ed1_cent <- ed1_cent %>% 
  as_tibble() %>%
  select(-geometry) %>% 
  rename(ed1 = ED)

ed2_cent <- st_centroid(phl_ed_t2)
ed2_cent$lon2 <- st_coordinates(ed2_cent)[,1]
ed2_cent$lat2 <- st_coordinates(ed2_cent)[,2]
ed2_cent <- ed2_cent %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  rename(ed2 = ED)

# attach centroid coordinates to ED migration flow data
coords1 <- select(ed1_cent, ed1, pct_blc, lon1, lat1) %>% 
  rename(pblk1 = pct_blc)

coords2 <- select(ed2_cent, ed2, pct_blc, lon2, lat2) %>% 
  rename(pblk2 = pct_blc)
  

race_flows_coord <- full_join(race_flows, coords1) %>% 
  full_join(coords2) %>% 
  group_by(ed1) %>% 
  mutate(pct_in_ed1 = count / sum(count) * 100) %>% 
  ungroup()

#### South Philly Animation ####

south_flows <- race_flows_coord %>% 
  filter(ed1 %in% south$ED) %>% 
  arrange(pblk1, ed1)

# create animation of race flow maps along eds
flow_map_by_race(south_flows, phl_ed_t1) + 
  transition_states(ed1,
                    transition_length = 5,
                    state_length = 1)

