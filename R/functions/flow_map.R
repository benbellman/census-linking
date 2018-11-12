### This is a function that generates flow maps of residential moves

# args:

# t1 = decade of 1900 that is initial time point

# unit = spatial unit user wishes to select movers by

# id = ID (or, eventually, vector of IDs) of spatial unit of movers to map

# map_type = two versions for now: 
#   1. "cluster": a basic cluster of flows based on destination coords
#   2. "race": a comparison of black and white movers from same origin

library(dplyr)
library(ggplot2)
library(ggthemes)
library(rio)
library(here)
library(sf)
library(tmap)
library(ggmap)
library(mclust)
library(tidyr)

for(a in list.files(here("R", "functions"), full.names = T)){
  source(a)
}


flow_map <- function(t1, unit, id, map_type){
  
  # return error if map_type is not supported
  if (map_type %in% c("cluster", "race") == F) stop('map_type must be one of: "cluster", "race"')
  
  # define time 2 
  t2 <- t1 + 10
  
  # load linked sample
  linked <- import(here("data", "linked", paste0("linked_", t1, "_", t2, ".csv"))) %>% 
    as_tibble() %>% 
    mutate(enumdist1 = as.character(enumdist1),
           enumdist2 = as.character(enumdist2),
           black = if_else(race_grp1 == "Black" | race_grp2 == "Black", 1, 0),
           white = if_else(race_grp1 == "White" & race_grp2 == "White", 1, 0))
  
  # get ED data
  ed1_data <- aggregate_microdata(t1, enumdist) %>% rename(ED = enumdist) %>% mutate(ED = as.character(ED))
  ed2_data <- aggregate_microdata(t2, enumdist) %>% rename(ED = enumdist) %>% mutate(ED = as.character(ED))
  
  # load 1910 and 1920 ED polyogons
  ed1_poly <- st_read(here("data", "shertzer_eds", paste0("Philadelphia_19", t1, ".shp"))) %>% 
    st_transform(4326) %>% 
    mutate(ED = as.character(ED))
  ed2_poly <- st_read(here("data", "shertzer_eds", paste0("Philadelphia_19", t2, ".shp"))) %>% 
    st_transform(4326) %>% 
    mutate(ED = as.character(ED))
  
  # merge together
  ed1 <- merge(ed1_poly, ed1_data)
  ed2 <- merge(ed2_poly, ed2_data)
  
  ### use map_type to run correct map module
  if (map_type == "cluster"){
    flow_map_cluster()
  }
  
  if (map_type == "race"){
    flow_map_race()
  }
}



### Actually, I can't really do this right until I have my geocoded points

### SO LET'S GET THAT SHIT DONNNNNNNE

# defining the cluster map module
flow_map_cluster <- function(linked){
  
  # count up flows between ED numbers
  flows <- linked %>% 
    filter(black == 1 | white == 1) %>% 
    group_by(enumdist1, enumdist2) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  
}

flow_map_race <- function(linked){
  
  flows <- linked %>% 
    filter(black == 1 | white == 1) %>% 
    group_by(enumdist1, enumdist2, black) %>% 
    summarize(count = n()) %>% 
    ungroup()
  
  
}

