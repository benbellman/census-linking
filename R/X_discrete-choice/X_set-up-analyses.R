source(here("R", "functions", "mapping", "flow_map_by_race.R"))

# testing contemporary philly border
library(tigris)
library(sf)
library(dplyr)

phl_boundary <- counties(42) %>% 
  st_as_sf() %>% 
  filter(NAME == "Philadelphia")
  
phl_boundary <- st_as_sf(phl_boundary)
phl_boundary <- filter(phl_boundary, NAME == "Philadelphia")
