library(here)
library(rio)
library(sf)
library(ggplot2)
library(dplyr)
library(purrr)
library(ghibli)


#source(here("R", "functions", "all_row_combos.R"))
#source(here("R", "functions", "aggregate_microdata.R"))


#### Load all data ####

# locational attainment linked sample
# drop sei == 0, non-household heads, and people who stayed at same address
linked <- import(here("data", "for_models", "phl_loc_attain.csv")) %>% 
  as_tibble() %>% 
  filter(hh_max_sei1 > 0 & not_hhh1 == 0) %>% 
  mutate(ed1 = as.character(ed1), ed2 = as.character(ed2))

# load and merge ed custom classes
ed_classes <- import(here("data", "ed_data", "custom_classes.csv")) %>% as_tibble()
ed_classes$custom <- factor(ed_classes$custom, levels = c("Black", "Mixed - Imm.","Mixed - U.S.", "Immigrant", "U.S. White"))

eds <- left_join(eds, ed_classes)

# create ED centroids and merge to linked data
ed_cents1 <- eds %>% 
  filter(year %in% c(1910, 1920, 1930)) %>% 
  select(-custom) %>% 
  rename(ed1 = ed, year1 = year)

ed_cents1 <- bind_cols(
  as_tibble(st_drop_geometry(ed_cents1)),
  as_tibble(st_coordinates(st_centroid(ed_cents1)))
) %>% 
  rename(x1 = X, y1 = Y)

ed_cents2 <- eds %>% 
  filter(year %in% c(1920, 1930, 1940)) %>% 
  select(-custom) %>% 
  rename(ed2 = ed, year2 = year)

ed_cents2 <- bind_cols(
  as_tibble(st_drop_geometry(ed_cents2)),
  as_tibble(st_coordinates(st_centroid(ed_cents2)))
) %>% 
  rename(x2 = X, y2 = Y)

linked <- linked %>% 
  left_join(ed_cents1) %>% 
  left_join(ed_cents2)

# merge ed classes to linked data
linked <- linked %>% 
  left_join(filter(ed_classes, year %in% c(1910, 1920, 1930)) %>% rename(ed1 = ed, year1 = year, custom1 = custom)) %>% 
  left_join(filter(ed_classes, year %in% c(1920, 1930, 1940)) %>% rename(ed2 = ed, year2 = year, custom2 = custom))

# merge distances of moves
move_dist <- list(
  here("data", "ed_distances_10_20.csv"),
  here("data", "ed_distances_20_30.csv"),
  here("data", "ed_distances_30_40.csv")
) %>% 
  map(import)

move_dist[[1]]$year1 <- 1910
move_dist[[2]]$year1 <- 1920
move_dist[[3]]$year1 <- 1930

move_dist <- move_dist %>% 
  map(mutate, ed1 = as.character(ed1), ed2 = as.character(dest_ed)) %>% 
  map(select, -dest_ed) %>% 
  bind_rows() %>% 
  group_by(year1, ed1, ed2) %>% 
  summarise(dist = mean(dist)) %>% 
  ungroup()

linked <- left_join(linked, move_dist)