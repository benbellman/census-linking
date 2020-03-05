library(here)
library(rio)
library(sf)
library(dplyr)
library(purrr)
library(readr)

# load locational attainment linked sample
linked <- import(here("data", "for_models", "phl_loc_attain.csv")) %>% 
  as_tibble() %>% 
  #filter(sei1 > 0) %>% 
  #filter(hh_max_sei1 > 0) %>% 
  #filter(not_hhh1 == 0) %>% 
  mutate(ed1 = as.character(ed1), ed2 = as.character(ed2))

# ED polygons
ed10 <- st_read(here("data", "shertzer_eds", "Philadelphia_1910.shp"))
ed20 <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp"))
ed30 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp"))
ed40 <- st_read(here("data", "shertzer_eds", "Philadelphia_1940.shp")) %>% st_transform(crs = st_crs(ed30))
ed10$year <- 1910
ed20$year <- 1920
ed30$year <- 1930
ed40$year <- 1940

eds <- rbind(ed10, ed20, ed30, ed40) %>% 
  mutate(ed = as.character(ED)) %>% 
  select(-ED)

# borders of philadelphia
#library(tigris)
#phl <- places(state = 42)
#phl <- st_as_sf(phl)
#phl <- filter(phl, NAME == "Philadelphia")
#phl <- st_transform(phl, crs = st_crs(eds))

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

# save data for use on server
write_csv(linked, here("data", "for_models", "links-no-filters-all-info.csv"))
