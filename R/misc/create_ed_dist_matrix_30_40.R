library(dplyr)
library(sf)
library(furrr)
library(here)
library(readr)

# load 1920 and 1930 ED polyogons with UTM (18N), merge with tabular data
ed1 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp")) %>% 
  st_transform(26918) %>% 
  mutate(ed1 = as.character(ED)) %>% 
  select(-ED) %>% 
  st_centroid()

ed2 <- st_read(here("data", "shertzer_eds", "Philadelphia_1940.shp")) %>% 
  st_transform(26918) %>% 
  mutate(dest_ed = as.character(ED)) %>% 
  select(-ED) %>% 
  st_centroid()

# First make a two-col table of all ed1 and e2 combinations
dist_matrix <- expand.grid(ed1 = ed1$ed1, dest_ed = ed2$dest_ed)

# write a function to compute the distance between ed centroids with sf polygons and ED# as inputs
calc_ed_dist <- function(ed1_id, ed2_id, ed1, ed2){
  st_distance(filter(ed1, ed1 == ed1_id)[1,], filter(ed2, dest_ed == ed2_id)[1,])
}

# set up multiprocessing
plan(tweak(multiprocess, workers = 16))

# compute all distances for distance matrix
dist_matrix$dist <- suppressWarnings(future_map2_dbl(dist_matrix$ed1, dist_matrix$dest_ed, calc_ed_dist, ed1, ed2))

# export distance matrix
write_csv(dist_matrix, here("data", "ed_distances_30_40.csv"))


