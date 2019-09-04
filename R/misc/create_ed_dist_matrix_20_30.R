library(dplyr)
library(sf)
library(furrr)
library(here)

# load 1920 and 1930 ED polyogons with UTM (18N), merge with tabular data
ed1_poly <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp")) %>% 
  st_transform(26918) %>% 
  mutate(ed1 = as.character(ED)) %>% 
  select(-ED)

ed2_poly <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp")) %>% 
  st_transform(26918) %>% 
  mutate(dest_ed = as.character(ED)) %>% 
  select(-ED)

# First make a two-col table of all ed1 and e2 combinations
dist_matrix <- expand.grid(ed1 = ed1_poly$ed1, dest_ed = ed2_poly$dest_ed)

# write a function to compute the distance between ed centroids with sf polygons and ED# as inputs
calc_ed_dist <- function(ed1_id, ed2_id, ed1, ed2){
  st_distance(st_centroid(filter(ed1, ed1 == ed1_id)), st_centroid(filter(ed2, dest_ed == ed2_id)))
}

# compute all distances for distance matrix
dist_matrix$dist <- suppressWarnings(future_map2_dbl(dist_matrix$ed1, dist_matrix$dest_ed, calc_ed_dist, ed1_poly, ed2_poly))

# export distance matrix
write_csv(dist_matrix, here("data", "ed_distances_20_30.csv"))


