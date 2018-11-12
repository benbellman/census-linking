library(tidyverse)
library(rio)
library(here)
library(sf)
library(tmap)
library(ggmap)

for(a in list.files(here("R", "functions"), full.names = T)){
  source(a)
}

## This script has my very first attempt at locational attaimnent models,
## uses first version of 1910/1920 links, no other years finished yet
## No spatial information to use (not ED maps or geocoded addresses)

#linked <- load_linked_sample(10, 20)

linked <- import(here("data", "linked", "linked_10_20.csv")) %>% as_tibble()

# get ED data
ed10_data <- aggregate_microdata(10, enumdist) %>% rename(ED = enumdist)
ed20_data <- aggregate_microdata(20, enumdist) %>% rename(ED = enumdist)

# load 1910 and 1920 ED polyogons
ed10_poly <- st_read(here("data", "shertzer_eds", "Philadelphia_1910.shp"))
ed20_poly <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp"))

# merge together
ed10 <- merge(ed10_poly, ed10_data)
ed20 <- merge(ed20_poly, ed20_data)

tm_shape(ed10) +
  tm_polygons("pct_black", border.alpha = 0)

tm_shape(ed20) +
  tm_polygons("pct_black")

# Let's look at how the match sample is spatially distributed in both years
ed10 <- linked %>% 
  mutate(ED = factor(enumdist1)) %>% 
  group_by(ED) %>% 
  summarize(n_linked = n()) %>% 
  merge(ed10, .) %>% 
  mutate(pct_linked = n_linked / total_pop * 100)

ed20 <- linked %>% 
  mutate(ED = factor(enumdist2)) %>% 
  group_by(ED) %>% 
  summarize(n_linked = n()) %>% 
  merge(ed20, .) %>% 
  mutate(pct_linked = n_linked / total_pop *100)

tm_shape(ed10) +
  tm_polygons("n_linked", border.alpha = 0)

### doing an interesting map to put on twitter

# check dist of % black, let's use a 15% cut-off
ggplot(ed10) + stat_bin(aes(x = pct_black))

blk_15_up <- ed10 %>% 
  filter(pct_black > 15) %>% 
  st_union()

tm_shape(ed10) +
  tm_polygons("pct_linked", 
              border.alpha = 0, 
              title = "% linked\nto 1920") +
  tm_shape(blk_15_up) +
  tm_polygons(title = "> 15% black",
              lwd = 2,
              alpha = 0,
              border.col = "black") +
  tm_legend(legend.position = c("right", "bottom"),
            legend.title.size = 1) +
  tm_add_legend(type = "fill",
                labels = ">15% black",
                col = "white",
                #alpha = 0,
                border.lwd = 2,
                border.col = "black") +
  tm_layout(title = "Philadelphia enumeration districts in 1910",
            frame = F,
            title.size = 1.5) +
  tm_credits("Map and analysis by Benjamin Bellman\nSources: IPUMS, Allison Shertzer", position=c("left", "top"))

###

# need to dig into defining # classes and bin widths for maps

tm_shape(ed20) +
  tm_polygons("n_linked", border.alpha = 0)

tm_shape(ed20) +
  tm_polygons("pct_linked", border.alpha = 0)


tm_shape(ed20) +
  tm_polygons("mean_age", border.alpha = 0)


### Let's make a map of ED-to-ED tranisiton flows using centroids
# Size of flow is thickness/color of arrow
# Maybe filter out smaller distances
# Do for a single ED or bundle of local EDs
# or only include an arrow if it's a major contributor to patterns
# OR run a cluster algorithm to combine similar arrows as a way to simplify the viz

flows <- linked %>% 
  group_by(enumdist1, enumdist2) %>% 
  summarize(count = n())

# get centroid coordinates and combine with counts for migration
# add variable that's tracks proportion within 1910 ED

ed10_cent <- st_centroid(ed1_poly) %>% 
  st_transform(4326) %>% 
  st_coordinates() %>% 
  cbind(ed10_poly$ED) %>% 
  as_tibble() %>% 
  rename(lon1 = X,
         lat1 = Y,
         enumdist1 = V1)

ed20_cent <- st_centroid(ed20_poly) %>% 
  st_transform(4326) %>% 
  st_coordinates() %>% 
  cbind(ed20_poly$ED) %>% 
  as_tibble() %>% 
  rename(lon2 = X,
         lat2 = Y,
         enumdist2 = V1)

flows_coord <- full_join(flows, ed10_cent) %>% 
  full_join(ed20_cent) %>% 
  group_by(enumdist1) %>% 
  mutate(pct_in_ed1 = count / sum(count) * 100) %>% 
  ungroup()

## Make a map for a single ED (try geom_curve next)

#tiles <- get_map("philadelphia, pa", zoom = 10, maptype = "toner-lite", source = "stamen")


flows_coord %>% 
filter(enumdist1 == 695) %>% 
ggplot() +
  #geom_sf(aes(data = ), col = grey, size = 0.5, alpha = 0) +
  geom_segment(aes(x = lon1, y = lat1, xend = lon2, yend = lat2, size = pct_in_ed1),
               arrow = arrow(length = unit(0.25, "cm")))


## Make a map for flows larger than __