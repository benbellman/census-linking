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
library(tigris)

for(a in list.files(here("R", "functions"), full.names = T)){
  source(a)
}

t1 <- 20
t2 <- t1 + 10

linked <- import(here("data", "linked", paste0("linked_", t1, "_", t2, ".csv"))) %>% 
  as_tibble() %>% 
  mutate(ed1 = as.character(ed1),
         ed2 = as.character(ed2))

# get ED data
ed1_data <- aggregate_microdata(t1, ed) %>% rename(ED = ed) %>% mutate(ED = as.character(ED))
ed2_data <- aggregate_microdata(t2, ed) %>% rename(ED = ed) %>% mutate(ED = as.character(ED))

# load 1910 and 1920 ED polyogons
ed1_poly <- st_read(here("data", "shertzer_eds", paste0("Philadelphia_19", t1, ".shp"))) %>% 
  st_transform(26918) %>% 
  mutate(ED = as.character(ED))
ed2_poly <- st_read(here("data", "shertzer_eds", paste0("Philadelphia_19", t2, ".shp"))) %>% 
  st_transform(26918) %>% 
  mutate(ED = as.character(ED))

# merge together
ed1 <- merge(ed1_poly, ed1_data)
ed2 <- merge(ed2_poly, ed2_data)

# calculate linkage ED variables
ed1 <- linked %>% 
  mutate(ED = ed1) %>% 
  group_by(ED) %>% 
  summarize(n_linked = n()) %>% 
  merge(ed1, .) %>% 
  mutate(pct_linked = n_linked / total_pop * 100)

ed2 <- linked %>% 
  mutate(ED = ed2) %>% 
  group_by(ED) %>% 
  summarize(n_linked = n()) %>% 
  merge(ed2, .) %>% 
  mutate(pct_linked = n_linked / total_pop *100)




tm_shape(ed1) +
  tm_polygons("pct_black", 
              breaks = c(0, 5, 10, 15, 25, 50, 100),
              border.alpha = 0)





ggplot(ed1) + stat_bin(aes(x = pct_black))

blk_15_up <- ed1 %>% 
  filter(pct_black > 15) %>% 
  st_union()

tm_shape(ed1) +
  tm_polygons("pct_linked",
              border.alpha = 0, 
              title = "% linked\nto 1940") +
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



### Make maps of migration flows using stamen web map tiles as basemap (and to define map zoom around ED)

# create black/white dummy vars
linked <- linked %>% 
  mutate(black = if_else(race_grp1 == "Black" | race_grp2 == "Black", 1, 0),
         white = if_else(race_grp1 == "White" & race_grp2 == "White", 1, 0))

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

# get centroid coordinates and combine with counts for migration
# add variable that's tracks proportion within 1910 ED

ed1_cent <- st_centroid(ed1_poly)
ed1_cent$lon1 <- st_coordinates(ed1_cent)[,1]
ed1_cent$lat1 <- st_coordinates(ed1_cent)[,2]
ed1_cent <- ed1_cent %>% 
  as_tibble() %>%
  select(-geometry) %>% 
  rename(ed1 = ED)


ed2_cent <- st_centroid(ed2_poly)
ed2_cent$lon2 <- st_coordinates(ed2_cent)[,1]
ed2_cent$lat2 <- st_coordinates(ed2_cent)[,2]
ed2_cent <- ed2_cent %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  rename(ed2 = ED)

# Load Philly boundary
phl_boundary <- counties(42) %>% 
  st_as_sf() %>% 
  filter(NAME == "Philadelphia")



#ed2_cent <- st_centroid(ed2_poly) %>% 
#  st_coordinates() %>% 
#  cbind(ed2_poly$ED) %>% 
#  as_tibble() %>% 
#  rename(lon2 = X,
#         lat2 = Y,
#         enumdist2 = V1)

flows_coord <- full_join(flows, ed1_cent) %>% 
  full_join(ed2_cent) %>% 
  group_by(ed1) %>% 
  mutate(pct_in_ed1 = count / sum(count) * 100) %>% 
  ungroup()

race_flows_coord <- full_join(race_flows, ed1_cent) %>% 
  full_join(ed2_cent) %>% 
  group_by(ed1) %>% 
  mutate(pct_in_ed1 = count / sum(count) * 100) %>% 
  ungroup()


# as a mapping function
create_flow_map <- function(flows_coord, ed){
  
  ed_flow <- flows_coord %>% 
    filter(ed1 == ed) %>% 
    filter(is.na(lon2) == F & is.na(lat2) == F)
  
  bound <- tibble(lon = c(ed_flow[["lon1"]], ed_flow[["lon2"]]),
                  lat = c(ed_flow[["lat1"]], ed_flow[["lat2"]])) %>% 
    st_as_sf(coords = c("lon", "lat")) %>% 
    st_bbox()
  
  names(bound) <- c("left", "bottom", "right", "top")
  
  ed_base <- get_stamenmap(bound, zoom = 11, maptype = "toner-lite")
  
  # run a clustering algorithm on the geographic cluster of flows
  clust <- Mclust(ed_flow[,c("lon2", "lat2")])
  
  # attach clsuter ids to flows
  ed_flow$class <- clust$classification
  
  # aggregate flow data to clusters
  class_flow <- ed_flow %>% 
    group_by(class) %>% 
    summarise(count = sum(count),
              lon1 = mean(lon1),
              lat1 = mean(lat1),
              lon2 = mean(lon2),
              lat2 = mean(lat2)) %>% 
    ungroup() %>% 
    mutate(total_moves = sum(count),
           pct_in_ed1 = count / total_moves * 100,
           `% of ED` = factor(case_when(pct_in_ed1 <= 10 ~ "< 10",
                                        pct_in_ed1 > 10 & pct_in_ed1 <= 20 ~ "10 to 20",
                                        pct_in_ed1 > 20 ~ "> 20"),
                              levels = c("< 10", "10 to 20", "> 20")))
  
  
  # attach classes to ED centroids to plot
  ed_flow <- left_join(ed_flow, select(class_flow, class, `% of ED`))
  
  ggmap(ed_base) +
    scale_size_manual(values = c(0.3, 0.8, 1.5)) +
    scale_color_hue(guide = FALSE) +
    geom_segment(data = class_flow,
                 aes(x = lon1, y = lat1, 
                     xend = lon2, yend = lat2, 
                     size = `% of ED`, col = factor(class)),
                 arrow = arrow(angle = 40, length = unit(0.25, "cm"))) +
    geom_point(data = ed_flow,
               aes(x = lon2, y = lat2, color = factor(class))) +
    ggtitle(paste0("Residential flows of linked records, 19", t1, " to 19", t2),
            subtitle = paste0("ED ", ed, " in 19", t1," (", sum(class_flow$count), " total links)"))
}


flowmap <- create_flow_map(flows_coord, ed = 120)

flowmap

### I want to change the above map function to do either race groups OR automated clusters

# make modules for each style map
# use inputs that would needed for all kinds of flow maps
# will eventually need to expand this beyond EDs, and create system for all kinds of queries
# once the ED polygons are done for all years, I should use those (and the grid) to make a simple
#     background for all these maps





### next set of maps will classify by white/black moves, arrows still show average new location
flow_map_by_race <- function(race_flows_coord, ed){
  
  ed_flow <- race_flows_coord %>% 
    filter(ed1 == ed) %>% 
    filter(is.na(lon2) == F & is.na(lat2) == F) %>% 
    mutate(black = if_else(black == 1, "Black", "White"))
  
  origin <- race_flows_coord %>%
    select(ed1, lon1, lat1) %>% 
    unique() %>% 
    filter(ed1 == ed)
  
  
  # aggregate flow data to clusters
  race_flow <- ed_flow %>% 
    group_by(black) %>% 
    summarise(count = sum(count),
              lon1 = median(lon1),
              lat1 = median(lat1),
              lon2 = median(lon2),
              lat2 = median(lat2)) %>% 
    ungroup() 
  
  
  phl_border <- st_union(ed1)
  
  points_plot <- st_as_sf(ed_flow, coords = c("lon2", "lat2"), crs = 26918)
  
  #  ggmap(ed_base) +
  ggplot() +
    theme_map() +
    scale_size_manual(values = c(0.3, 0.8, 1.5)) +
    scale_color_hue(name = "") +
    geom_sf(data = phl_border, fill = "grey85", col = "black", size = 0.2) +
    #geom_segment(data = race_flow,
    #             aes(x = lon1, y = lat1, 
    #                 xend = lon2, yend = lat2, 
    #                 col = factor(black)),
    #             size = 1,
    #             arrow = arrow(angle = 40, length = unit(0.25, "cm"))) +
    geom_point(data = ed_flow,
               aes(x = lon2, y = lat2, color = factor(black))) +
    geom_point(data = origin, 
               aes(x = lon1, y = lat1)) +
    #geom_sf(data = points_plot, aes(color = factor(black))) +
    #coord_sf(crs = st_crs(phl_border), datum = NA) +
    ggtitle(paste0("Residential flows of linked records, 19", t1, " to 19", t2),
            subtitle = paste0("ED ", ed, " in 19", t1," (", sum(race_flow$count), " total links)"))
}


phl_boundary <- counties(42) %>% 
  st_as_sf() %>% 
  st_transform(crs = 26918) %>% 
  filter(NAME == "Philadelphia")


flow_map_by_race(race_flows_coord, 119, ed2, phl_boundary)



flow_map_by_race(race_flows_coord, ed = 258) +
  ggsave("/Users/benjaminbellman/Desktop/desparation_plot5.pdf")

# 119, 729, 1252, 1838, 258, 182

animate_eds <- c()

