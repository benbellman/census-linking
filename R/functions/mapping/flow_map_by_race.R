library(dplyr)

flow_map_by_race <- function(race_flows_coord, ed1_id, ed2, boundary){
  
  # set flows for input ED
  ed_flow <- race_flows_coord %>% 
    filter(ed1 == as.character(ed1_id)) %>% 
    filter(is.na(lon2) == F & is.na(lat2) == F) %>% 
    mutate(black = if_else(black == 1, "Black", "White")) 
  #%>% 
  #  st_as_sf(coords = c("lon2", "lat2"), crs = 26918)
  
  # set origin point of input ED
  origin <- race_flows_coord %>%
    select(ed1, lon1, lat1) %>% 
    unique() %>% 
    filter(ed1 == as.character(ed1_id)) %>% 
    st_as_sf(coords = c("lon1", "lat1"), crs = 26918)
  
  
  # aggregate flow data to clusters
  race_flow <- ed_flow %>% 
    group_by(black) %>% 
    summarise(count = sum(count),
              lon1 = median(lon1),
              lat1 = median(lat1),
              lon2 = median(lon2),
              lat2 = median(lat2)) %>% 
    ungroup() 
  
  # set 1km buffer for origin ED centroid
  #ed1_buffer <- st_buffer(st_as_sf(origin, coords = c("lon1", "lat1"), crs = 26918), 1000)
  ed1_buffer <- st_buffer(origin, 1000)
  
  # create object of EDs that are more than 15% black
  ed2_over_10b <- filter(ed2, pct_black >= 10) %>% st_union
  
  #points_plot <- st_as_sf(ed_flow, coords = c("lon2", "lat2"), crs = 26918)
  
  #boundary <- st_union(ed2)
  
  ggplot() +
    #theme_map() +
    #scale_size_manual(values = c(0.3, 0.8, 1.5)) +
    #geom_sf(data = boundary, fill = "white", col = "black", size = 0.2) +
    geom_sf(data = ed2_over_10b, fill = "grey85", size = 0) +
    #geom_sf(data = ed_flow, aes(color = factor(black))) +
    geom_sf(data = origin) +
    geom_sf(data = ed1_buffer, col = "black", size = 0.4, alpha = 0) +
    theme(legend.position = "none") +
  
    #scale_color_hue(name = "", 
    #                guide = guide_legend(override.aes = list(shape = c("a", "a")))) +
    geom_segment(data = race_flow,
                 aes(x = lon1, y = lat1, 
                     xend = lon2, yend = lat2, 
                     col = factor(black)),
                 size = 1,
                 arrow = arrow(angle = 40, length = unit(0.25, "cm"))) +
    geom_point(data = ed_flow,
               aes(x = lon2, y = lat2, group = ed1, color = factor(black)))
    

}