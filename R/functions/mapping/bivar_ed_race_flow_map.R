library(ggplot2)
library(sf)
library(dplyr)
library(tidyr)

ed1_choice <- 1

bivar_ed_race_flow_map <- function(linked, ed1_choice, ed1, ed2){
  
  #### 1. Organize linked data and ED coordinates ####
  
  # grab linked records that originated in chosen ED
  flows <- linked %>% 
    filter(ed1 == ed1_choice & (white == 1 | black == 1)) %>% 
    # add single race variable, convert ed to numeric
    mutate(plot_race = if_else(black == 1, "Black", "White"),
           ed1 = as.numeric(ed1),
           ed2 = as.numeric(ed2)) %>% 
    # aggregated to ED-to-ED flows
    group_by(ed1, ed2, plot_race) %>% 
    summarize(count = n())
  
  # set centroids from ED polygons
  ed1_cent <- st_centroid(ed1) %>% 
    st_transform(4326) %>% 
    st_coordinates() %>% 
    cbind(ed1$ed) %>% 
    as_tibble() %>% 
    rename(lon1 = X,
           lat1 = Y,
           ed1 = V3) %>% 
    filter(is.na(lon1) == F & is.na(lat1) == F)
  
  ed2_cent <- st_centroid(ed2) %>% 
    st_transform(4326) %>% 
    st_coordinates() %>% 
    cbind(ed2$ed) %>% 
    as_tibble() %>% 
    rename(lon2 = X,
           lat2 = Y,
           ed2 = V3) %>% 
    filter(is.na(lon2) == F & is.na(lat2) == F)
  
  # combine flow data with coordinates
  flow_coords <- flows %>% 
    inner_join(ed1_cent) %>% 
    inner_join(ed2_cent)
  
  #%>% 
  #  st_as_sf(coords = c("lon2", "lat2"), crs = 26918)
  
  # set origin point of input ED
  origin <- filter(ed1_cent, ed1 == ed1_choice)
  
  # aggregate flow data to single median arrows by race
  race_arrows <- flow_coords %>% 
    group_by(plot_race) %>% 
    summarise(count = sum(count),
              lon1 = mean(lon1),
              lat1 = mean(lat1),
              lon2 = mean(lon2),
              lat2 = mean(lat2))
  
  # set 1km buffer for origin ED centroid
  #ed1_buffer <- st_buffer(st_as_sf(origin, coords = c("lon1", "lat1"), crs = 26918), 1000)
  #ed1_buffer <- st_buffer(origin, 1000)
  
  
  #### 2. Set up bivariate polygon map
  
  # create 3 buckets for % black
  cutoffs_var1 <- c(0, 20, 40, 100)
  
  # create 3 buckets for % foreign born
  cutoffs_var2 <- c(0, 20, 40, 100)
  
  var1_lab <- "% Black"
  var2_lab <- "% Foreign born"
  
  # manually set color scale
  bivariate_color_scale <- tibble(
    "3 - 3" = "#2a5a5b", # high var1, high var2
    "2 - 3" = "#567994",
    "1 - 3" = "#6c83b5", # low var1, high var2
    "3 - 2" = "#5a9178",
    "2 - 2" = "#90b2b3", # medium var1, medium var2
    "1 - 2" = "#b5c0da",
    "3 - 1" = "#73ae80", # high var1, low var2
    "2 - 1" = "#b8d6be",
    "1 - 1" = "#e8e8e8" # low var1, low var2
  ) %>%
    gather("group", "bg_fill")
  
  # merge color information into ed1 data based on cut-offs
  ed1_bivar <- ed1 %>%
    # categorize vars by cutoffs
    mutate(var1_cat = cut(pct_black, breaks = cutoffs_var1, include.lowest = TRUE),
           var2_cat = cut(pct_frnbrn, breaks = cutoffs_var2, include.lowest = TRUE),
           # create 9 bi-variate groups
           group = paste(as.numeric(var1_cat), "-", as.numeric(var2_cat))) %>% 
    # merge hex color codes
    left_join(bivariate_color_scale, by = "group")

  
  
  ggplot() +
    # First, plot the bi-varariate choropleth map
    geom_sf(data = ed1_bivar,
            mapping = aes(fill = bg_fill), 
            color = "white", size = 0) +
    scale_fill_identity() +
    # Next, add the points marking locations where households lived in t2
    geom_point(data = flow_coords,
               mapping = aes(x = lon2, y = lat2, color = plot_race)) +
    # Next add the arrows marking the median movement of each racial group
    geom_segment(data = race_arrows,
                 mapping = aes(x = lon1, y = lat1, xend = lon2, yend = lat2, 
                     col = plot_race),
                 size = 1,
                 arrow = arrow(angle = 40, length = unit(0.25, "cm"))) +
    # set the color scheme for points and arrows
    scale_color_manual(values = c("red", "black")) +
    # Finally, add point and buffer marking ED origin
    geom_point(data = origin, aes(x = lon1, y = lat1)) +
    # labels and formatting
    labs(x = "", y = "", size = "", col = "") +
    theme_minimal()
  
  
}
