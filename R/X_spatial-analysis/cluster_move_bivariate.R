library(mclust)
library(rio)
library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(tigris)
library(magrittr)
library(cowplot)
library(useful)
library(purrr)

source(here("R", "functions", "aggregate_microdata.R"))

# function to return cluster of input df
get_clusters <- function(df, G = NULL){
  # return a single value for class if only one unique row
  if(nrow(unique(df)) == 1){
    mutate(df, class = 1)
  # otherwise, run the clustering algorithm
  } else {
    df$class <- Mclust(df, G = G)$classification
    df
  }
} 

safe_clusters <- safely(get_clusters)

# set years for time points
t1 <- 20
t2 <- t1 + 10

# load linked data
linked <- import(here("data", "linked", paste0("linked_", t1, "_", t2, ".csv"))) %>% 
  as_tibble() %>% 
  mutate(ed1 = as.character(ed1),
         ed2 = as.character(ed2),
         black = if_else(race_grp1 == "Black" | race_grp2 == "Black", 1, 0),
         white = if_else(race_grp1 == "White" & race_grp2 == "White", 1, 0))


# load 1920 polygons
phl_ed_t1 <- st_read(here("data", "merged_eds", paste0("ed_19", t1, ".shp"))) %>% 
  rename(pct_black = pct_blc,
         pct_frnbrn = pct_frn)
phl_ed_t2 <- st_read(here("data", "merged_eds", paste0("ed_19", t1, ".shp"))) %>% 
  rename(pct_black = pct_blc,
         pct_frnbrn = pct_frn)

# get ED data
ed_data_t1 <- aggregate_microdata(t1, ed) %>% rename(ED = ed)
ed_data_t2 <- aggregate_microdata(t2, ed) %>% rename(ED = ed)

# load ED polyogons
ed_poly_t1 <- st_read(here("data", "shertzer_eds", paste0("Philadelphia_19", t1,".shp"))) %>% 
  st_transform(4326)
ed_poly_t2 <- st_read(here("data", "shertzer_eds", paste0("Philadelphia_19", t2,".shp"))) %>% 
  st_transform(4326)

# merge together
ed_t1 <- merge(ed_poly_t1, ed_data_t1)
ed_t2 <- merge(ed_poly_t2, ed_data_t2)

# create centroid tables to merge to linked data
cent_t1 <- ed_poly_t1 %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(ed_poly_t1) %>% 
  select(X, Y, ED) %>% 
  rename(X1 = X, Y1 = Y, ed1 = ED) %>% 
  mutate(ed1 = as.character(ed1))

cent_t2 <- ed_poly_t2 %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(ed_poly_t2) %>% 
  select(X, Y, ED) %>% 
  rename(X2 = X, Y2 = Y, ed2 = ED) %>% 
  mutate(ed2 = as.character(ed2))

# merge coordinates to linked data, add additional geometry variables for clustering
linked %>% 
  left_join(cent_t1) %>% 
  left_join(cent_t2) %>% 
  select(X1, Y1, X2, Y2, black, white, ed1, ed2) -> move_arrows# %>% 
  #mutate(chg_x = X2 - X1, 
  #       chg_y = Y2 - Y1,
  #       distance = sqrt(chg_x^2 + chg_y^2),
  #       theta = cart2pol(chg_x, chg_y)$theta) -> move_arrows

# separate by race
white_arrows <- move_arrows %>% 
  filter(white == 1) %>% 
  select(-black, -white) %>% 
  filter((is.na(X1) == F) & (is.na(Y1) == F) & (is.na(X2) == F) & (is.na(Y2) == F))

black_arrows <- move_arrows %>% 
  filter(black == 1) %>% 
  select(-black, -white, -ed2) %>% 
  filter((is.na(X1) == F) & (is.na(Y1) == F) & (is.na(X2) == F) & (is.na(Y2) == F))

# group by ED in time 1, cluster, and combine into ED-cluster ID col
black_arrows %>% 
  # first summarize by count
  #group_by(ed1, ed2) %>% 
  #mutate(count = n()) %>% 
  # keep only unique flows
  #unique() %>% 
  # re-group by only ed1 and compute clusters
  group_by(ed1) %>% 
  group_split() %>% 
  map(safe_clusters) %>% 
  map(pluck, "result") %>% 
  bind_rows() %>% 
  mutate(ed_class = paste(ed1, class, sep = "-")) %>%
  group_by(ed_class) %>% 
  summarise(X1 = mean(X1),
            X2 = mean(X2),
            Y1 = mean(Y1),
            Y2 = mean(Y2),
            count = n()) -> test

get_clusters(df = select(test, -ed_class, -count), G = 40) %>% 
  bind_cols(select(test, count)) %>% 
  group_by(class) %>% 
  summarise(X1 = mean(X1),
            X2 = mean(X2),
            Y1 = mean(Y1),
            Y2 = mean(Y2),
            count = sum(count)) -> test2

### OR just doing a single run of clustering using defined G
white_arrows %>% 
  select(-ed1) %>% 
  get_clusters(G = 100) %>% 
  group_by(class) %>% 
  summarise(X1 = mean(X1),
            X2 = mean(X2),
            Y1 = mean(Y1),
            Y2 = mean(Y2)) -> white_test


black_arrows %>% 
  select(-ed1) %>% 
  get_clusters(G = 60) %>% 
  group_by(class) %>% 
  summarise(X1 = mean(X1),
            X2 = mean(X2),
            Y1 = mean(Y1),
            Y2 = mean(Y2)) -> black_test


  


  


# cluster identify 50 clusters of each
#white_arrows$cluster <- Mclust(white_arrows, G = 75)$classification
#black_arrows$cluster <- Mclust(black_arrows, G = 20)$classification

#black_clusters <- Mclust(black_arrows, G = 25)
#black_arrows$cluster <- black_clusters$classification

# summarize ED means as flows
# classify these mean ED flows to reduce map clutter

white_flows <- white_arrows %>% 
  group_by(ed1) %>% 
  summarise(X1 = mean(X1),
            Y1 = mean(Y1),
            X2 = mean(X2),
            Y2 = mean(Y2))

#white_flows$cluster <- Mclust(white_flows, G = 50)$classification
#white_clusters <- white_flows %>% 
#  group_by(cluster) %>% 
#  summarise(X1 = mean(X1),
#            Y1 = mean(Y1),
#            X2 = mean(X2),
#            Y2 = mean(Y2))

ed_clusters <- black_arrows %>% 
  group_split(ed1) %>% 
  purrr::map(safe_clusters) %>% 
  purrr::map(pluck, result) %>% 
  purrr::map(pluck, classification)

  mutate(cluster = get_clusters(c(X1, Y1, X2, Y2, chg_x, chg_y, distance, theta))) %>% 
  summarise(X1 = mean(X1),
            Y1 = mean(Y1),
            X2 = mean(X2),
            Y2 = mean(Y2))

black_flows$cluster <- Mclust(black_flows, G = 15)$classification
black_clusters <- black_flows %>% 
  group_by(cluster) %>% 
  summarise(X1 = mean(X1),
            Y1 = mean(Y1),
            X2 = mean(X2),
            Y2 = mean(Y2))


#### Start Code for Bivariate Map, add in arrows to separate panels of same map

# create 3 buckets for % black
cutoffs_var1 <- c(0, 5, 20, 100)

# create 3 buckets for % foreign born
cutoffs_var2 <- c(0, 15, 25, 100)

var1_lab <- "% Black"
var2_lab <- "% Foreign born"

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#2a5a5b", # high inequality, high income
  "2 - 3" = "#567994",
  "1 - 3" = "#6c83b5", # low inequality, high income
  "3 - 2" = "#5a9178",
  "2 - 2" = "#90b2b3", # medium inequality, medium income
  "1 - 2" = "#b5c0da",
  "3 - 1" = "#73ae80", # high inequality, low income
  "2 - 1" = "#b8d6be",
  "1 - 1" = "#e8e8e8" # low inequality, low income
) %>%
  gather("group", "bg_fill")

# attach groups and colors to ED data
ed_t1 %<>%
  # categorize vars by cutoffs
  mutate(var1_cat = cut(pct_black, breaks = cutoffs_var1, include.lowest = TRUE),
         var2_cat = cut(pct_frnbrn, breaks = cutoffs_var2, include.lowest = TRUE),
         # create 9 bi-variate groups
         group = paste(as.numeric(var1_cat), "-", as.numeric(var2_cat))) %>% 
  # merge hex color codes
  left_join(bivariate_color_scale, by = "group")

# create map

ggplot(ed_t1) +
  # color EDs according to their pct_black / mean_sei combination
  # use white boundaries
  geom_sf(aes(fill = bg_fill), color = "white", size = 0) +
  # as the sf object municipality_prod_geo has a column with name "fill" that
  # contains the literal color as hex code for each municipality, we can use
  # scale_fill_identity here
  scale_fill_identity() +
  # add mean flow arrows for sample of white movers
  geom_segment(data = white_test,
               aes(x = X1, y = Y1, 
                   xend = X2, yend = Y2),
               col = "black",
               size = 0.3,
               alpha = 1,
               arrow = arrow(angle = 45, length = unit(0.1, "cm"))) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "White movers") +
  # add the theme
  theme_minimal()

# black movers
ggplot(ed_t1) +
  # color EDs according to their pct_black / mean_sei combination
  # use white boundaries
  geom_sf(aes(fill = bg_fill), color = "white", size = 0) +
  # as the sf object municipality_prod_geo has a column with name "fill" that
  # contains the literal color as hex code for each municipality, we can use
  # scale_fill_identity here
  scale_fill_identity() +
  # add mean flow arrows for sample of white movers
  geom_segment(data = black_flows,
               aes(x = X1, y = Y1, 
                   xend = X2, yend = Y2),
               col = "black",
               size = 0.5,
               arrow = arrow(angle = 35, length = unit(0.2, "cm"))) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Black movers") +
  # add the theme
  theme_minimal()

# create legend
# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("var1", "var2"), sep = " - ") %>%
  mutate(var1 = as.integer(var1),
         var2 = as.integer(var2))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = var1,
      y = var2,
      fill = bg_fill)
  ) +
  scale_fill_identity() +
  #labs(x = paste("Higher", var1_lab, sprintf("\u27f6")),
  #     y = paste("Higher", var2_lab, sprintf("\u27f6"))) +
  labs(x = paste("Higher", var1_lab, "-->"),
       y = paste("Higher", var2_lab, "-->")) +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6)#, axis.text.y = element_text(angle = 90)
  ) +
  # quadratic tiles
  coord_fixed()

#list(map = map, legend = legend)
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)

