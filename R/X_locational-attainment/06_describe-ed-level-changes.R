library(here)
library(rio)
library(tibble)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(purrr)
library(stringr)
library(classInt)
library(hrbrthemes)
library(tigris)
library(sf)
library(ggforce)
library(cowplot)

source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "aggregate_microdata.R"))

#### Load Data ####

# load linked sample, restrict to those who hosuehold heads changed addresses
linked <- import(here("data", "for_models", "phl_loc_attain.csv")) %>% 
  as_tibble() %>% 
  filter(sei1 > 0) %>% 
  # if the address is the same in both years, the family did not move
  mutate(moved = if_else(both_same == 1, 0, 1),
         woman = sex1 - 1,
         married = if_else(marst1 < 3, 1, 0),
         age_sq1 = age1 * age1,
         hh_n_adults1 = hh_size1 - hh_n_kids1,
         # create squared term for ed % black and ed % foreign born
         ed_pct_black_sq1 = ed_pct_black1 * ed_pct_black1,
         ed_pct_frnbrn_sq1 = ed_pct_frnbrn1 * ed_pct_frnbrn1,
         lag_pct_black_sq1 = lag_pct_black1 * lag_pct_black1,
         lag_pct_frnbrn_sq1 = lag_pct_frnbrn1 * lag_pct_frnbrn1,
         # calculate terms for time differences between ed-level vars
         ed_pct_black_diff = ed_pct_black2 - ed_pct_black1,
         ed_pct_frnbrn_diff = ed_pct_frnbrn2 - ed_pct_frnbrn1,
         ed_mean_sei_diff = ed_mean_sei2 - ed_mean_sei1) %>% 
  # drop people in "Other" race category
  filter(race_cat != "Other")


# limit to 1920-30 movers, add distance
ed_dist <- import(here("data", "ed_distances_20_30.csv")) %>% 
  rename(ed2 = dest_ed) %>% 
  mutate(ed2 = as.character(ed2))

linked_dist <- linked %>% 
  filter(moved == 1 & year1 == 1920) %>% 
  inner_join(ed_dist)

brks10 <- classIntervals(linked_dist$dist, style = "quantile", n = 10)$brks
brks100 <- classIntervals(linked_dist$dist, style = "quantile", n = 100)$brks

# create decile and percentile categorical variables
linked_dist <- mutate(linked_dist,
                      dist_decile = cut(dist,
                                        breaks = brks10,
                                        labels = (1:10)),
                      dist_percentile = cut(dist,
                                        breaks = brks100,
                                        labels = (1:100)))

# summarize each change by distance moved
summ10 <- linked_dist %>% 
  filter(is.na(dist_decile) == F) %>% 
  group_by(dist_decile, race_cat) %>% 
  summarise(n = n(),
            dist = mean(dist, na.rm = T),
            ed_pct_black_diff = mean(ed_pct_black_diff, na.rm = T),
            ed_pct_frnbrn_diff = mean(ed_pct_frnbrn_diff, na.rm = T),
            ed_mean_sei_diff = mean(ed_mean_sei_diff, na.rm = T),
            ed_pct_black1 = mean(ed_pct_black1, na.rm = T),
            ed_pct_frnbrn1 = mean(ed_pct_frnbrn1, na.rm = T),
            ed_mean_sei1 = mean(ed_mean_sei1, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(dist_decile = as.numeric(dist_decile))

summ100 <- linked_dist %>% 
  filter(is.na(dist_percentile) == F) %>% 
  group_by(dist_percentile, race_cat) %>% 
  summarise(n = n(),
            dist = mean(dist, na.rm = T),
            ed_pct_black_diff = mean(ed_pct_black_diff, na.rm = T),
            ed_pct_frnbrn_diff = mean(ed_pct_frnbrn_diff, na.rm = T),
            ed_mean_sei_diff = mean(ed_mean_sei_diff, na.rm = T),
            ed_pct_black1 = mean(ed_pct_black1, na.rm = T),
            ed_pct_frnbrn1 = mean(ed_pct_frnbrn1, na.rm = T),
            ed_mean_sei1 = mean(ed_mean_sei1, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(dist_percentile = as.numeric(dist_percentile))

# percentile scatterplot of change in % black and % foreign born
# color intensity increases with distance percentile
ggplot(summ100) +
  geom_point(aes(x = ed_pct_black_diff, y = ed_pct_frnbrn_diff, col = dist_percentile)) +
  scale_color_gradient(low = "#d6d6d6", high = "#4a4a4a") + 
  theme_ipsum(axis_title_size = 12) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(#title = "Neighborhood change by distance moved",
       #subtitle = "Philadelphia, 1920-1930",
       x = "Change in % black",
       y = "Change in % foreign-born",
       col = "Distance\npercentile") +
  facet_grid(race_cat ~ .)


# plotting the average change in these attributes by decile across racial categories
ggplot(summ10) +
  geom_segment(aes(x = ed_pct_black1, y = ed_pct_frnbrn1, 
               xend = ed_pct_black1 + ed_pct_black_diff, yend = ed_pct_frnbrn1 + ed_pct_frnbrn_diff, 
               col = dist_decile),
               arrow = arrow(length = unit(0.2, "cm"))) +
  scale_color_gradient(low = "#d6d6d6", high = "#2b2b2b") + 
  theme_ipsum(axis_title_size = 12) +
  labs(#title = "Neighborhood change by distance moved",
    #subtitle = "Philadelphia, 1920-1930",
    x = "% black",
    y = "% foreign-born",
    col = "Distance\ndecile") +
  facet_grid(race_cat ~ .)

# create Philadelphia map that describes these distances with concentric circles
circles <- summ10 %>% 
  group_by(dist_decile) %>% 
  summarise(dist = mean(dist, weighted.mean(dist, n, na.rm = T)))

phl <- st_as_sf(counties("42")) %>% filter(COUNTYFP == "101") %>% st_transform(26918)

center <- st_geometry(st_centroid(phl))[[1]]

ggplot() +
  geom_sf(data = phl, fill = "white", size = 0.2) + 
  #geom_point(aes(x = center[1], y = center[2]), col = "black", size = 0.5) +
  geom_circle(data = circles, 
              aes(x0 = center[1], y0 = center[2], r = dist, col = dist_decile),
              fill = NA, size = 0.4) +
  scale_color_gradient(low = "#ffffff", high = "#000000", breaks = seq(2, 10, 2)) +
  theme_ipsum() +
  theme(axis.text = element_blank()) +
  labs(x = "", y = "", col = "Distance\ndecile")

ggplot(circles) +
  geom_path(aes(x = dist_decile, y = dist)) +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  theme_ipsum(axis_title_size = 12) +
  labs(x = "Decile", y = "Mean distance moved (m)")


