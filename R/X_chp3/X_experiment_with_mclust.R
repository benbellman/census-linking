library(mclust)
library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(ghibli)
library(rio)
library(purrr)
library(readr)

# load and combine all four ED files
ed10 <- st_read(here("data", "merged_eds", "Philadelphia_1910.shp"))
ed20 <- st_read(here("data", "merged_eds", "Philadelphia_1920.shp"))
ed30 <- st_read(here("data", "merged_eds", "Philadelphia_1930.shp"))
ed40 <- st_read(here("data", "merged_eds", "Philadelphia_1940.shp")) %>% st_transform(crs = st_crs(ed30))
ed10$year <- 1910
ed20$year <- 1920
ed30$year <- 1930
ed40$year <- 1940

eds <- rbind(ed10, ed20, ed30, ed40) %>% 
  filter(is.na(ed_pct_b) == F | is.na(ed_pct_f) == F)

# create matrix objects for ed and lag values
#ed_vars <- tibble(black = eds$ed_pct_b, imm = eds$ed_pct_f) 
#lag_vars <- tibble(black = eds$lg_pct_b, imm = eds$lg_pct_f) 

# run 3-group clustering on % black and % frnbrn (ED and lag)
#ed_clust <- Mclust(ed_vars, G = 3)
#lag_clust <- Mclust(lag_vars, G = 3)

#plot(ed_clust, what = "uncertainty")
#plot(lag_clust, what = "uncertainty")


# attach predictions and uncertainty to data
#eds$ed_class <- ed_clust$classification
#eds$ed_p <- ed_clust$uncertainty
#eds$lag_class <- lag_clust$classification
#eds$lag_p <- lag_clust$uncertainty

# Let's do our own custom classification (black, mixed-us, mixed-imm, imm, us)
eds$custom <- case_when(
  eds$ed_pct_b >= 25 ~ "Black",
  eds$ed_pct_b < 25 & eds$ed_pct_b >= 5 & eds$ed_pct_f >= 25 ~ "Mixed - Imm.",
  eds$ed_pct_b < 25 & eds$ed_pct_b >= 5 & eds$ed_pct_f < 25 ~ "Mixed - U.S.",
  eds$ed_pct_b < 5 & eds$ed_pct_f > 25 ~ "Immigrant",
  eds$ed_pct_b < 5 & eds$ed_pct_f < 25 ~ "U.S. White"
)
eds$custom <- factor(eds$custom, levels = c("Black", "Mixed - Imm.","Mixed - U.S.", "Immigrant", "U.S. White"))


# limit to only year, ED, and class and save as csv
eds %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  mutate(ed = as.character(ED)) %>% 
  select(ed, year, custom) %>% 
  write_csv(here("data", "ed_data", "custom_classes.csv"))
  


# specific palette for custom maps
custom_pal <- c("lightgrey",
                ghibli_palette("KikiMedium")[3:4],
                ghibli_palette("KikiLight")[3:4])



# let's make our own uncertainty plots
ggplot(eds) +
  geom_point(aes(x = ed_pct_b, y = ed_pct_f, col = factor(ed_class), size = ed_p), alpha = 0.2)

ggplot(eds) +
  geom_point(aes(x = ed_pct_b, y = ed_pct_f, col = factor(ed_class_4), size = ed_p), alpha = 0.2) +
  xlim(0,10) +
  ylim(0,50)

ggplot(eds) +
  geom_point(aes(x = ed_pct_b, y = ed_pct_f), alpha = 0.05)

ggplot(eds) +
  geom_point(aes(x = ed_pct_b, y = ed_pct_f, col = custom), alpha = 0.05) +
  scale_color_brewer(palette = "Set1")



# maps without considering uncertainty
# this tells use which group has been assigned to which profile for labeling (depends on chance)
ggplot(eds) +
  geom_sf(aes(fill = factor(ed_class)), lwd = 0) +
  scale_fill_manual(values = c(ghibli_palette("KikiMedium")[3:5])) +
  facet_wrap(. ~ year) +
  theme_bw() +
  ggsave(here("figures", "ed_class", "ed_val_3class.png"), height = 7, width = 7)


# create an "in-between" class 4 for the most uncertain cases
eds$ed_class_lh <- case_when(
  eds$ed_class == 1 & eds$ed_p < 0.05 ~ "U.S. White, high certainty",
  eds$ed_class == 2 & eds$ed_p < 0.05 ~ "Immigrant, high certainty",
  eds$ed_class == 3 & eds$ed_p < 0.05 ~ "Black, high certainty",
  eds$ed_class == 1 & eds$ed_p >= 0.05 ~ "U.S. White, low certainty",
  eds$ed_class == 2 & eds$ed_p >= 0.05 ~ "Immigrant, low certainty",
  eds$ed_class == 3 & eds$ed_p >= 0.05 ~ "Black, low certainty"
)
eds$ed_class_lh <- factor(eds$ed_class_lh, levels = c("U.S. White, high certainty", "Immigrant, high certainty", "Black, high certainty",
                                                      "U.S. White, low certainty", "Immigrant, low certainty", "Black, low certainty"))


# create maps, with shading for certainty
ggplot(eds) +
  geom_sf(aes(fill = factor(ed_class_lh)), lwd = 0) +
  scale_fill_manual(values = c(ghibli_palette("KikiMedium", direction = -1)[3:5], ghibli_palette("KikiLight", direction = -1)[3:5])) +
  facet_wrap(. ~ year) +
  theme_bw() +
  ggsave(here("figures", "ed_class", "ed_val_low-high.png"), height = 7, width = 7)




# add a fourth class to grey out any uncertain ED
eds$ed_class_4 <- case_when(
  eds$ed_class == 1 & eds$ed_p < 0.05 ~ "U.S. White",
  eds$ed_class == 2 & eds$ed_p < 0.05 ~ "Immigrant",
  eds$ed_class == 3 & eds$ed_p < 0.05 ~ "Black",
  eds$ed_p >= 0.05 ~ "Uncertain"
)

# create maps, with shading for certainty
ggplot(eds) +
  geom_sf(aes(fill = factor(ed_class_4)), lwd = 0) +
  scale_fill_manual(values = c(ghibli_palette("KikiMedium", direction = -1)[3:5], "lightgrey")) +
  facet_wrap(. ~ year) +
  theme_bw() +
  ggsave(here("figures", "ed_class", "ed_val_4class.png"), height = 7, width = 7)




# Map for custom classes
ggplot(eds) +
  geom_sf(aes(fill = factor(custom)), lwd = 0) +
  scale_fill_manual(values = custom_pal) +
  facet_wrap(. ~ year) +
  theme_bw() +
  ggsave(here("figures", "ed_class", "ed_custom.png"), height = 7, width = 7)



# Custom classes in each year
filter(eds, year == 1910) %>% 
  ggplot() +
  geom_sf(aes(fill = factor(custom)), lwd = 0) +
  scale_fill_manual(values = custom_pal) +
  #facet_wrap(. ~ year) +
  theme_bw() +
  ggsave(here("figures", "ed_class", "ed_custom_1910.png"), height = 7, width = 7)

filter(eds, year == 1920) %>% 
  ggplot() +
  geom_sf(aes(fill = factor(custom)), lwd = 0) +
  scale_fill_manual(values = custom_pal) +
  #facet_wrap(. ~ year) +
  theme_bw() +
  ggsave(here("figures", "ed_class", "ed_custom_1920.png"), height = 7, width = 7)

filter(eds, year == 1930) %>% 
ggplot() +
  geom_sf(aes(fill = factor(custom)), lwd = 0) +
  scale_fill_manual(values = custom_pal) +
  #facet_wrap(. ~ year) +
  theme_bw() +
  ggsave(here("figures", "ed_class", "ed_custom_1930.png"), height = 7, width = 7)

filter(eds, year == 1940) %>% 
  ggplot() +
  geom_sf(aes(fill = factor(custom)), lwd = 0) +
  scale_fill_manual(values = custom_pal) +
  #facet_wrap(. ~ year) +
  theme_bw() +
  ggsave(here("figures", "ed_class", "ed_custom_1940.png"), height = 7, width = 7)



# export classified ED data
eds %>% 
  filter(year == 1910) %>% 
  st_write(here("data", "merged_eds", "class", "Phl1910.shp"))

eds %>% 
  filter(year == 1920) %>% 
  st_write(here("data", "merged_eds", "class", "Phl1920.shp"))

eds %>% 
  filter(year == 1930) %>% 
  st_write(here("data", "merged_eds", "class", "Phl1930.shp"))

eds %>% 
  filter(year == 1940) %>% 
  st_write(here("data", "merged_eds", "class", "Phl1940.shp"))


# lets load in the linked data and only keep movers (based on same chp2 methods)
linked <- import(here("data", "for_models", "phl_loc_attain.csv")) %>% 
  as_tibble() %>% 
  filter(hh_max_sei1 > 0 & not_hhh1 == 0) %>% 
  # if the address is the same in both years, the family did not move
  mutate(#moved = if_else(both_same == 1, 0, 1),
    woman = sex1 - 1,
    married = if_else(marst1 < 3, 1, 0),
    age_sq1 = age1 * age1,
    #hh_n_adults1 = hh_size1 - hh_n_kids1,
    kids_1_to_2 = if_else(hh_n_kids1 > 0 & hh_n_kids1 < 3, 1, 0),
    kids_3_up = if_else(hh_n_kids1 >= 3, 1, 0),
    # create squared term for ed % black and ed % foreign born
    ed_pct_black_sq1 = ed_pct_black1 * ed_pct_black1,
    ed_pct_frnbrn_sq1 = ed_pct_frnbrn1 * ed_pct_frnbrn1,
    ed_mean_sei_sq1 = ed_mean_sei1 * ed_mean_sei1,
    lag_pct_black_sq1 = lag_pct_black1 * lag_pct_black1,
    lag_pct_frnbrn_sq1 = lag_pct_frnbrn1 * lag_pct_frnbrn1)

# split data by year for merging with ED distances
data_list <- list(
  filter(linked, year1 == 1910),
  filter(linked, year1 == 1920),
  filter(linked, year1 == 1930)
)

# merge with pre-calcualted ED distances
list(
  here("data", "ed_distances_10_20.csv"),
  here("data", "ed_distances_20_30.csv"),
  here("data", "ed_distances_30_40.csv")
) %>% 
  map(import) %>% 
  map(rename, ed2 = dest_ed) %>% 
  map(mutate, ed2 = as.character(ed2)) %>% 
  map2(data_list, inner_join) %>% 
  map(as_tibble) %>% 
  bind_rows() %>% 
  mutate(moved = if_else(dist > 750, "Moved", "Stayed"),
         ed1 = as.character(ed1)) -> linked

# set up the ed data te merge with linked households
ed1 <- filter(eds, year %in% c(1910, 1920, 1930)) %>% 
  select(year, custom, ED) %>% 
  rename(year1 = year, ed1 = ED, Origin = custom) %>% 
  st_drop_geometry()

ed2 <- filter(eds, year %in% c(1920, 1930, 1940)) %>% 
  select(year, custom, ED) %>% 
  rename(year2 = year, ed2 = ED, Destination = custom) %>% 
  st_drop_geometry()

# merge with linked households
linked <- linked %>% left_join(ed1) %>% left_join(ed2)

# movers and stayers
#movers <- filter(linked, moved == 1)
#stayers <- filter(linked, moved == 0)

# create data structure for alluvial diagram for movers
# re-scale all counts to represent % of all linked households
alluv <- linked %>% 
  filter(race_cat != "Other" & (is.na(Origin) == F) & (is.na(Destination) == F)) %>% 
  group_by(moved, Origin, Destination, race_cat, year1) %>% 
  summarize(flow_n = n()) %>% 
  group_by(moved, race_cat, year1) %>% 
  mutate(flow_pct = flow_n / sum(flow_n) * 100)
  
  
  
## Alluvial diagrams for movers
# black
filter(alluv, race_cat == "Black") %>% 
  ggplot(aes(y = flow_pct, axis1 = Origin, axis2 = Destination)) +
  geom_flow(aes(fill = Destination), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", label.strata = TRUE, size = 2) +
  scale_x_discrete(limits = c("Origin", "Destination")) +
  facet_grid(moved ~ year1) +
  labs(y = "Percent of households",
       fill = "Destination type",
       title = "Transitions between neighborhood types",
       subtitle = "Black households") +
  theme_bw() +
  ggsave(here("figures", "ed_class", "black_custom_alluv.pdf"), height = 5, width = 7)

# white immigrant
filter(alluv, race_cat == "White Imm") %>% 
  ggplot(aes(y = flow_pct, axis1 = Origin, axis2 = Destination)) +
  geom_flow(aes(fill = Destination), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", label.strata = TRUE, size = 2) +
  scale_x_discrete(limits = c("Origin", "Destination")) +
  facet_grid(moved ~ year1) +
  labs(y = "Percent of households",
       fill = "Destination type",
       title = "Transitions between neighborhood types",
       subtitle = "Immigrant households") +
  theme_bw() +
  ggsave(here("figures", "ed_class", "wimm_custom_alluv.pdf"), height = 5, width = 7)


# native-born white
filter(alluv, race_cat == "White NB") %>% 
  ggplot(aes(y = flow_pct, axis1 = Origin, axis2 = Destination)) +
  geom_flow(aes(fill = Destination), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", label.strata = TRUE, size = 2) +
  scale_x_discrete(limits = c("Origin", "Destination")) +
  facet_grid(moved ~ year1) +
  labs(y = "Percent of households",
       fill = "Destination type",
       title = "Transitions between neighborhood types",
       subtitle = "U.S. white households") +
  theme_bw() +
  ggsave(here("figures", "ed_class", "wnb_custom_alluv.pdf"), height = 5, width = 7)


# whites in 1930
filter(alluv, race_cat == "White NB") %>% 
  ggplot(aes(y = flow_pct, axis1 = Origin, axis2 = Destination)) +
  geom_flow(aes(fill = Destination), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", label.strata = TRUE, size = 2) +
  scale_x_discrete(limits = c("Origin", "Destination")) +
  facet_grid(moved ~ year1) +
  labs(y = "Percent of households",
       fill = "Destination type",
       title = "Transitions between neighborhood types",
       subtitle = "U.S. white households") +
  theme_bw() +
  ggsave(here("figures", "ed_class", "wnb_custom_alluv.pdf"), height = 5, width = 7)

  

## More ideas

# map these types of moves separately, or make a cowplot
# arbitrarily cluster the ED moves and use line thickness to represent frequency of move cluster
# code for this kind of thing already exists






