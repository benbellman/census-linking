library(here)
library(rio)
library(sf)
library(ggplot2)
library(dplyr)
library(mclust)
library(purrr)
library(ghibli)


source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "aggregate_microdata.R"))

source(here("R", "functions", "sept_flow_maps", "create_flows.R"))

#### Load all data ####

# locational attainment linked sample
# drop sei == 0, non-household heads, and people who stayed at same address
linked <- import(here("data", "for_models", "phl_loc_attain.csv")) %>% 
  as_tibble() %>% 
  filter(sei1 > 0 & not_hhh1 == 0 & both_same == 0) %>% 
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
library(tigris)
phl <- places(state = 42)
phl <- st_as_sf(phl)
phl <- filter(phl, NAME == "Philadelphia")
phl <- st_transform(phl, crs = st_crs(eds))

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
library(readr)
write_csv(linked, here("data", "for_models", "links-for-flow-maps.csv"))

#### Prepare flow data ####

#linked <- import(here("data", "for_models", "links-for-flow-maps.csv"))

# create an object of all possible flows (by year, race, origin class, destination class)
linked %>% 
  # group by the appropriate categories
  group_by(year1, race_cat, custom1, custom2) %>% 
  group_split() %>% 
  map(get_flows) -> test









# create flow objects for neighborhoods in all years

flows_10_black <- list(
  get_flows(linked, year1 == 1910, custom1 == "Black", race_cat == "Black") %>% mutate(race_cat = "Black"),
  get_flows(linked, year1 == 1910, custom1 == "Black", race_cat == "White Imm") %>% mutate(race_cat = "White Imm"),
  get_flows(linked, year1 == 1910, custom1 == "Black", race_cat == "White NB") %>% mutate(race_cat = "White NB")
) %>% 
  bind_rows() %>% 
  mutate(Year = "1910-20")

flows_20_black <- list(
  get_flows(linked, year1 == 1920, custom1 == "Black", race_cat == "Black") %>% mutate(race_cat = "Black"),
  get_flows(linked, year1 == 1920, custom1 == "Black", race_cat == "White Imm") %>% mutate(race_cat = "White Imm"),
  get_flows(linked, year1 == 1920, custom1 == "Black", race_cat == "White NB") %>% mutate(race_cat = "White NB")
) %>% 
  bind_rows() %>% 
  mutate(Year = "1920-30")

flows_30_black <- list(
  get_flows(linked, year1 == 1930, custom1 == "Black", race_cat == "Black") %>% mutate(race_cat = "Black"),
  get_flows(linked, year1 == 1930, custom1 == "Black", race_cat == "White Imm") %>% mutate(race_cat = "White Imm"),
  get_flows(linked, year1 == 1930, custom1 == "Black", race_cat == "White NB") %>% mutate(race_cat = "White NB")
) %>% 
  bind_rows() %>% 
  mutate(Year = "1930-40")


# create additional grouping of flows by neighborhood type in time 2

# black households in black EDs in 1910
flows_blackn_blackhh_10 <- list(
  get_flows(linked, year1 == 1910, race_cat == "Black", custom1 == "Black", custom2 == "Black") %>% mutate(custom2 = "Black"),
  get_flows(linked, year1 == 1910, race_cat == "Black", custom1 == "Black", custom2 == "Mixed - Imm.") %>% mutate(custom2 = "Mixed - Imm."),
  get_flows(linked, year1 == 1910, race_cat == "Black", custom1 == "Black", custom2 == "Mixed - U.S.") %>% mutate(custom2 = "Mixed - U.S."),
  get_flows(linked, year1 == 1910, race_cat == "Black", custom1 == "Black", custom2 == "Immigrant") %>% mutate(custom2 = "Immigrant"),
  get_flows(linked, year1 == 1910, race_cat == "Black", custom1 == "Black", custom2 == "U.S. White") %>% mutate(custom2 = "U.S. White")
) %>% 
  bind_rows() %>% 
  mutate(custom2 = factor(custom2, levels = c("Black", "Mixed - Imm.","Mixed - U.S.", "Immigrant", "U.S. White")),
         n_class = case_when(n < 20 ~ "5 to 19", n >= 20 & n < 50 ~ "20 to 49", n >= 50 ~ "50 and up"),
         n_class = factor(n_class, levels = c("5 to 19", "20 to 49", "50 and up")))

# white households in black EDs in 1910
flows_blackn_whitehh_10 <- list(
  get_flows(linked, year1 == 1910, race_cat == "White NB", custom1 == "Black", custom2 == "Black") %>% mutate(custom2 = "Black"),
  get_flows(linked, year1 == 1910, race_cat == "White NB", custom1 == "Black", custom2 == "Mixed - Imm.") %>% mutate(custom2 = "Mixed - Imm."),
  get_flows(linked, year1 == 1910, race_cat == "White NB", custom1 == "Black", custom2 == "Mixed - U.S.") %>% mutate(custom2 = "Mixed - U.S."),
  get_flows(linked, year1 == 1910, race_cat == "White NB", custom1 == "Black", custom2 == "Immigrant") %>% mutate(custom2 = "Immigrant"),
  get_flows(linked, year1 == 1910, race_cat == "White NB", custom1 == "Black", custom2 == "U.S. White") %>% mutate(custom2 = "U.S. White")
) %>% 
  bind_rows() %>% 
  mutate(custom2 = factor(custom2, levels = c("Black", "Mixed - Imm.","Mixed - U.S.", "Immigrant", "U.S. White")),
         n_class = case_when(n < 20 ~ "5 to 19", n >= 20 & n < 50 ~ "20 to 49", n >= 50 ~ "50 and up"),
         n_class = factor(n_class, levels = c("5 to 19", "20 to 49", "50 and up")))




#### Create maps ####

# color palette for ED categories
custom_pal <- c("lightgrey", ghibli_palette("KikiMedium")[3:4], ghibli_palette("KikiLight")[3:4])

# plot for all three groups in Black neighborhoods in 1910
ggplot() +
  geom_sf(
    data = filter(eds, year == 1910),
    aes(fill = factor(custom)), 
    lwd = 0,
    alpha = 0.6
  ) +
  geom_segment(
    data = flows_10_black,
    aes(x = x1, xend = x2, y = y1, yend = y2),
    arrow = arrow(angle = 40, length = unit(0.15, "cm")),
    lwd = 0.4
  ) +
  scale_fill_manual(values = custom_pal) +
  facet_wrap(. ~ race_cat) +
  theme_bw() +
  labs(fill = "", x = "", y = "") +
  coord_sf(xlim = c(1744000, 1756000), ylim = c(463000, 480000)) +
  ggsave(here("figures", "ed_class", "flows", "black_eds_1910.png"), width = 10, height = 5)




# plot for all three groups in Black neighborhoods in 1920
ggplot() +
  geom_sf(
    data = filter(eds, year == 1920),
    aes(fill = factor(custom)), 
    lwd = 0,
    alpha = 0.6
  ) +
  geom_segment(
    data = flows_20_black,
    aes(x = x1, xend = x2, y = y1, yend = y2),
    arrow = arrow(angle = 40, length = unit(0.15, "cm")),
    lwd = 0.4
  ) +
  scale_fill_manual(values = custom_pal) +
  facet_wrap(. ~ race_cat) +
  theme_bw() +
  labs(fill = "", x = "", y = "") +
  coord_sf(xlim = c(1744000, 1756000), ylim = c(463000, 480000)) +
  ggsave(here("figures", "ed_class", "flows", "black_eds_1920.png"), width = 10, height = 5)



# map for black households in black neighborhoods in 1910 by destination ED type
ggplot() +
  geom_sf(
    data = filter(eds, year == 1920),
    aes(fill = factor(custom)), 
    lwd = 0,
    alpha = 0.6
  ) +
  geom_segment(
    data = flows_blackn_blackhh_10,
    aes(x = x1, xend = x2, y = y1, yend = y2, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))#,
    #lwd = 0.4
  ) +
  scale_fill_manual(values = custom_pal) +
  scale_size_manual(values = c(0.3, 0.7, 1)) +
  facet_wrap(. ~ custom2, nrow = 1) +
  theme_bw() +
  labs(fill = "", size = "", x = "", y = "") +
  coord_sf(xlim = c(1744000, 1756000), ylim = c(463000, 480000)) +
  ggsave(here("figures", "ed_class", "flows", "1910_blackn_blackhh.png"), width = 10, height = 5)


# map for white households in black neighborhoods in 1910 by destination ED type
ggplot() +
  geom_sf(
    data = filter(eds, year == 1920),
    aes(fill = factor(custom)), 
    lwd = 0,
    alpha = 0.6
  ) +
  geom_segment(
    data = filter(flows_blackn_whitehh_10, custom2 %in% c("Black", "U.S. White")),
    aes(x = x1, xend = x2, y = y1, yend = y2, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))#,
    #lwd = 0.4
  ) +
  scale_fill_manual(values = custom_pal) +
  scale_size_manual(values = c(0.3, 0.7, 1)) +
  facet_wrap(. ~ custom2, nrow = 1) +
  theme_bw() +
  labs(fill = "", size = "", x = "", y = "") +
  coord_sf(xlim = c(1744000, 1756000), ylim = c(463000, 480000)) +
  ggsave(here("figures", "ed_class", "flows", "1910_blackn_whitehh.png"), width = 10, height = 5)

# same plot as above, but plain union of EDs and flows colored by sei
ggplot() +
  geom_sf(
    data = phl,
    col = "black",
    fill = "white", 
    lwd = 0.5
  ) +
  geom_segment(
    data = filter(flows_blackn_whitehh_10, custom2 %in% c("Black", "U.S. White")),
    aes(x = x1, xend = x2, y = y1, yend = y2, col = mean_sei, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))#,
    #lwd = 0.4
  ) +
  scale_fill_manual(values = custom_pal) +
  scale_size_manual(values = c(0.3, 0.7, 1)) +
  facet_wrap(. ~ custom2, nrow = 1) +
  theme_bw() +
  labs(fill = "", size = "", x = "", y = "") +
  coord_sf(xlim = c(1744000, 1756000), ylim = c(463000, 480000)) +
  ggsave(here("figures", "ed_class", "flows", "1910_blackn_whitehh_sei.png"), width = 8, height = 5)



##### Ok, so my next steps are to fully explore attribute patterns in n-to-n flows by racial group
# what are most common and relevant?
# what are most stratified by SEI (between or within groups and kinds of flows)?
# how is move distance distributed across and within these kinds of flows?
# what is most important for creating maps? (obviously some of the maps in these combos aren't impootant at all)

# when doing individual neighborhood-group-year combos as a single paned
# I can include a second panel that has black-white spatial context and colors according to SEI of flow

# perhaps in some of these maps, I should just include the observed ED destinations that make up these arrows


