library(here)
library(rio)
library(sf)
library(ggplot2)
library(dplyr)
library(mclust)
library(purrr)
library(ghibli)
library(tigris)
library(forcats)
library(tidyr)
library(RColorBrewer)

source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "aggregate_microdata.R"))

source(here("R", "functions", "sept_flow_maps", "create_flows.R"))

#### Load all data ####

# full linked sample
linked <- import(here("data", "analysis", "links-full-info.csv")) %>% 
  mutate(
    move_type = factor(move_type, levels = c("Same Address", "Same Neighborhood", "Left Neighborhood", "Missing Data")),
    home_type1 = factor(home_type1, levels = c("Owner", "Renter", "Not Head", "Missing Data"))#,
    #ed_custom1 = factor(ed_custom1, levels = c("Black", "Mixed - Imm.", "Mixed - U.S.", "Immigrant", "U.S. White")),
    #ed_custom2 = factor(ed_custom2, levels = c("Black", "Mixed - Imm.", "Mixed - U.S.", "Immigrant", "U.S. White"))
  )

# ED polygons
ed10 <- st_read(here("data", "shertzer_eds", "Philadelphia_1910.shp")) %>% 
  mutate(year = 1910, decade = NA)
ed20 <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp")) %>% 
  mutate(year = 1920, decade = "1910-20")
ed30 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp")) %>% 
  mutate(year = 1930, decade = "1920-30")
ed40 <- st_read(here("data", "shertzer_eds", "Philadelphia_1940.shp")) %>% 
  mutate(year = 1940, decade = "1930-40") %>% 
  st_transform(crs = st_crs(ed30))

eds <- rbind(ed10, ed20, ed30, ed40) %>% 
  mutate(ed = as.character(ED)) %>% 
  select(-ED)

# borders of philadelphia
phl <- places(state = 42)
phl <- st_as_sf(phl)
phl <- filter(phl, NAME == "Philadelphia")
phl <- st_transform(phl, crs = st_crs(eds))

# load and merge ed custom classes
ed_classes <- import(here("data", "ed_data", "custom_classes.csv")) %>% as_tibble()

eds <- left_join(eds, ed_classes)

# custom color palette for ed classes
custom_pal <- c(ghibli_palette("KikiLight")[4],
                ghibli_palette("KikiLight")[3],
                "lightgrey")


#### Creating Maps ####


# first, make a basic map of ED categories in all years

ggplot(eds) +
  geom_sf(aes(fill = custom),
          size = 0) +
  facet_wrap(. ~ year, nrow = 2) +
  scale_fill_manual(values = custom_pal) +
  labs(x = "", y = "", fill = "Neighborhood\nType") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  ggsave(here("figures", "new_chp2", "map_ed_categories.png"), height = 8, width = 7)





## I want to make these maps more targeted
# data from all years (color/facet)
# only looking at movers from one race group, between specific types of neighborhoods
# get_flows()
 
# Black movers, black neighborhood origin
bbb10 <- filter(linked, year1 == 1910, race_cat == "Black", move_type == "Left Neighborhood", ed_custom1 == "Black") %>% get_flows() %>% mutate(decade = "1910-20")
bbb20 <- filter(linked, year1 == 1920, race_cat == "Black", move_type == "Left Neighborhood", ed_custom1 == "Black") %>% get_flows() %>% mutate(decade = "1920-30")
bbb30 <- filter(linked, year1 == 1930, race_cat == "Black", move_type == "Left Neighborhood", ed_custom1 == "Black") %>% get_flows() %>% mutate(decade = "1930-40")

bbb <- list(bbb10, bbb20, bbb30) %>% 
  bind_rows() %>%
  mutate(
    race_cat = "Black",
    n_class = case_when(
      n < 300 ~ "Under 300",
      n >= 300 ~ "300 and up"
    ),
    n_class = factor(n_class, levels = c("Under 300", "300 and up"))
  )

ggplot() +
  geom_sf(
    data = filter(eds, is.na(decade) == F),
    aes(fill = custom),
    size = 0
  ) +
  geom_segment(
    data = bbb,
    aes(x = X1, xend = X2, y = Y1, yend = Y2, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))
  ) +
  scale_size_manual(values = c(0.3, 0.5, 0.7)) +
  scale_fill_manual(values = custom_pal) +
  facet_grid(. ~ decade) +
  theme_bw() +
  labs(fill = "", x = "", y = "") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  ggsave(here("figures", "new_chp2", "flowmap_black_b-origin.png"), width = 9, height = 5)



# immigrant movers, black neighborhood origin
ib10 <- filter(linked, year1 == 1910, race_cat == "White Imm", move_type == "Left Neighborhood", ed_custom1 == "Black") %>% get_flows() %>% mutate(decade = "1910-20")
ib20 <- filter(linked, year1 == 1920, race_cat == "White Imm", move_type == "Left Neighborhood", ed_custom1 == "Black") %>% get_flows() %>% mutate(decade = "1920-30")
ib30 <- filter(linked, year1 == 1930, race_cat == "White Imm", move_type == "Left Neighborhood", ed_custom1 == "Black") %>% get_flows() %>% mutate(decade = "1930-40")

ib <- list(ib10, ib20, ib30) %>% 
  bind_rows() %>%
  mutate(
    race_cat = "White Imm",
    n_class = case_when(
      n < 300 ~ "Under 300",
      n >= 300 ~ "300 and up"
    ),
    n_class = factor(n_class, levels = c("Under 300", "300 and up"))
  )

ggplot() +
  geom_sf(
    data = filter(eds, is.na(decade) == F),
    aes(fill = custom),
    size = 0
  ) +
  geom_segment(
    data = ib,
    aes(x = X1, xend = X2, y = Y1, yend = Y2, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))
  ) +
  scale_size_manual(values = c(0.3, 0.5, 0.7)) +
  scale_fill_manual(values = custom_pal) +
  facet_grid(. ~ decade) +
  theme_bw() +
  labs(fill = "", x = "", y = "") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  ggsave(here("figures", "new_chp2", "flowmap_wimm_b-origin.png"), width = 9, height = 5)




# white NB movers, black neighborhood origin
wbw10 <- filter(linked, year1 == 1910, race_cat == "White NB", move_type == "Left Neighborhood", ed_custom1 == "Black") %>% get_flows() %>% mutate(decade = "1910-20")
wbw20 <- filter(linked, year1 == 1920, race_cat == "White NB", move_type == "Left Neighborhood", ed_custom1 == "Black") %>% get_flows() %>% mutate(decade = "1920-30")
wbw30 <- filter(linked, year1 == 1930, race_cat == "White NB", move_type == "Left Neighborhood", ed_custom1 == "Black") %>% get_flows() %>% mutate(decade = "1930-40")

wbw <- list(wbw10, wbw20, wbw30) %>% 
  bind_rows() %>%
  mutate(
    race_cat = "White NB",
    n_class = case_when(
      n < 300 ~ "Under 300",
      n >= 300 ~ "300 and up"
    ),
    n_class = factor(n_class, levels = c("Under 300", "300 and up"))
  )

ggplot() +
  geom_sf(
    data = filter(eds, is.na(decade) == F),
    aes(fill = edcustom),
    size = 0
  ) +
  geom_segment(
    data = wbw,
    aes(x = X1, xend = X2, y = Y1, yend = Y2, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))
  ) +
  scale_size_manual(values = c(0.3, 0.5, 0.7)) +
  scale_fill_manual(values = custom_pal) +
  facet_grid(. ~ decade) +
  labs(fill = "ED Type", size = "Households", x = "", y = "") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "flowmap_wnb_b-origin.png"), width = 9, height = 5)



# plot them all together
ggplot() +
  geom_sf(
    data = filter(eds, is.na(decade) == F),
    aes(fill = custom),
    size = 0
  ) +
  geom_segment(
    data = rbind(bbb, ib, wbw),
    aes(x = X1, xend = X2, y = Y1, yend = Y2, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))
  ) +
  scale_size_manual(values = c(0.3, 0.6)) +
  scale_fill_manual(values = custom_pal) +
  facet_grid(race_cat ~ decade) +
  labs(fill = "", size = "Households", x = "", y = "") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "flowmap_all_b-origin.png"), width = 9, height = 11)






##### Maps that plot observed rates of leaving / moving to specific EDs by group and year
ed_leave_rates <- linked %>% 
  mutate(leave = if_else(move_type == "Left Neighborhood", 1, 0)) %>% 
  group_by(year1, ed1, race_cat) %>% 
  summarize(
    n_hhh = n(),
    n_leave = sum(leave),
    leave_rate = n_leave / n_hhh * 100
  ) %>% 
  ungroup()
  
ed_moveto_rates <- linked %>% 
  filter(move_type == "Left Neighborhood") %>% 
  group_by(year2, race_cat) %>% 
  mutate(n_movers = n()) %>% 
  group_by(year2, ed2, race_cat) %>% 
  summarize(n_moveto = n(), n_movers = max(n_movers)) %>% 
  mutate(moveto_rate = n_moveto / n_movers * 100) %>% 
  ungroup() %>% 
  filter(race_cat != "Other")


#### Leaving maps


## Maps of White NB leaving rates
ed_leave_rates %>% 
  filter(race_cat == "White NB") %>% 
  mutate(
    ed1 = as.character(ed1),
    #year1 = as.numeric(year1),
    leave_cat = case_when(
      n_hhh <= 5 ~ "<6 households",
      leave_rate <= 50 ~ "0 to 50%",
      leave_rate > 50 & leave_rate <= 75 ~ "50 to 75%",
      leave_rate >= 75 ~ "75 to 100%"
    )
  ) %>% 
  left_join(filter(rename(eds, ed1 = ed, year1 = year), year1 != 1940), .) %>% 
  mutate(leave_cat = if_else(is.na(leave_cat), "<6 households", leave_cat)) %>% 
  # start plotting
  ggplot() +
  geom_sf(
    aes(fill = leave_cat), size = 0
  ) +
  scale_fill_manual(values = c("lightgrey", ghibli_palette("MononokeMedium", direction = -1)[1:3])) +
  facet_grid(. ~ year1) +
  labs(fill = "Rates of leaving\nneighborhood") +
  theme_bw() +
  ggsave(here("figures", "new_chp2", "leavemap_wnb.png"), width = 9, height = 5)


## Maps of White Imm leaving rates
ed_leave_rates %>% 
  filter(race_cat == "White Imm") %>% 
  mutate(
    ed1 = as.character(ed1),
    #year1 = as.numeric(year1),
    leave_cat = case_when(
      n_hhh <= 5 ~ "<6 households",
      leave_rate <= 50 ~ "0 to 50%",
      leave_rate > 50 & leave_rate <= 75 ~ "50 to 75%",
      leave_rate >= 75 ~ "75 to 100%"
    )
  ) %>% 
  left_join(filter(rename(eds, ed1 = ed, year1 = year), year1 != 1940), .) %>% 
  mutate(leave_cat = if_else(is.na(leave_cat), "<6 households", leave_cat)) %>% 
  # start plotting
  ggplot() +
  geom_sf(
    aes(fill = leave_cat), size = 0
  ) +
  scale_fill_manual(values = c("lightgrey", ghibli_palette("MononokeMedium", direction = -1)[1:3])) +
  facet_grid(. ~ year1) +
  labs(fill = "Rates of leaving\nneighborhood") +
  theme_bw() +
  ggsave(here("figures", "new_chp2", "leavemap_wimm.png"), width = 9, height = 5)

## Maps of Black leaving rates
ed_leave_rates %>% 
  filter(race_cat == "Black") %>% 
  mutate(
    ed1 = as.character(ed1),
    #year1 = as.numeric(year1),
    leave_cat = case_when(
      n_hhh <= 5 ~ "<6 households",
      leave_rate <= 50 ~ "0 to 50%",
      leave_rate > 50 & leave_rate <= 75 ~ "50 to 75%",
      leave_rate >= 75 ~ "75 to 100%"
    )
  ) %>% 
  left_join(filter(rename(eds, ed1 = ed, year1 = year), year1 != 1940), .) %>% 
  mutate(leave_cat = if_else(is.na(leave_cat), "<6 households", leave_cat)) %>% 
  # start plotting
  ggplot() +
  geom_sf(
    aes(fill = leave_cat), size = 0
  ) +
  scale_fill_manual(values = c("lightgrey", ghibli_palette("MononokeMedium", direction = -1)[1:3])) +
  facet_grid(. ~ year1) +
  labs(fill = "Rates of leaving\nneighborhood") +
  theme_bw() +
  ggsave(here("figures", "new_chp2", "leavemap_black.png"), width = 9, height = 5)  


# try it as one big collage
part1_leave <- ed_leave_rates %>% 
  filter(race_cat == "White NB") %>% 
  mutate(
    ed1 = as.character(ed1),
    #year1 = as.numeric(year1),
    leave_cat = case_when(
      n_hhh <= 5 ~ "<6 households",
      leave_rate <= 50 ~ "0 to 50%",
      leave_rate > 50 & leave_rate <= 75 ~ "50 to 75%",
      leave_rate >= 75 ~ "75 to 100%"
    )
  ) %>% 
  left_join(filter(rename(eds, ed1 = ed, year1 = year), year1 != 1940), .) %>% 
  mutate(leave_cat = if_else(is.na(leave_cat), "<6 households", leave_cat),
         race_cat = if_else(is.na(race_cat), "White NB", race_cat))

part2_leave <- ed_leave_rates %>% 
  filter(race_cat == "White Imm") %>% 
  mutate(
    ed1 = as.character(ed1),
    #year1 = as.numeric(year1),
    leave_cat = case_when(
      n_hhh <= 5 ~ "<6 households",
      leave_rate <= 50 ~ "0 to 50%",
      leave_rate > 50 & leave_rate <= 75 ~ "50 to 75%",
      leave_rate >= 75 ~ "75 to 100%"
    )
  ) %>% 
  left_join(filter(rename(eds, ed1 = ed, year1 = year), year1 != 1940), .) %>% 
  mutate(leave_cat = if_else(is.na(leave_cat), "<6 households", leave_cat),
         race_cat = if_else(is.na(race_cat), "White Imm", race_cat))

part3_leave <- ed_leave_rates %>% 
  filter(race_cat == "Black") %>% 
  mutate(
    ed1 = as.character(ed1),
    #year1 = as.numeric(year1),
    leave_cat = case_when(
      n_hhh <= 5 ~ "<6 households",
      leave_rate <= 50 ~ "0 to 50%",
      leave_rate > 50 & leave_rate <= 75 ~ "50 to 75%",
      leave_rate >= 75 ~ "75 to 100%"
    )
  ) %>% 
  left_join(filter(rename(eds, ed1 = ed, year1 = year), year1 != 1940), .) %>% 
  mutate(leave_cat = if_else(is.na(leave_cat), "<6 households", leave_cat),
         race_cat = if_else(is.na(race_cat), "Black", race_cat))

# plot them all together
rbind(part1_leave, part2_leave, part3_leave) %>% 
  ggplot() +
  geom_sf(
    aes(fill = leave_cat), size = 0
  ) +
  scale_fill_manual(values = c("lightgrey", ghibli_palette("MononokeMedium", direction = -1)[1:3])) +
  facet_grid(race_cat ~ year1) +
  labs(fill = "Rates of leaving\nneighborhood") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "leavemap_all.png"), width = 9, height = 11)


#### Move-to maps


## Maps of White NB leaving rates
ed_moveto_rates %>% 
  filter(race_cat == "White NB") %>% 
  mutate(ed1 = as.character(ed2)) %>% 
  left_join(filter(rename(eds, ed2 = ed, year2 = year), year2 != 1910), .) %>% 
  mutate(moveto_rate = if_else(is.na(moveto_rate), 0, moveto_rate)) %>% 
  # start plotting
  ggplot() +
  geom_sf(
    aes(fill = moveto_rate), size = 0
  ) +
  #scale_fill_continuous(type = "viridis") +
  scale_fill_gradient(low = "lightgrey", high = "#006aff") +
  facet_grid(. ~ year2) +
  labs(fill = "Rates of\nmoving to ED") +
  theme_bw() +
  ggsave(here("figures", "new_chp2", "movetomap_wnb.png"), width = 9, height = 5)


## Maps of White Imm leaving rates
ed_moveto_rates %>% 
  filter(race_cat == "White Imm") %>% 
  mutate(ed1 = as.character(ed2)) %>% 
  left_join(filter(rename(eds, ed2 = ed, year2 = year), year2 != 1910), .) %>% 
  mutate(moveto_rate = if_else(is.na(moveto_rate), 0, moveto_rate)) %>% 
  # start plotting
  ggplot() +
  geom_sf(
    aes(fill = moveto_rate), size = 0
  ) +
  #scale_fill_continuous(type = "viridis") +
  scale_fill_gradient(low = "lightgrey", high = "#006aff") +
  facet_grid(. ~ year2) +
  labs(fill = "Rates of\nmoving to ED") +
  theme_bw() +
  ggsave(here("figures", "new_chp2", "movetomap_wimm.png"), width = 9, height = 5)

## Maps of Black leaving rates
ed_moveto_rates %>% 
  filter(race_cat == "Black") %>% 
  mutate(ed1 = as.character(ed2)) %>% 
  left_join(filter(rename(eds, ed2 = ed, year2 = year), year2 != 1910), .) %>% 
  mutate(moveto_rate = if_else(is.na(moveto_rate), 0, moveto_rate)) %>% 
  # start plotting
  ggplot() +
  geom_sf(
    aes(fill = moveto_rate), size = 0
  ) +
  #scale_fill_continuous(type = "viridis") +
  scale_fill_gradient(low = "lightgrey", high = "#006aff") +
  facet_grid(. ~ year2) +
  labs(fill = "Rates of\nmoving to ED") +
  theme_bw() +
  ggsave(here("figures", "new_chp2", "movetomap_black.png"), width = 9, height = 5)


# Map them all together
part1_moveto <- ed_moveto_rates %>% 
  filter(race_cat == "White NB") %>% 
  mutate(ed1 = as.character(ed2)) %>% 
  left_join(filter(rename(eds, ed2 = ed, year2 = year), year2 != 1910), .) %>% 
  mutate(moveto_rate = if_else(is.na(moveto_rate), 0, moveto_rate),
         race_cat = if_else(is.na(race_cat), "White NB", race_cat))

part2_moveto <- ed_moveto_rates %>% 
  filter(race_cat == "White Imm") %>% 
  mutate(ed1 = as.character(ed2)) %>% 
  left_join(filter(rename(eds, ed2 = ed, year2 = year), year2 != 1910), .) %>% 
  mutate(moveto_rate = if_else(is.na(moveto_rate), 0, moveto_rate),
         race_cat = if_else(is.na(race_cat), "White Imm", race_cat))

part3_moveto <- ed_moveto_rates %>% 
  filter(race_cat == "Black") %>% 
  mutate(ed1 = as.character(ed2)) %>% 
  left_join(filter(rename(eds, ed2 = ed, year2 = year), year2 != 1910), .) %>% 
  mutate(moveto_rate = if_else(is.na(moveto_rate), 0, moveto_rate),
         race_cat = if_else(is.na(race_cat), "Black", race_cat))

rbind(part1_moveto, part2_moveto, part3_moveto) %>% 
  mutate(moveto_cat = factor(case_when(moveto_rate >= 0.5 ~ "0.5% or more",
                                moveto_rate >= 0.1 & moveto_rate < 0.5 ~ "0.1% to 0.5%",
                                TRUE ~ "Less than 0.1%"),
                             levels = c("Less than 0.1%", "0.1% to 0.5%", "0.5% or more"))) %>% 
  ggplot() +
  geom_sf(
    aes(fill = moveto_cat), size = 0
  ) +
  scale_fill_manual(values = c("lightgrey", "#7db3ff", "#006aff")) +
  facet_grid(race_cat ~ year2) +
  labs(fill = "Rates of\nmoving to ED") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "movetomap_all.png"), width = 9, height = 11)



#### Mapping distribution of homeownership and SEI by race and year

own_ed_data <- c(10, 20, 30) %>% 
  map(load_microdata, formatted = F) %>% 
  map(select, year, ed, race, bpl, ownershp, relate) %>% 
  bind_rows() %>% 
  mutate(race_cat = case_when(race %in% c(200, 210) ~ "Black",
                              race == 100 & bpl >= 15000 ~ "White Imm",
                              race == 100 & bpl < 15000 ~ "White NB",
                              TRUE ~ "Other"),
         own = if_else(ownershp == 10, 1, 0)) %>% 
  filter(race_cat != "Other" & relate == 101 & ownershp > 0) %>% 
  select(-c(race, bpl, relate, ownershp)) %>% 
  rename(year1 = year, ED = ed) %>% 
  group_by(year1, ED, race_cat) %>% 
  summarise(N = n(),
            pct_own = sum(own) / N * 100) %>% 
  filter(N > 5)

# merge with polygons
own_ed_data <- own_ed_data %>% 
  group_by(race_cat) %>% 
  group_split() %>% 
  map(right_join, rename(rbind(ed10, ed20, ed30), year1 = year))

# fix empty race_cat values
own_ed_data[[1]]$race_cat <- "Black"
own_ed_data[[2]]$race_cat <- "White Imm"
own_ed_data[[3]]$race_cat <- "White NB"

# combine and create column for mapping
own_ed_data <- rbind(own_ed_data[[1]], own_ed_data[[2]], own_ed_data[[3]]) %>% 
  mutate(pct_own_map = factor(case_when(
    is.na(pct_own) ~ "Too few cases",
    pct_own <= 25 ~ "25% and below",
    pct_own > 25 & pct_own <= 50 ~ "25 to 50%",
    pct_own > 50 ~ "Over 50%"
  ), levels = c("25% and below", "25 to 50%", "Over 50%", "Too few cases")))

# do the maps
ggplot(own_ed_data) +
  geom_sf(aes(fill = pct_own_map), size = 0) +
  scale_fill_manual(values = c(ghibli_palette("LaputaMedium", direction = -1)[3:5], "grey")) +
  facet_grid(year1 ~ race_cat) +
  labs(fill = "% Homeowner") +
  theme_bw() +
  theme(axis.text = element_blank()) +
  ggsave(here("figures", "new_chp3", "maps_pct_owner.png"), height = 11, width = 9)


### Mean SEI
sei_ed_data <- c(10, 20, 30) %>% 
  map(load_microdata, formatted = F) %>% 
  map(select, year, ed, race, bpl, sei) %>% 
  bind_rows() %>% 
  mutate(race_cat = case_when(race %in% c(200, 210) ~ "Black",
                              race == 100 & bpl >= 15000 ~ "White Imm",
                              race == 100 & bpl < 15000 ~ "White NB",
                              TRUE ~ "Other")) %>% 
  filter(race_cat != "Other" & sei > 0) %>% 
  select(-c(race, bpl)) %>% 
  rename(year1 = year, ED = ed) %>% 
  group_by(year1, ED, race_cat) %>% 
  summarise(N = n(),
            mean_sei = mean(sei),
            med_sei = median(sei)) %>% 
  filter(N > 5)

# merge with polygons
sei_ed_data <- sei_ed_data %>% 
  group_by(race_cat) %>% 
  group_split() %>% 
  map(right_join, rename(rbind(ed10, ed20, ed30), year1 = year))

# fix empty race_cat values
sei_ed_data[[1]]$race_cat <- "Black"
sei_ed_data[[2]]$race_cat <- "White Imm"
sei_ed_data[[3]]$race_cat <- "White NB"

# combine and create column for mapping
sei_ed_data <- rbind(sei_ed_data[[1]], sei_ed_data[[2]], sei_ed_data[[3]]) %>% 
  mutate(med_sei_map = factor(case_when(
    is.na(med_sei) ~ "Too few cases",
    med_sei <= 15 ~ "15 and below",
    med_sei > 15 & med_sei <= 25 ~ "15 to 25",
    med_sei > 25 & med_sei <= 35 ~ "25 to 35",
    med_sei > 35 ~ "Over 35"
  ), levels = c("15 and below", "15 to 25", "25 to 35", "Over 35", "Too few cases")))

# do the maps
ggplot(sei_ed_data) +
  geom_sf(aes(fill = med_sei_map), size = 0) +
  scale_fill_manual(values = c(brewer.pal(4, "YlGn"), "grey")) +
  facet_grid(year1 ~ race_cat) +
  labs(fill = "Median SEI") +
  theme_bw() +
  theme(axis.text = element_blank()) +
  ggsave(here("figures", "new_chp3", "maps_mean_sei.png"), height = 11, width = 9)




