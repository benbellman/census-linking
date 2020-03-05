library(here)
library(rio)
library(mclust)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggalluvial)
library(data.table)
library(sf)
library(tigris)
library(ghibli)
library(RColorBrewer)
library(patchwork)

source(here("R", "functions", "load_linked_sample.R"))
source(here("R", "functions", "all_row_combos.R"))
#source(here("R", "functions", "aggregate_microdata.R"))
source(here("R", "functions", "segregation_indices.R"))
source(here("R", "functions", "sept_flow_maps", "create_flows.R"))

#### Set up data ####

# import select variables for full microdata in each year
m10 <- load_microdata(10, formatted = F) %>% 
  #filter(relate == 101) %>% 
  select(year, relate, race, bpl, ed, overall_match, overall_match_bool, street_precleaned, ownershp, sei) %>% 
  mutate(
    race_cat = case_when(
      race == 100 & bpl < 15000 ~ "White NB",
      race == 100 & bpl >= 15000 ~ "White Imm",
      race %in% c(200, 210) ~ "Black",
      TRUE ~ "Other"
    ),
    street = if_else(overall_match_bool == T, overall_match, street_precleaned),
    ed_street = paste0(ed, " - ", street),
    own = if_else(ownershp == 10, 1, 0)
  )

m20 <- load_microdata(20, formatted = F) %>% 
  #filter(relate == 101) %>% 
  select(year, relate, race, bpl, ed, overall_match, overall_match_bool, street_precleaned, ownershp, sei) %>% 
  mutate(
    race_cat = case_when(
      race == 100 & bpl < 15000 ~ "White NB",
      race == 100 & bpl >= 15000 ~ "White Imm",
      race %in% c(200, 210) ~ "Black",
      TRUE ~ "Other"
    ),
    street = if_else(overall_match_bool == T, overall_match, street_precleaned),
    ed_street = paste0(ed, " - ", street),
    own = if_else(ownershp == 10, 1, 0)
  )

m30 <- load_microdata(30, formatted = F) %>% 
  #filter(relate == 101) %>% 
  select(year, relate, race, bpl, ed, overall_match, overall_match_bool, street_precleaned, ownershp, sei) %>% 
  mutate(
    race_cat = case_when(
      race == 100 & bpl < 15000 ~ "White NB",
      race == 100 & bpl >= 15000 ~ "White Imm",
      race %in% c(200, 210) ~ "Black",
      TRUE ~ "Other"
    ),
    street = if_else(overall_match_bool == T, overall_match, street_precleaned),
    ed_street = paste0(ed, " - ", street),
    own = if_else(ownershp == 10, 1, 0)
  )

m40 <- load_microdata(40, formatted = F) %>% 
  #filter(relate == 101) %>% 
  select(year, relate, race, bpl, ed, overall_match, overall_match_bool, street_precleaned, ownershp, sei) %>% 
  mutate(
    race_cat = case_when(
      race == 100 & bpl < 15000 ~ "White NB",
      race == 100 & bpl >= 15000 ~ "White Imm",
      race %in% c(200, 210) ~ "Black",
      TRUE ~ "Other"
    ),
    street = if_else(overall_match_bool == T, overall_match, street_precleaned),
    ed_street = paste0(ed, " - ", street),
    own = if_else(ownershp == 10, 1, 0)
  )

# import linked data for analysis and convert vars to factors with levels
linked <- import(here("data", "analysis", "links-full-info-2.csv")) %>% 
  mutate(
    move_type = factor(move_type, levels = c("Same Address", "Short Move", "Long Move", "Missing Data")),
    home_type1 = factor(home_type1, levels = c("Owner", "Renter", "Missing Data")),
    # create a new variable that also includes if they were household head in time 1
    head_tenure1 = factor(if_else(not_hhh1 == 1 & home_type1 != "Missing Data", 
                                  paste0("NH - ", home_type1), 
                                  as.character(home_type1)),
                          levels = c("Owner", "Renter", "NH - Owner", "NH - Renter", "Missing Data"))#,
    # Create additionally collapsed relate category for non-heads
    #non_head_cat_slim = case_when(
    #  non_head_cat == "Head" ~ "Head",
    #  boarder1 == 1 ~ "Boarder",
    #  employee1 == 1 ~ "Employee",
    #  inst1 == 1 ~ "Institution",
    #  spouse1 == 1 ~ "Spouse",
    #  rel_child1 == 1 ~ "Rel. Child",
    #  other_fam1 == 1 ~ "Other Family",
    #  TRUE ~ "Other"
    #)
  )

# separate into heads and non-heads in time 1
#heads <- filter(linked, not_hhh1 == 0)
#nonheads <- filter(linked, not_hhh1 == 1)

# ED polygons
ed10 <- st_read(here("data", "merged_eds", "Philadelphia_1910.shp")) %>% 
  mutate(year = 1910, decade = NA)
ed20 <- st_read(here("data", "merged_eds", "Philadelphia_1920.shp")) %>% 
  mutate(year = 1920, decade = "1910-20")
ed30 <- st_read(here("data", "merged_eds", "Philadelphia_1930.shp")) %>% 
  mutate(year = 1930, decade = "1920-30")
ed40 <- st_read(here("data", "merged_eds", "Philadelphia_1940.shp")) %>% 
  mutate(year = 1940, decade = "1930-40") %>% 
  st_transform(crs = st_crs(ed30))

eds <- rbind(ed10, ed20, ed30, ed40) %>% 
  mutate(ed = as.character(ED)) %>% 
  select(-ED)

# add custom ED classification based on %black and %frnbrn
eds$ed_custom <- case_when(
  eds$ed_pct_b >= 25 ~ "Black Neigh.",
  eds$ed_pct_b < 25 & eds$ed_pct_f >= 25 ~ "Imm. Neigh.",
  eds$ed_pct_b < 25 & eds$ed_pct_f < 25 ~ "White Neigh."
)

# custom color palette for ed classes
custom_pal <- c(ghibli_palette("KikiLight")[4],
                ghibli_palette("KikiLight")[3],
                "lightgrey")

# borders of philadelphia
#phl <- places(state = 42)
#phl <- st_as_sf(phl)
#phl <- filter(phl, NAME == "Philadelphia")
#phl <- st_transform(phl, crs = st_crs(eds))


#### Figures about demographic + spatial context (part 1) ####

# Figure 1
# share of city population by race over time
rbind(m10, m20, m30, m40) %>% 
  filter(race_cat != "Other") %>% 
  group_by(year) %>% 
  mutate(N = n()) %>% 
  group_by(year, race_cat) %>% 
  mutate(Pct = round(n() / N * 100, 1),
         y_pos = case_when(
           race_cat == "Black" ~ 100 - (Pct * 0.5),
           race_cat == "White NB" ~ Pct * 0.5,
           race_cat == "White Imm" ~ 80
         )) %>% 
  select(year, race_cat, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = year, y = Pct, fill = race_cat)) +
  geom_text(aes(x = year, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("SpiritedMedium", direction = -1)) +
  labs(x = "Year", y = "Cumulative Percent", fill = "Race Category") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "figure1_race_by_year.png"), height = 3, width = 5)

# Actual Figure 2 (need to reorganize code once chapter is done)
# Breakdown of all households heads by year
rbind(m10, m20, m30, m40) %>% 
  filter(race_cat != "Other" & relate == 101) %>% 
  group_by(year) %>% 
  mutate(N = n()) %>% 
  group_by(year, race_cat) %>% 
  mutate(Pct = round(n() / N * 100, 1),
         y_pos = case_when(
           race_cat == "Black" ~ 100 - (Pct * 0.5),
           race_cat == "White NB" ~ Pct * 0.5,
           race_cat == "White Imm" ~ 80
         )) %>% 
  select(year, race_cat, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = year, y = Pct, fill = race_cat)) +
  geom_text(aes(x = year, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("SpiritedMedium", direction = -1)) +
  labs(x = "Year", y = "Cumulative Percent", fill = "Race Category", title = "All Household Heads") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) -> plot_all



# Actual Figure 3 (need to reorganize code once chapter is done)
# Breakdown of linked samples in time 1 by year
linked %>% 
  filter(race_cat != "Other") %>% 
  group_by(year2) %>% 
  mutate(N = n()) %>% 
  group_by(year2, race_cat) %>% 
  mutate(n = n(),
         Pct = round(n / N * 100, 1),
         y_pos = case_when(
           race_cat == "Black" ~ 100 - (Pct * 0.5),
           race_cat == "White NB" ~ Pct * 0.5,
           race_cat == "White Imm" ~ 80
         )) %>% 
  select(year2, race_cat, n, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = year2, y = Pct, fill = race_cat)) +
  geom_text(aes(x = year2, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "% (", format(n, trim = T, big.mark = ","), ")")), size = 3) +
  scale_fill_manual(values = ghibli_palette("SpiritedMedium", direction = -1)) +
  labs(x = "Year (Time 2)", y = "Cumulative Percent", fill = "Race Category", title = "Linked Household Heads") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) -> plot_linked


plot_all + plot_linked + 
  plot_layout(guides = 'collect') + 
  ggsave(here("figures", "new_chp2", "figure2_heads-race-by-year.png"), height = 4, width = 8)




# Figure 2
# Map showing racial neighborhood categories in each year
ggplot(eds) +
  geom_sf(aes(fill = ed_custom),
          size = 0) +
  facet_wrap(. ~ year, nrow = 2) +
  scale_fill_manual(values = custom_pal) +
  labs(x = "", y = "", fill = "") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure2_map-ed-categories.png"), height = 7, width = 7)


# Figure 3
# maps of ED % black for each year
eds %>% 
  mutate(
    pct_b_cat = factor(case_when(
      ed_pct_b >= 90 ~ ">90%",
      ed_pct_b < 90 & ed_pct_b >= 50 ~ "50% - 90%",
      ed_pct_b < 50 & ed_pct_b >= 25 ~ "25% - 50%",
      ed_pct_b < 25 & ed_pct_b >= 5 ~ "5% - 25%",
      ed_pct_b < 5 ~ "<5%"
    ), levels = c("<5%", "5% - 25%", "25% - 50%", "50% - 90%", ">90%"))
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = pct_b_cat), size = 0) +
  scale_fill_brewer(palette = "PuBuGn") +
  facet_wrap(. ~ year) +
  labs(fill = "Percent Black") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(458000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure3_map-ed-pct-black.png"), height = 7, width = 7)



# Figure 4
# Segregation indices of household heads over time
# write a function that computes all desired indices for data from a single year
calc_all_index <- function(rows){
  bind_rows(
    tibble(index = "D wb", unit = "ED", value = diss(rows, ed, race_cat, "White NB", "Black")),
    #tibble(index = "D wb", unit = "ED-street", value = diss(rows, ed_street, race_cat, "White NB", "Black")),
    tibble(index = "D wi", unit = "ED", value = diss(rows, ed, race_cat, "White NB", "White Imm")),
    #tibble(index = "D wi", unit = "ED-street", value = diss(rows, ed_street, race_cat, "White NB", "White Imm")),
    tibble(index = "D bi", unit = "ED", value = diss(rows, ed, race_cat, "Black", "White Imm")),
    #tibble(index = "D bi", unit = "ED-street", value = diss(rows, ed_street, race_cat, "Black", "White Imm")),
    tibble(index = "p* ww", unit = "ED", value = pstar(rows, ed, race_cat, "White NB", "White NB")),
    #tibble(index = "p* ww", unit = "ED-street", value = pstar(rows, ed_street, race_cat, "White NB", "White NB")),
    tibble(index = "p* ii", unit = "ED", value = pstar(rows, ed, race_cat, "White Imm", "White Imm")),
    #tibble(index = "p* ii", unit = "ED-street", value = pstar(rows, ed_street, race_cat, "White Imm", "White Imm")),
    tibble(index = "p* bb", unit = "ED", value = pstar(rows, ed, race_cat, "Black", "Black"))#,
    #tibble(index = "p* bb", unit = "ED-street", value = pstar(rows, ed_street, race_cat, "Black", "Black"))
  )
}

# group by year and apply function for all indices
indices <- list(
  calc_all_index(filter(m10, relate == 101)) %>% mutate(year = 1910),
  calc_all_index(filter(m20, relate == 101)) %>% mutate(year = 1920),
  calc_all_index(filter(m30, relate == 101)) %>% mutate(year = 1930),
  calc_all_index(filter(m40, relate == 101)) %>% mutate(year = 1940)
) %>% bind_rows()

# plot them
ggplot(indices) +
  geom_path(aes(x = year, y = value, col = index), size = 1.5) +
  #facet_grid(. ~ unit) +
  labs(x = "Year", y = "Value", col = "Index") +
  scale_color_manual(values = ghibli_palette("KikiMedium")[2:7]) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure4_segregation-indices.png"), height = 3, width = 5)


# Figure 5
# maps of % homeowner by race and year
own_ed_data <- rbind(m10, m20, m30) %>% 
  filter(race_cat != "Other" & relate == 101 & ownershp > 0) %>% 
  rename(year1 = year, ED = ed) %>% 
  group_by(year1, ED, race_cat) %>% 
  summarise(N = n(),
            pct_own = sum(own) / N * 100) %>% 
  filter(N > 5) %>% 
  # merge with polygons
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
  geom_sf(aes(geometry = geometry, fill = pct_own_map), size = 0) +
  scale_fill_manual(values = c(ghibli_palette("LaputaMedium", direction = -1)[3:5], "grey")) +
  facet_grid(year1 ~ race_cat) +
  labs(fill = "% Homeowner") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(458000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure5_maps-pct-owner.png"), height = 11, width = 9)


# Figure 6
# maps of median SEI by race and year
sei_ed_data <- rbind(m10, m20, m30) %>% 
  filter(race_cat != "Other" & sei > 0) %>% 
  rename(year1 = year, ED = ed) %>% 
  group_by(year1, ED, race_cat) %>% 
  summarise(N = n(),
            mean_sei = mean(sei),
            med_sei = median(sei)) %>% 
  filter(N > 5) %>% 
  # merge with polygons
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
  geom_sf(aes(geometry = geometry, fill = med_sei_map), size = 0) +
  scale_fill_manual(values = c(brewer.pal(4, "YlGn"), "grey")) +
  facet_grid(year1 ~ race_cat) +
  labs(fill = "Median SEI") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(458000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure6_maps-median-sei.png"), height = 11, width = 9)


#### Figures describing demographics of linked sample (part 2) ####

# Figure 7
# housing tenure of linked household heads by race and year
linked %>% 
  filter(race_cat != "Other") %>% 
  group_by(decade, race_cat) %>% 
  mutate(n_grp = n()) %>% 
  group_by(decade, race_cat, head_tenure1) %>% 
  mutate(n_home = n(), 
         Pct = round(n_home / n_grp * 100, 1),
         y_pos = case_when(
           head_tenure1 == "Owner" ~ 100 - (Pct * 0.5),
           race_cat == "Black" & head_tenure1 == "Renter" ~ 62,
           race_cat == "Black" & head_tenure1 == "NH - Owner" ~ 35,
           race_cat == "Black" & head_tenure1 == "NH - Renter" ~ 15,
           race_cat == "White Imm" & head_tenure1 == "Renter" & decade == "1910-20" ~ 50,
           race_cat == "White Imm" & head_tenure1 == "Renter" & decade == "1920-30" ~ 38,
           race_cat == "White Imm" & head_tenure1 == "Renter" & decade == "1930-40" ~ 30,
           race_cat == "White Imm" & head_tenure1 == "NH - Owner" & decade == "1910-20" ~ 17,
           race_cat == "White Imm" & head_tenure1 == "NH - Owner" & decade == "1920-30" ~ 14,
           race_cat == "White Imm" & head_tenure1 == "NH - Owner" & decade == "1930-40" ~ 11,
           race_cat == "White Imm" & head_tenure1 == "NH - Renter" ~ Pct * 0.5,
           race_cat == "White NB" & head_tenure1 == "Renter" & decade == "1910-20" ~ 50,
           race_cat == "White NB" & head_tenure1 == "Renter" & decade == "1920-30" ~ 45,
           race_cat == "White NB" & head_tenure1 == "Renter" & decade == "1930-40" ~ 40,
           race_cat == "White NB" & head_tenure1 == "NH - Owner" & decade == "1910-20" ~ 23,
           race_cat == "White NB" & head_tenure1 == "NH - Owner" & decade == "1920-30" ~ 20,
           race_cat == "White NB" & head_tenure1 == "NH - Owner" & decade == "1930-40" ~ 19,
           race_cat == "White NB" & head_tenure1 == "NH - Renter" ~ Pct * 0.5
         )
  ) %>% 
  select(decade, year1, race_cat, head_tenure1, n_grp, n_home, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = year1, y = Pct, fill = head_tenure1)) +
  geom_text(aes(x = year1, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("YesterdayLight", direction = -1)) +
  facet_grid(. ~ race_cat) +
  labs(x = "Year", y = "Cumulative Percent", fill = "Housing Tenure") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure7_tenure-by-race.png"), height = 3, width = 6)


# Figure 8
# Breakdown of non-heads by race and year
linked %>% 
  filter(not_hhh1 == 1 & race_cat != "Other" & home_type1 != "Missing Data") %>% 
  group_by(year1, race_cat, home_type1) %>% 
  mutate(N = n()) %>% 
  group_by(year1, race_cat, home_type1, non_head_cat) %>% 
  mutate(
    Pct = round(n() / N * 100, 1),
    y_pos = case_when(
      non_head_cat == "Boarder" ~ 100 - (Pct * 0.5),
      non_head_cat == "Spouse" ~ Pct * 0.5,
      non_head_cat == "Rel. Child" & race_cat == "Black" ~ 40,
      non_head_cat == "Rel. Child" & race_cat == "White Imm" ~ 60,
      non_head_cat == "Rel. Child" & race_cat == "White NB" ~ 50,
      non_head_cat == "Employee" & race_cat == "Black" & year1 == 1910 & home_type1 == "Owner" ~ 65
    )
  ) %>% 
  select(year1, race_cat, home_type1, non_head_cat, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = year1, y = Pct, fill = non_head_cat)) +
  geom_text(aes(x = year1, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(home_type1 ~ race_cat) +
  labs(x = "Year", y = "Cumulative Percent", fill = "") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure8_non-head-breakdown.png"), height = 5, width = 5)


# Figure 9
# Distributions of SEI by race, year, and housing tenure
sei_means <- filter(linked, race_cat != "Other" & head_tenure1 != "Missing Data") %>% 
  group_by(race_cat, year1, head_tenure1) %>% 
  summarise(sei = round(mean(sei1), 1)) %>%
  mutate(
    y_pos = case_when(
      head_tenure1 == "Owner" ~ 0.19,
      head_tenure1 == "Renter" ~ 0.155,
      head_tenure1 == "NH - Owner" ~ 0.12,
      head_tenure1 == "NH - Renter" ~ 0.085
    )
  )

linked %>% 
  filter(race_cat != "Other" & head_tenure1 != "Missing Data") %>% 
  ggplot() +
  geom_density(aes(x = sei1, fill = head_tenure1, col = head_tenure1), alpha = 0.3) +
  geom_vline(data = sei_means,
             aes(xintercept = sei, col = head_tenure1)) +
  geom_label(data = sei_means,
             aes(x = 70, y = y_pos, label = paste0("Mean = ", sei), fill = head_tenure1),
             size = 3, alpha = 0.5) +
  scale_fill_manual(values = ghibli_palette("YesterdayMedium", direction = -1)) +
  scale_color_manual(values = ghibli_palette("YesterdayMedium", direction = -1)) +
  facet_grid(race_cat ~ year1) +
  ylim(0, 0.2) +
  labs(x = "SEI", y = "Density", col = "Housing\nTenure", fill = "Housing\nTenure") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure9_sei-by-race-year-tenure.png"), height = 5, width = 7)

# Figure 10
# Age distributions by race and housing tenure
age_means <- filter(linked, race_cat != "Other" & head_tenure1 != "Missing Data") %>% 
  group_by(race_cat, year1, head_tenure1) %>% 
  summarise(age = round(mean(age1), 1)) %>%
  mutate(
    x_pos = case_when(
      head_tenure1 == "Owner" ~ 20,
      head_tenure1 == "Renter" ~ 20,
      head_tenure1 == "NH - Owner" ~ 65,
      head_tenure1 == "NH - Renter" ~ 65
    ),
    y_pos = case_when(
      head_tenure1 == "Owner" ~ 0.062,
      head_tenure1 == "Renter" ~ 0.052,
      head_tenure1 == "NH - Owner" ~ 0.062,
      head_tenure1 == "NH - Renter" ~ 0.052
    )
  )

linked %>% 
  filter(race_cat != "Other" & head_tenure1 != "Missing Data") %>% 
  ggplot() +
  geom_density(aes(x = age1, fill = head_tenure1, col = head_tenure1), alpha = 0.3) +
  geom_label(data = age_means,
             aes(x = x_pos, y = y_pos, label = paste0("Mean = ", age), fill = head_tenure1),
             size = 2.5, alpha = 0.5) +
  scale_fill_manual(values = ghibli_palette("YesterdayMedium", direction = -1)) +
  scale_color_manual(values = ghibli_palette("YesterdayMedium", direction = -1)) +
  facet_grid(race_cat ~ year1) +
  labs(x = "Age", y = "Density", col = "Housing\nTenure", fill = "Housing\nTenure") +
  ylim(0, 0.065) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure10_age-by-race-year-tenure.png"), height = 5, width = 7)


# Figure 11
# Neighborhood type in time 1 by housing tenure
linked %>% 
  filter(race_cat != "Other" & head_tenure1 %in% c("Owner", "Renter") & is.na(ed_custom1) == F) %>% 
  group_by(year1, race_cat, head_tenure1) %>% 
  mutate(n_home = n()) %>% 
  group_by(year1, race_cat, head_tenure1, ed_custom1) %>% 
  mutate(n_cust = n(), 
         Pct = round(n_cust / n_home * 100, 1),
         y_pos = case_when(
           ed_custom1 == "Black Neigh." ~ 100 - (Pct * 0.5),
           race_cat == "Black" & ed_custom1 == "White Neigh." ~ (Pct * 0.5),
           race_cat == "White Imm" & ed_custom1 == "Imm. Neigh." ~ 70,
           race_cat == "White NB" & ed_custom1 == "Imm. Neigh." & head_tenure1 == "Owner" ~ 90,
           race_cat == "White NB" & ed_custom1 == "Imm. Neigh." ~ 85,
           race_cat == "White Imm" & ed_custom1 == "White Neigh." ~ 20,
           race_cat == "White NB" & ed_custom1 == "White Neigh." ~ 40
         )) %>% 
  select(year1, race_cat, head_tenure1, ed_custom1, n_home, n_cust, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = year1, y = Pct, fill = ed_custom1)) +
  geom_text(aes(x = year1, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = custom_pal) +
  facet_grid(head_tenure1 ~ race_cat) +
  labs(x = "Year", y = "Cumulative Percent", fill = "Neighborhood Type") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure11_neigh-by-race-year-tenure.png"), height = 4, width = 7)


#### Figures describing residential mobility outcomes (part 3) ####

# Figure 12
# Types of moves by race
linked %>% 
  filter(race_cat != "Other" & head_tenure1 %in% c("Owner", "Renter")) %>% 
  group_by(decade, race_cat) %>% 
  mutate(n_grp = n()) %>% 
  group_by(decade, race_cat, move_type) %>% 
  mutate(n_move = n(), 
         Pct = round(n_move / n_grp * 100, 1),
         y_pos = case_when(
           move_type == "Same Address" ~ 100 - (Pct * 0.5),
           move_type == "Long Move" ~ Pct * 0.5,
           move_type == "Short Move" & race_cat == "Black" ~ 65,
           move_type == "Short Move" & race_cat == "White Imm" & year1 == 1910 ~ 65,
           move_type == "Short Move" & race_cat == "White Imm" & year1 == 1920 ~ 60,
           move_type == "Short Move" & race_cat == "White Imm" & year1 == 1930 ~ 45,
           move_type == "Short Move" & race_cat == "White NB" & year1 == 1910 ~ 65,
           move_type == "Short Move" & race_cat == "White NB" & year1 == 1920 ~ 60,
           move_type == "Short Move" & race_cat == "White NB" & year1 == 1930 ~ 50
         )) %>% 
  select(decade, year1, race_cat, move_type, n_grp, n_move, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = decade, y = Pct, fill = move_type)) +
  geom_text(aes(x = decade, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("LaputaMedium", direction = -1)) +
  facet_grid(. ~ race_cat) +
  labs(x = "Decade", y = "Cumulative Percent", fill = "Move Type") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "figure12_moves-by-race-year.png"), height = 4, width = 7)

# Figure 13
# Types of moves by race and housing tenure
linked %>% 
  filter(head_tenure1 %in% c("Owner", "Renter") & race_cat != "Other") %>% 
  group_by(race_cat, decade, head_tenure1) %>% 
  mutate(grp_yr_total = n()) %>% 
  group_by(race_cat, decade, head_tenure1, move_type) %>% 
  mutate(Pct = round(n() / grp_yr_total * 100, 1),
         y_pos = case_when(
           move_type == "Same Address" ~ 100 - (Pct * 0.5),
           move_type == "Short Move" & head_tenure1 == "Owner" & decade == "1910-20" ~ 50,
           move_type == "Short Move" & head_tenure1 == "Owner" & decade == "1920-30" ~ 45,
           move_type == "Short Move" & head_tenure1 == "Owner" & decade == "1930-40" ~ 40,
           move_type == "Short Move" & head_tenure1 == "Renter" ~ 70,
           #move_type == "Short Move" & head_tenure1 == "NH - Owner" & race_cat == "Black" ~ 75,
           #move_type == "Short Move" & head_tenure1 == "NH - Owner" & race_cat != "Black" ~ 60,
           #move_type == "Short Move" & head_tenure1 == "NH - Renter" ~ 80,
           move_type == "Long Move" ~ Pct * 0.6
         )) %>% 
  select(race_cat, decade, head_tenure1, move_type, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = decade, y = Pct, fill = move_type)) +
  geom_text(aes(x = decade, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("LaputaMedium", direction = -1)) +
  facet_grid(head_tenure1 ~ race_cat) +
  labs(x = "Decade", y = "Cumulative Percent", fill = "Type of Move") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "figure13_moves-by-race-year-tenure.png"), height = 4, width = 7)


# Figure 14
# Types of moves by Race and neighborhood types
linked %>% 
  filter(race_cat != "Other" & move_type != "Missing Data" & is.na(ed_custom1) == F & head_tenure1 %in% c("Owner", "Renter")) %>% 
  group_by(year1, race_cat, ed_custom1) %>% 
  mutate(n_cust = n()) %>% 
  group_by(year1, race_cat, ed_custom1, move_type) %>% 
  mutate(n_move = n(), 
         Pct = round(n_move / n_cust * 100, 1),
         y_pos = case_when(
           move_type == "Long Move" ~ Pct * 0.5,
           move_type == "Same Address" ~ 100 - (Pct * 0.5),
           race_cat == "Black" & ed_custom1 == "Black Neigh." ~ 65,
           race_cat == "Black" & ed_custom1 == "Imm. Neigh." ~ 80,
           race_cat == "Black" & ed_custom1 == "White Neigh." ~ 75,
           race_cat == "White Imm" & year1 %in% c(1910, 1920) ~ 65,
           race_cat == "White Imm" & year1 == 1930 ~ 50,
           race_cat == "White NB" ~ 60
         )) %>% 
  select(decade, race_cat, ed_custom1, move_type, n_cust, n_move, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = decade, y = Pct, fill = move_type)) +
  geom_text(aes(x = decade, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("LaputaMedium", direction = -1)) +
  facet_grid(ed_custom1 ~ race_cat) +
  labs(x = "Decade", y = "Cumulative Percent", fill = "Type of Move") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "figure14_moves-by-race-and-neigh.png"), width = 7, height = 6)


# Figure 15
# white moves by tenure and neighborhood
linked %>% 
  filter(race_cat %in% c("White NB", "White Imm") & home_type1 != "Missing Data" & move_type != "Missing Data" & is.na(ed_custom1) == F & head_tenure1 %in% c("Owner", "Renter")) %>% 
  group_by(year1, head_tenure1, ed_custom1) %>% 
  mutate(n_grp = n()) %>% 
  group_by(year1, head_tenure1, ed_custom1, move_type) %>% 
  mutate(n_move = n(), 
         Pct = round(n_move / n_grp * 100, 1),
         y_pos = case_when(
           move_type == "Long Move" ~ Pct * 0.5,
           move_type == "Same Address" ~ 100 - (Pct * 0.5),
           head_tenure1 == "Owner" & year1 %in% c(1910, 1920) ~ 45,
           head_tenure1 == "Owner" & year1 == 1930 ~ 35,
           head_tenure1 == "Renter" ~ 70,
           head_tenure1 == "NH - Owner" ~ 62,
           head_tenure1 == "NH - Renter" ~ 80
         )) %>% 
  select(decade, head_tenure1, ed_custom1, move_type, n_grp, n_move, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = decade, y = Pct, fill = move_type)) +
  geom_text(aes(x = decade, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("LaputaMedium", direction = -1)) +
  facet_grid(head_tenure1 ~ ed_custom1) +
  labs(x = "Decade", y = "Cumulative Percent", fill = "Type of Move") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "figure15_white-moves-by-neigh-tenure.png"), width = 7, height = 4)


#### Analysis of long moves (part 4) ####

# Figure 16
# ED maps of long move rates by year and race
ed_long_rates <- linked %>% 
  mutate(long = if_else(move_type == "Long Move", 1, 0)) %>% 
  group_by(year1, ed1, race_cat) %>% 
  summarize(
    n_hhh = n(),
    n_long = sum(long),
    long_rate = n_long / n_hhh * 100
  ) %>% 
  ungroup()

part1_long <- ed_long_rates %>% 
  filter(race_cat == "White NB") %>% 
  mutate(
    ed1 = as.character(ed1),
    #year1 = as.numeric(year1),
    long_cat = case_when(
      n_hhh <= 5 ~ "<6 households",
      long_rate <= 25 ~ "0 to 25%",
      long_rate > 25 & long_rate <= 50 ~ "25 to 50%",
      long_rate > 50 & long_rate <= 75 ~ "50 to 75%",
      long_rate >= 75 ~ "75 to 100%"
    )
  ) %>% 
  left_join(filter(rename(eds, ed1 = ed, year1 = year), year1 != 1940), .) %>% 
  mutate(long_cat = if_else(is.na(long_cat), "<6 households", long_cat),
         race_cat = if_else(is.na(race_cat), "White NB", race_cat))

part2_long <- ed_long_rates %>% 
  filter(race_cat == "White Imm") %>% 
  mutate(
    ed1 = as.character(ed1),
    #year1 = as.numeric(year1),
    long_cat = case_when(
      n_hhh <= 5 ~ "<6 households",
      long_rate <= 25 ~ "0 to 25%",
      long_rate > 25 & long_rate <= 50 ~ "25 to 50%",
      long_rate > 50 & long_rate <= 75 ~ "50 to 75%",
      long_rate >= 75 ~ "75 to 100%"
    )
  ) %>% 
  left_join(filter(rename(eds, ed1 = ed, year1 = year), year1 != 1940), .) %>% 
  mutate(long_cat = if_else(is.na(long_cat), "<6 households", long_cat),
         race_cat = if_else(is.na(race_cat), "White Imm", race_cat))

part3_long <- ed_long_rates %>% 
  filter(race_cat == "Black") %>% 
  mutate(
    ed1 = as.character(ed1),
    #year1 = as.numeric(year1),
    long_cat = case_when(
      n_hhh <= 5 ~ "<6 households",
      long_rate <= 25 ~ "0 to 25%",
      long_rate > 25 & long_rate <= 50 ~ "25 to 50%",
      long_rate > 50 & long_rate <= 75 ~ "50 to 75%",
      long_rate >= 75 ~ "75 to 100%"
    )
  ) %>% 
  left_join(filter(rename(eds, ed1 = ed, year1 = year), year1 != 1940), .) %>% 
  mutate(long_cat = if_else(is.na(long_cat), "<6 households", long_cat),
         race_cat = if_else(is.na(race_cat), "Black", race_cat))

# plot them all together
rbind(part1_long, part2_long, part3_long) %>% 
  ggplot() +
  geom_sf(
    aes(fill = long_cat), size = 0
  ) +
  scale_fill_manual(values = c("lightgrey", brewer.pal(4, "OrRd"))) +
  facet_grid(race_cat ~ year1) +
  labs(fill = "% of Linked Records in Group\nThat Made Long Move") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure16_map-long-move-rates.png"), width = 9, height = 9)

# Figure 17
# ED maps of long dist mover destionation proportions by race and year
ed_moveto_rates <- linked %>% 
  filter(move_type == "Long Move") %>% 
  group_by(year2, race_cat) %>% 
  mutate(n_movers = n()) %>% 
  group_by(year2, ed2, race_cat) %>% 
  summarize(n_moveto = n(), n_movers = max(n_movers)) %>% 
  mutate(moveto_rate = n_moveto / n_movers * 100) %>% 
  ungroup() %>% 
  filter(race_cat != "Other")

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
  labs(fill = "% of Group's Long Moves") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure17_map-long-move-dests.png"), width = 9, height = 9)


### Setting up residential flow maps

linked %>% 
  # filter linked data for long moves only and three main racial groups
  filter(race_cat != "Other" & move_type == "Long Move" & is.na(ed_custom1) == F) %>% 
  # select only the needed vars for creating flows (grouping vars, X1, X2, Y1, Y2, sei1, ownershp1)
  select(decade, race_cat, ed_custom1, X1, Y1, X2, Y2, sei1, ownershp1) %>% 
  # nest linked data by: decade, race_cat, ed_custom1
  group_by(decade, race_cat, ed_custom1) %>% 
  nest() %>% 
  # create a new column of flow data
  mutate(flows = map(data, get_flows)) %>% 
  # add classification of how many households in flow for arrow width 
  select(-data) %>% 
  unnest(cols = c(flows)) %>% 
  mutate(
    n_class = case_when(
      n < 300 ~ "Under 300",
      n >= 300 & n < 1500 ~ "300 to 1,500",
      n >= 1500 ~ "1,500 and up"
    ),
    n_class = factor(n_class, levels = c("Under 300", "300 to 1,500", "1,500 and up"))
  ) -> flows

# Figure 18
# maps of households leaving black neighborhoods
ggplot() +
  geom_sf(
    data = filter(eds, is.na(decade) == F),
    aes(fill = ed_custom),
    size = 0
  ) +
  geom_segment(
    data = filter(flows, race_cat == "Black"),
    aes(x = X1, xend = X2, y = Y1, yend = Y2, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))
  ) +
  scale_size_manual(values = c(0.3, 0.5, 0.7)) +
  scale_fill_manual(values = custom_pal) +
  facet_grid(ed_custom1 ~ decade) +
  labs(fill = "Neighborhood Type", size = "Households", x = "", y = "") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure18_flowmaps-black.png"), width = 9, height = 9)

# Figure 19
# maps of households leaving immigrant neighborhoods
ggplot() +
  geom_sf(
    data = filter(eds, is.na(decade) == F),
    aes(fill = ed_custom),
    size = 0
  ) +
  geom_segment(
    data = filter(flows, race_cat == "White Imm"),
    aes(x = X1, xend = X2, y = Y1, yend = Y2, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))
  ) +
  scale_size_manual(values = c(0.3, 0.5, 0.7)) +
  scale_fill_manual(values = custom_pal) +
  facet_grid(ed_custom1 ~ decade) +
  labs(fill = "Neighborhood Type", size = "Households", x = "", y = "") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure19_flowmaps-wimm.png"), width = 9, height = 9)


# Figure 20
# maps of households leaving white neighborhoods
ggplot() +
  geom_sf(
    data = filter(eds, is.na(decade) == F),
    aes(fill = ed_custom),
    size = 0
  ) +
  geom_segment(
    data = filter(flows, race_cat == "White NB"),
    aes(x = X1, xend = X2, y = Y1, yend = Y2, size = n_class),
    arrow = arrow(angle = 40, length = unit(0.15, "cm"))
  ) +
  scale_size_manual(values = c(0.3, 0.5, 0.7)) +
  scale_fill_manual(values = custom_pal) +
  facet_grid(ed_custom1 ~ decade) +
  labs(fill = "Neighborhood Type", size = "Households", x = "", y = "") +
  coord_sf(xlim = c(1740000, 1756000), ylim = c(460000, 480000)) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  ggsave(here("figures", "new_chp2", "figure20_flowmaps-wnb.png"), width = 9, height = 9)

# Figure 21
# Alluvial diagram of all long move
# create necessary data structure
alluv <- linked %>% 
  filter(race_cat != "Other" & (is.na(ed_custom1) == F) & (is.na(ed_custom2) == F) & move_type != "Missing Data") %>% 
  group_by(move_type, ed_custom1, ed_custom2, race_cat, decade) %>% 
  summarize(flow_n = n()) %>% 
  group_by(move_type, race_cat, decade) %>% 
  mutate(flow_pct = flow_n / sum(flow_n) * 100)

# black
filter(alluv, race_cat != "Other" & move_type == "Long Move") %>% 
  group_by(decade, race_cat) %>% 
  mutate(total = format(sum(flow_n), big.mark = ",")) %>% 
  ggplot(aes(y = flow_pct, axis1 = ed_custom1, axis2 = ed_custom2)) +
  geom_flow(aes(fill = ed_custom2), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", label.strata = TRUE, size = 4) +
  geom_text(aes(x = 1.5, y = 105, label = paste0("N = ", total)), size = 4) +
  scale_fill_manual(values = c(custom_pal[1:2], "grey")) +
  scale_x_discrete(limits = c("Origin", "Destination")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  facet_grid(race_cat ~ decade) +
  labs(x = "",
       y = "Percent of householders",
       fill = "Destination type") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") +
  ggsave(here("figures", "new_chp2", "figure21_alluv-all-long-moves.png"), height = 11, width = 9)

# quick table to view % within ed1 types
linked %>% 
  filter(race_cat != "Other" & (is.na(ed_custom1) == F) & (is.na(ed_custom2) == F) & move_type != "Missing Data") %>% 
  group_by(move_type, ed_custom1, ed_custom2, race_cat, decade) %>% 
  summarize(flow_n = n()) %>% 
  group_by(move_type, race_cat, decade, ed_custom1) %>% 
  mutate(flow_pct = flow_n / sum(flow_n) * 100) %>% 
  filter(move_type == "Long Move") %>% 
  View()












#### Finally, some descriptive maps for spatial/group context


#### Mapping distribution of homeownership and SEI by race and year




### Mean SEI
