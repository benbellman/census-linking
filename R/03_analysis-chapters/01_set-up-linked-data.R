library(here)
library(rio)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(readr)
library(sf)

source(here("R", "functions", "load_linked_sample.R"))
#source(here("R", "functions", "aggregate_microdata.R"))

# define individual level variables for analysis
ind_cols <- c("uniqueid", "pernum", "year", "serial", "relate", 
              "ed", "hn", "overall_match", "street_precleaned",
              "age", "sex", "race", "race_grp", "nativity", "marst", "nchild", "bpl", "sei", "occ1950", "ownershp")
ind_cols <- c(paste0(ind_cols, "1"), paste0(ind_cols, "2"))

# define household-level columns to keep and aggregate in the load section
agg_cols <- c("serial", "ed", "overall_match", "year", "age", "sex", "race", "race_grp", "bpl", "nchild", "sei", "labforce")

#### Set Up Population Data ####

# create a combined linked sample with specified vars
linked <- here("data", "linked") %>% 
  list.files(full.names = T) %>% 
  map(import) %>% 
  map(select, ind_cols) %>% 
  map(mutate, ed1 = as.character(ed1),
      ed2 = as.character(ed2)) %>%
  bind_rows() %>% 
  # generate dummy vars for household position in time 1
  mutate(woman = sex1 - 1,
         married = if_else(marst1 < 3, 1, 0),
         not_hhh1 = if_else(relate1 == 101, 0, 1),
         boarder1 = if_else(between(relate1, 1200, 1206), 1, 0),
         employee1 = if_else(between(relate1, 1210, 1217), 1, 0),
         inst1 = if_else(relate1 %in% c(1301, 1223, 1221, 1222), 1, 0),
         spouse1 = if_else(relate1 %in% c(201, 202), 1, 0),
         rel_child1 = if_else(relate1 %in% c(301, 302, 303, 304, 401, 402, 901, 902, 903, 904, 
                                             1031, 1032, 1033, 1034, 1051), 1, 0),
         other_fam1 = if_else(relate1 %in% c(501, 502, 601, 602, 701, 702, 801, 802, 1001, 1011,
                                             1012, 1013, 1021, 1022, 1041, 1042, 1061), 1, 0),
         non_head_cat = case_when(
           not_hhh1 == 0 ~ "Head",
           boarder1 == 1 ~ "Boarder",
           employee1 == 1 ~ "Employee",
           inst1 == 1 ~ "Institution",
           spouse1 == 1 ~ "Spouse",
           rel_child1 == 1 ~ "Rel. Child",
           other_fam1 == 1 ~ "Other Family",
           TRUE ~ "Other"
         ),
         # make race variable based on both years of link
         black = if_else(race_grp1 == "Black" | race_grp2 == "Black", 1, 0),
         white = if_else(race_grp1 == "White" & race_grp2 == "White", 1, 0),
         frnbrn = if_else(nativity1 == 5 | nativity2 == 5, 1, 0),
         race_cat = case_when(
           black == 1 ~ "Black",
           white == 1 & frnbrn == 0 ~ "White NB",
           white == 1 & frnbrn == 1 ~ "White Imm",
           TRUE ~ "Other"
         ),
         # strip direction from fuzzy matched street names
         overall_match1 = str_remove(overall_match1, "^[NSEW][EW]? "),
         overall_match2 = str_remove(overall_match2, "^[NSEW][EW]? "),
         hn_same = if_else(hn1 == hn2, 1, 0),
         st_same = if_else(overall_match1 == overall_match2, 1, 0),
         both_same = if_else(hn_same == 1 & st_same == 1, 1, 0),
         # create ED-street variable in both time points
         edst1 = paste(ed1, overall_match1, sep = " - "),
         edst2 = paste(ed2, overall_match2, sep = " - "))

# get every microdata record with needed vars
micro <- c(10, 20, 30, 40) %>% 
  map(load_microdata, formatted = F) %>% 
  map(select, agg_cols) %>% 
  bind_rows() %>% 
  mutate(
    black = if_else(race_grp == "Black", 1, 0),
    frnbrn = if_else(bpl >= 15000, 1, 0),
    overall_match = str_remove(overall_match, "^[NSEW][EW]? "),
    edst = paste(ed, overall_match, sep = " - "),
    sei = ifelse(sei == 0, NA, sei)
  )

# aggregate household level variables
hh <- micro %>% 
  group_by(serial, year) %>% 
  summarize(hh_size = n(),
            hh_max_sei = max(sei),
            hh_sum_sei = sum(sei, na.rm = T),
            hh_n_kids = sum(if_else(age < 18, 1, 0)),
            hh_n_labforce = sum(if_else(labforce == 2, 1, 0)),
            hh_p_labforce = hh_n_labforce / hh_size,
            hh_mean_sei = hh_sum_sei / hh_n_labforce) %>% 
  ungroup()

# aggregate ED-street level variables
edst <- micro %>% 
  group_by(edst, year) %>% 
  summarize(
    total_pop = n(),
    n_black = sum(black),
    n_frnbrn = sum(frnbrn),
    pct_black = n_black / total_pop * 100,
    pct_frnbrn = n_frnbrn / total_pop * 100,
    mean_sei = mean(sei, na.rm = T),
    mean_age = mean(age)
  ) %>% 
  ungroup()
names(edst)[-(1:2)] <- paste0("edst_", names(ed)[-(1:2)])


# aggregate ED level variables
ed <- micro %>% 
  group_by(ed, year) %>% 
  summarize(
    total_pop = n(),
    n_black = sum(black),
    n_frnbrn = sum(frnbrn),
    pct_black = n_black / total_pop * 100,
    pct_frnbrn = n_frnbrn / total_pop * 100,
    mean_sei = mean(sei, na.rm = T),
    mean_age = mean(age)
  ) %>% 
  ungroup()
names(ed)[-(1:2)] <- paste0("ed_", names(ed)[-(1:2)])

# aggregate
#ed <- c(10, 20, 30, 40) %>% 
#  map(aggregate_microdata, ed) %>% 
#  map(mutate, ed = as.character(ed)) %>% 
#  bind_rows()
#names(ed)[-(1:2)] <- paste0("ed_", names(ed)[-(1:2)])

# add custom ED classification based on %black and %frnbrn
ed$ed_custom <- case_when(
  ed$ed_pct_black >= 25 ~ "Black Neigh.",
  ed$ed_pct_black < 25 & ed$ed_pct_frnbrn >= 25 ~ "Imm. Neigh.",
  ed$ed_pct_black < 25 & ed$ed_pct_frnbrn < 25 ~ "White Neigh."
)

#### Load Additional Data ####

# Creating ED centroids from polygon files, join with ED data
# ED polygons
ed10 <- st_read(here("data", "shertzer_eds", "Philadelphia_1910.shp"))
ed20 <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp"))
ed30 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp"))
ed40 <- st_read(here("data", "shertzer_eds", "Philadelphia_1940.shp")) %>% st_transform(crs = st_crs(ed30))
ed10$year <- 1910
ed20$year <- 1920
ed30$year <- 1930
ed40$year <- 1940

# combine polygons into single table
polys <- rbind(ed10, ed20, ed30, ed40) %>% 
  mutate(ed = as.character(ED)) %>% 
  select(-ED)

# convert polygons into centroid points
points <- bind_cols(
  as_tibble(st_drop_geometry(polys)),
  as_tibble(st_coordinates(st_centroid(polys)))
)

# average distances where ED is made up of more than 1 polygon (error or otherwise)
points <- points %>% 
  group_by(year, ed) %>% 
  summarise(X = mean(X, na.rm = T), Y = mean(Y, na.rm = T)) %>% 
  ungroup()

# join points to ED data
ed <- left_join(ed, points)

# import and prepare distances of moves
move_dist <- list(
  here("data", "ed_distances_10_20.csv"),
  here("data", "ed_distances_20_30.csv"),
  here("data", "ed_distances_30_40.csv")
) %>% 
  map(import)

# add years
move_dist[[1]]$year1 <- 1910
move_dist[[2]]$year1 <- 1920
move_dist[[3]]$year1 <- 1930

# average distances where ED is made up of more than 1 polygon (error or otherwise)
move_dist <- move_dist %>% 
  map(mutate, ed1 = as.character(ed1), ed2 = as.character(dest_ed)) %>% 
  map(select, -dest_ed) %>% 
  bind_rows() %>% 
  group_by(year1, ed1, ed2) %>% 
  summarise(dist = mean(dist)) %>% 
  ungroup()


#### Combine Data ####


# generate T1/T2 specific files for merging
hh1 <- filter(hh, year != 1940)
names(hh1) <- paste0(names(hh1), "1")
hh2 <- filter(hh, year != 1910)
names(hh2) <- paste0(names(hh2), "2")

edst1 <- filter(edst, year != 1940)
names(edst1) <- paste0(names(edst1), "1")
edst2 <- filter(edst, year != 1910)
names(edst2) <- paste0(names(edst2), "2")

ed1 <- filter(ed, year != 1940)
names(ed1) <- paste0(names(ed1), "1")
ed2 <- filter(ed, year != 1910)
names(ed2) <- paste0(names(ed2), "2")



# execute all merges
combined <- linked %>% 
  left_join(hh1) %>% 
  left_join(hh2) %>% 
  left_join(edst1) %>% 
  left_join(edst2) %>% 
  left_join(ed1) %>% 
  left_join(ed2) %>% 
  left_join(move_dist)


#### Last Changes and Export ####

# drop householders whose household status in time 1 was military, student, religious order, or inmate
#combined <- filter(combined, inst1 == 0)

# recompute hh_max_sei1 as sei1 value when boarder1 == 1, employee1 == 1, or...
combined$hh_max_sei1 <- ifelse(combined$boarder1 == 1 | combined$employee1 == 1, 
                               combined$sei1, 
                               combined$hh_max_sei1)

# drop householders where hh_max_sei1 == 0
#combined <- filter(combined, hh_max_sei1 > 0)

# classify move types based on distance and address
combined$move_type <- case_when(combined$both_same == 1 ~ "Same Address",
                                combined$both_same == 0 & combined$dist <= 750 ~ "Short Move",
                                combined$both_same == 0 & combined$dist > 750 ~ "Long Move",
                                TRUE ~ "Missing Data")

# classify links by household head and homeownership status in time 1
combined$home_type1 <- case_when(combined$ownershp1 == 10 ~ "Owner",
                                 combined$ownershp1 == 20 ~ "Renter",
                                 TRUE ~ "Missing Data")

# create new "decade" column for use in plots
combined$decade <- case_when(combined$year1 == 1910 ~ "1910-20",
                             combined$year1 == 1920 ~ "1920-30",
                             combined$year1 == 1930 ~ "1930-40")

# save data file
write_csv(combined, here("data", "analysis", "links-full-info-3.csv"))







