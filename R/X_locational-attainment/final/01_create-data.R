# This script creates the necessary data structure for 
# running LOCATIONAL ATTAINMENT models with the linked Philadelphia samples

# The final data structure has one row for each decade link
# columns have a "1" or a "2" at the end to refer to which time point the data is for
# Only ED-level neighborhood variables are available at this point without geocodes (Dec 4, 2018)

library(here)
library(rio)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(readr)
library(sf)

source(here("R", "functions", "load_linked_sample.R"))
source(here("R", "functions", "aggregate_microdata.R"))

#### Define Columns ####

# define individual-level columns to keep for analysis
ind_cols <- c("uniqueid", "pernum", "year", "serial", "relate", "ed", "hn", "overall_match", "ownershp",
          "age", "sex", "race", "race_grp", "marst", "nchild", "bpl", "nativity", "citizen", "mtongue", "speakeng",
          "school", "lit", "occ1950", "occscore", "sei", "ind1950", "erscor50", "edscor50", "npboss50")
ind_cols <- c(paste0(ind_cols, "1"), paste0(ind_cols, "2"))
ind_cols <- ind_cols[ind_cols %in% c("lit2", "speakeng2") == F]

# define household-level columns to keep and aggregate in the load section
hh_cols <- c("serial", "year", "age", "sex", "race", "nativity", "mtongue", "school", "nchild",
             "labforce", "occ1950", "occscore", "sei", "ind1950", "erscor50", "edscor50", "npboss50")


#### Load Data ####

# create a combined linked sample with specified vars
linked <- here("data", "linked") %>% 
  list.files(full.names = T) %>% 
  map(import) %>% 
  map(select, ind_cols) %>% 
  map(mutate, ed1 = as.character(ed1),
      ed2 = as.character(ed2)) %>%
  bind_rows() %>% 
  # generate dummy vars for household position in time 1
  mutate(not_hhh1 = if_else(relate1 == 101, 0, 1),
         boarder1 = if_else(between(relate1, 1200, 1206), 1, 0),
         employee1 = if_else(between(relate1, 1210, 1217), 1, 0),
         inst1 = if_else(relate1 %in% c(1301, 1223, 1221, 1222), 1, 0),
         rel_child1 = if_else(relate1 %in% c(301, 302, 303, 304, 401, 402, 901, 902, 903, 904, 
                                             1021, 1022, 1031, 1032, 1033, 1034), 1, 0),
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
         hn_same = if_else(hn1 == hn2, 1, 0),
         st_same = if_else(overall_match1 == overall_match2, 1, 0),
         both_same = if_else(hn_same == 1 & st_same == 1, 1, 0))

# select and aggregate household level variables
hh <- c(10, 20, 30, 40) %>% 
  map(load_microdata, formatted = F) %>% 
  map(select, hh_cols) %>% 
  bind_rows() %>% 
  group_by(serial, year) %>% 
  summarize(hh_size = n(),
            hh_max_sei = max(sei),
            hh_sum_sei = sum(sei, na.rm = T),
            hh_n_kids = sum(if_else(age < 18, 1, 0)),
            hh_n_labforce = sum(if_else(labforce == 2, 1, 0)),
            hh_p_labforce = hh_n_labforce / hh_size,
            hh_mean_sei = hh_sum_sei / hh_n_labforce) %>% 
  ungroup()

# load and aggregate data to ED level
ed <- c(10, 20, 30, 40) %>% 
  map(aggregate_microdata, ed) %>% 
  map(mutate, ed = as.character(ed)) %>% 
  bind_rows()
names(ed)[-(1:2)] <- paste0("ed_", names(ed)[-(1:2)])

#### Combine Data ####

# generate T1/T2 specific files for merging
hh1 <- filter(hh, year != 1940)
names(hh1) <- paste0(names(hh1), "1")

hh2 <- filter(hh, year != 1910)
names(hh2) <- paste0(names(hh2), "2")

ed1 <- filter(ed, year != 1940)
names(ed1) <- paste0(names(ed1), "1")

ed2 <- filter(ed, year != 1910)
names(ed2) <- paste0(names(ed2), "2")


### Add Queen contiguity "lag_" values
# these are generated in the ed variables table

# load polygons of 1910-1930 EDs
phl10 <- st_read(here("data", "shertzer_eds", "Philadelphia_1910.shp"))
phl20 <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp"))
phl30 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp"))
phl40 <- st_read(here("data", "shertzer_eds", "Philadelphia_1940.shp"))

# create a named list of polygon files to reference with funciton
polygons1 <- list("1910" = phl10, "1920" = phl20, "1930" = phl30)
polygons2 <- list("1920" = phl20, "1930" = phl30, "1940" = phl40)

# generate lagged values for ED-level vars using queen contiguity (simple intersect)
lag_var1 <- function(ed_num, year){
  # pull polygons for given year
  poly <- polygons1[[as.character(year)]]
  # get ED numbers of touching polygon main ED
  neighs <- suppressWarnings(st_intersection(filter(poly, ED == ed_num), poly)$ED.1)
  # drop focal ED from the lagged calculations, want to measure distinct area diff. from ED
  #neighs <- neighs[!(neighs == ed_num)]
  # filter only these EDs from data and return lagged values (add ed1 and year1 for merging with ed data)
  ed1 %>% 
    filter(year1 == year & ed1 %in% neighs) %>% 
    summarise(lag_pct_black1 = sum(ed_n_black1) / sum(ed_total_pop1) * 100,
              lag_pct_frnbrn1 = sum(ed_n_frnbrn1) / sum(ed_total_pop1) * 100,
              lag_pct_under_151 = sum(ed_n_under_151) / sum(ed_total_pop1) * 100,
              lag_mean_age1 = weighted.mean(x = ed_mean_age1, w = ed_total_pop1),
              lag_mean_sei1 = weighted.mean(x = ed_mean_sei1, w = ed_total_pop1)) %>% 
    mutate(ed1 = ed_num, year1 = year)
}

lag_var2 <- function(ed_num, year){
  # pull polygons for given year
  poly <- polygons2[[as.character(year)]]
  # get ED numbers of touching polygon main ED
  neighs <- suppressWarnings(st_intersection(filter(poly, ED == ed_num), poly)$ED.1)
  # drop focal ED from the lagged calculations, want to measure distinct area diff. from ED
  #neighs <- neighs[!(neighs == ed_num)]
  # filter only these EDs from data and return lagged values (add ed1 and year1 for merging with ed data)
  ed2 %>% 
    filter(year2 == year & ed2 %in% neighs) %>% 
    summarise(lag_pct_black2 = sum(ed_n_black2) / sum(ed_total_pop2) * 100,
              lag_pct_frnbrn2 = sum(ed_n_frnbrn2) / sum(ed_total_pop2) * 100,
              lag_pct_under_152 = sum(ed_n_under_152) / sum(ed_total_pop2) * 100,
              lag_mean_age2 = weighted.mean(x = ed_mean_age2, w = ed_total_pop2),
              lag_mean_sei2 = weighted.mean(x = ed_mean_sei2, w = ed_total_pop2)) %>% 
    mutate(ed2 = ed_num, year2 = year)
}

# map function to ed1 data to create all lagged values for all ED-years
lagged1 <- map2_dfr(ed1$ed1, ed1$year1, lag_var1)
lagged2 <- map2_dfr(ed2$ed2, ed2$year2, lag_var2)

# merged lagged values with ED data
ed1 <- left_join(ed1, lagged1)
ed2 <- left_join(ed2, lagged2)

# execute all merges
combined <- linked %>% 
  left_join(hh1) %>% 
  left_join(hh2) %>% 
  left_join(ed1) %>% 
  left_join(ed2)

# compute differences for each households ED and lagged ED neighborhods across decade
combined <- combined %>% 
  mutate(ed_pct_black_diff = ed_pct_black2 - ed_pct_black1,
         ed_pct_frnbrn_diff = ed_pct_frnbrn2 - ed_pct_frnbrn1,
         ed_mean_sei_diff = ed_mean_sei2 - ed_mean_sei1,
         lag_pct_black_diff = lag_pct_black2 - lag_pct_black1,
         lag_pct_frnbrn_diff = lag_pct_frnbrn2 - lag_pct_frnbrn1,
         lag_mean_sei_diff = lag_mean_sei2 - lag_mean_sei1)

# export new file
write_csv(combined, here("data", "for_models", "phl_loc_attain.csv"))


