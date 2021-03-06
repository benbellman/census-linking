# This script creates the necessary data structure for 
# running DISCRETE CHOICE models with the linked Philadelphia samples

# The final data structure has one row for each decade link
# columns have a "1" or a "2" at the end to refer to which time point the data is for

library(here)
library(rio)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(readr)

source(here("R", "functions", "load_linked_sample.R"))
source(here("R", "functions", "aggregate_microdata.R"))
source(here("R", "functions", "all_row_combos.R"))

#### Define Columns ####

# define individual-level columns to keep for analysis
ind_cols <- c("uniqueid", "pernum", "year", "serial", "relate", "ed", 
              "age", "sex", "race", "race_grp", "marst", "nchild", "bpl", "nativity", "citizen", "mtongue", "speakeng",
              "school", "lit", "occ1950", "occscore", "sei", "ind1950", "erscor50", "edscor50", "npboss50")
ind_cols <- c(paste0(ind_cols, "1"), paste0(ind_cols, "2"))
ind_cols <- ind_cols[ind_cols %in% c("lit2", "speakeng2") == F]

# define household-level columns to keep and aggregate in the load section
hh_cols <- c("serial", "year", "age", "sex", "race", "nativity", "mtongue", "school", "nchild",
             "labforce", "occ1950", "occscore", "sei", "ind1950", "erscor50", "edscor50", "npboss50")


#### Load Data ####

# create a combined linked sample with specified vars
#linked <- here("data", "linked") %>% 
#  list.files(full.names = T) %>% 
#  map(import) %>% 
#  map(select, ind_cols) %>% 
#  map(mutate, ed1 = as.character(ed1),
#      ed2 = as.character(ed2)) %>%
#  bind_rows() %>% 

## just running for 20-30 to keep data small for now
linked <- import(here("data", "linked", "linked_20_30")) %>% 
  # keep individual variables
  select(ind_cols) %>% 
  # set EDs to character vector
  mutate(ed1 = as.character(ed1),
         ed2 = as.character(ed2),
         # create dummy variables for calculations
         not_hhh1 = if_else(relate1 == 101, 0, 1),
         boarder1 = if_else(between(relate1, 1200, 1206), 1, 0),
         employee1 = if_else(between(relate1, 1210, 1217), 1, 0),
         inst1 = if_else(relate1 %in% c(1301, 1223, 1221, 1222), 1, 0),
         rel_child1 = if_else(relate1 %in% c(301, 302, 303, 304, 401, 402, 901, 902, 903, 904, 
                                             1021, 1022, 1031, 1032, 1033, 1034), 1, 0),
         # make race variable based on both years of link
         black = if_else(race_grp1 == "Black" | race_grp2 == "Black", 1, 0),
         other = if_else(race_grp1 == "Other" | race_grp2 == "Other", 1, 0))

# select and aggregate household level variables
#hh <- c(10, 20, 30, 40) %>% 
hh <- c(20, 30) %>% 
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
#ed <- c(10, 20, 30, 40) %>% 
ed <- c(20, 30) %>% 
  map(aggregate_microdata, ed) %>% 
  map(mutate, ed = as.character(ed)) %>% 
  bind_rows()

names(ed)[-(1:2)] <- paste0("ed_", names(ed)[-(1:2)])

#### Combine Data ####

# generate T1/T2 specific files for merging
#hh1 <- filter(hh, year != 1940)
#names(hh1) <- paste0(names(hh1), "1")

#hh2 <- filter(hh, year != 1910)
#names(hh2) <- paste0(names(hh2), "2")

#ed1 <- filter(ed, year != 1940)
#names(ed1) <- paste0(names(ed1), "1")

#ed2 <- filter(ed, year != 1910)
#names(ed2) <- paste0("dest_", names(ed2))
# split into a list
#dest <- split(ed2, ed2$dest_year)

## execute all merges

# create sets of data by census in a list
#origin <- linked %>% 
#  left_join(hh1) %>% 
  #left_join(hh2) %>% 
#  left_join(ed1) %>% 
#  split(.$year2)

# combine each year with possible destination EDs for that year
#combined <- map2(origin, dest, all_row_combos)

#combined <- list()
#for(a in 1:3){
#  combined[[a]] <- origin[[a]] %>% 
#    all_row_combos(filter(ed2, dest_year == .$year2))
#}

#test <- bind_rows(combined)

# export new file
write_csv(combined[[1]], here("data", "for_models", "phl_discrete_choice_20.csv"))
write_csv(combined[[2]], here("data", "for_models", "phl_discrete_choice_30.csv"))
write_csv(combined[[3]], here("data", "for_models", "phl_discrete_choice_40.csv"))

