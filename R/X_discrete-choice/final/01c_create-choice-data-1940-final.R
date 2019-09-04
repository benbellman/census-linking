library(here)
library(rio)
library(dplyr)
library(ggplot2)
library(purrr)
library(furrr)
library(stringr)
library(readr)
library(sf)

source(here("R", "functions", "load_linked_sample.R"))
source(here("R", "functions", "aggregate_microdata.R"))
source(here("R", "functions", "all_row_combos.R"))
#source(here("R", "functions", "mapping", "flow_map_by_race.R"))

options(scipen = 999)

#### Define Columns ####

# define individual-level columns to keep for analysis
ind_cols <- c("uniqueid", "year", "serial", "relate", "ed",  "hn", "overall_match",
              "age", "sex", "race", "race_grp", "marst", "nchild", "nativity",
              "school", "sei")
ind_cols <- c(paste0(ind_cols, "1"), paste0(ind_cols, "2"))

# define household-level columns to keep and aggregate in the load section
hh_cols <- c("serial", "year", "age", "sei")


#### Load Data ####

# create a combined linked sample with specified vars
linked <- here("data", "linked", "linked_30_40.csv") %>% 
  import() %>% 
  select(ind_cols) %>% 
  mutate(ed1 = as.character(ed1),
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
         both_same = if_else(hn_same == 1 & st_same == 1, 1, 0),
         # variables from locational attainment data creation
         moved = if_else(both_same == 1, 0, 1),
         woman = sex1 - 1,
         married = if_else(marst1 < 3, 1, 0)) %>% 
  # finally, filter out cases I don't want to analyze
  filter(sei1 > 0 & not_hhh1 == 0 & moved == 1)

# select and aggregate household level variables
hh1 <- load_microdata(30, formatted = F) %>% 
  select(hh_cols) %>% 
  bind_rows() %>% 
  group_by(serial, year) %>% 
  summarize(hh_max_sei = max(sei)) %>% 
  ungroup()
# set names
names(hh1) <- paste0(names(hh1), "1")

hh2 <- load_microdata(40, formatted = F) %>% 
  select(hh_cols) %>% 
  bind_rows() %>% 
  group_by(serial, year) %>% 
  summarize(hh_max_sei = max(sei)) %>% 
  ungroup()
# set names
names(hh2) <- paste0(names(hh2), "2")

# load aggregate data at ED level
ed1 <- import(here("data", "ed_data", "ED_data_1930.csv")) %>% 
  mutate(ed = as.character(ed))
names(ed1) <- paste0(names(ed1), "1")

ed2 <- import(here("data", "ed_data", "ED_data_1940.csv")) %>% 
  mutate(ed = as.character(ed))
names(ed2) <- paste0("dest_", names(ed2))



#### Combine Data ####

# set function that takes a single linked row, vector of EDs in time 2:
# - generates a 10% sample of destinations that aren't it's actual destination,
# - sets these as additional rows, and
# - creates a "choice" dummy variable for actual destination ED
generate_ed_choices <- function(linked_row, ed_numbers){
  ed_choice <- linked_row[1, "ed2"]
  ed_numbers_less1 <- ed_numbers[ed_numbers != ed_choice]
  
  ed_sample <- sample(ed_numbers_less1, round(length(ed_numbers_less1) * 0.1))
  ed_sample <- c(ed_choice, ed_sample)
  
  all_row_combos(linked_row, tibble(dest_ed = ed_sample)) %>% 
    mutate(choice = if_else(ed2 == dest_ed, 1, 0))
}

# get vector of populated ED numbers for 1930
ed_numbers <- ed2$dest_ed

# set up multiple cores for speed
plan(tweak(multiprocess, workers = 32))

# split rows into a list
split(linked, row.names(linked)) %>% 
  #.[1:10] %>% 
  # map generate_ed_choices
  future_map(generate_ed_choices, ed_numbers) %>% 
  # combine rows for full set of sampled destinations
  bind_rows() %>% 
  # coerce
  mutate() -> choices

# load 1920-1930 ED distance matrix
distances <- import(here("data", "ed_distances_30_40.csv")) %>% 
  mutate(dest_ed = as.character(dest_ed),
         ed1 = as.character(ed1),
         dist = if_else(dist > 0 & dist < 1, 1, dist))

# add all household and ed-level variables
choices %>% 
  left_join(hh1) %>% 
  left_join(hh2) %>% 
  left_join(ed1) %>% 
  left_join(ed2) %>% 
  left_join(distances) -> choices_full

# drop cases where the acutal move has missing distance
dist_miss <- choices_full %>% 
  filter(choice == 1 & is.na(dist)) %>% 
  pull(serial1) %>% 
  unique()

choices_full <- filter(choices_full, serial1 %in% dist_miss == F)

# now drop all remaining cases with a missing distance value
choices_full <- filter(choices_full, is.na(dist) == F)

# compute and select columns for estimating and interpreting models
choices_full <- choices_full %>% 
  mutate(
    # create squared terms
    dest_ed_pct_black_sq = dest_ed_pct_black * dest_ed_pct_black,
    dest_ed_pct_frnbrn_sq = dest_ed_pct_frnbrn * dest_ed_pct_frnbrn,
    dest_ed_mean_sei_sq = dest_ed_mean_sei * dest_ed_mean_sei,
    dist_sq = dist * dist,
    # create all interactions from time 1
    # ED % black
    bla1_pblack_ed = ed_pct_black1 * dest_ed_pct_black,
    bla1_pblack_ed_sq = ed_pct_black1 * dest_ed_pct_black_sq,
    bla1_pfrnbrn_ed = ed_pct_black1 * dest_ed_pct_frnbrn,
    bla1_pfrnbrn_ed_sq = ed_pct_black1 * dest_ed_pct_frnbrn_sq,
    bla1_msei_ed = ed_pct_black1 * dest_ed_mean_sei,
    bla1_msei_ed_sq = ed_pct_black1 * dest_ed_mean_sei_sq,
    bla1_dist = ed_pct_black1 * dist,
    bla1_dist_sq = ed_pct_black1 * dist_sq,
    # ED % immigrant
    imm1_pblack_ed = ed_pct_frnbrn1 * dest_ed_pct_black,
    imm1_pblack_ed_sq = ed_pct_frnbrn1 * dest_ed_pct_black_sq,
    imm1_pfrnbrn_ed = ed_pct_frnbrn1 * dest_ed_pct_frnbrn,
    imm1_pfrnbrn_ed_sq = ed_pct_frnbrn1 * dest_ed_pct_frnbrn_sq,
    imm1_msei_ed = ed_pct_frnbrn1 * dest_ed_mean_sei,
    imm1_msei_ed_sq = ed_pct_frnbrn1 * dest_ed_mean_sei_sq,
    imm1_dist = ed_pct_frnbrn1 * dist,
    imm1_dist_sq = ed_pct_frnbrn1 * dist_sq,
    # ED mean SEI
    sei1_pblack_ed = ed_mean_sei1 * dest_ed_pct_black,
    sei1_pblack_ed_sq = ed_mean_sei1 * dest_ed_pct_black_sq,
    sei1_pfrnbrn_ed = ed_mean_sei1 * dest_ed_pct_frnbrn,
    sei1_pfrnbrn_ed_sq = ed_mean_sei1 * dest_ed_pct_frnbrn_sq,
    sei1_msei_ed = ed_mean_sei1 * dest_ed_mean_sei,
    sei1_msei_ed_sq = ed_mean_sei1 * dest_ed_mean_sei_sq,
    sei1_dist = ed_mean_sei1 * dist,
    sei1_dist_sq = ed_mean_sei1 * dist_sq,
    # Individual SEI
    s_pblack_ed = hh_max_sei1 * dest_ed_pct_black,
    s_pblack_ed_sq = hh_max_sei1 * dest_ed_pct_black_sq,
    s_pfrnbrn_ed = hh_max_sei1 * dest_ed_pct_frnbrn,
    s_pfrnbrn_ed_sq = hh_max_sei1 * dest_ed_pct_frnbrn_sq,
    s_msei_ed = hh_max_sei1 * dest_ed_mean_sei,
    s_msei_ed_sq = hh_max_sei1 * dest_ed_mean_sei_sq,
    s_dist = hh_max_sei1 * dist,
    s_dist_sq = hh_max_sei1 * dist_sq,
    # Individual SEI with all time 1 ED interactions
    s_bla1_pblack_ed = hh_max_sei1 * bla1_pblack_ed,
    s_bla1_pblack_ed_sq = hh_max_sei1 * bla1_pblack_ed_sq,
    s_bla1_pfrnbrn_ed = hh_max_sei1 * bla1_pfrnbrn_ed,
    s_bla1_pfrnbrn_ed_sq = hh_max_sei1 * bla1_pfrnbrn_ed_sq,
    s_bla1_msei_ed = hh_max_sei1 * bla1_msei_ed,
    s_bla1_msei_ed_sq = hh_max_sei1 * bla1_msei_ed_sq,
    s_bla1_dist = hh_max_sei1 * bla1_dist,
    s_bla1_dist_sq  = hh_max_sei1 * bla1_dist_sq,
    # ED % immigrant
    s_imm1_pblack_ed = hh_max_sei1 * imm1_pblack_ed,
    s_imm1_pblack_ed_sq = hh_max_sei1 * imm1_pblack_ed_sq,
    s_imm1_pfrnbrn_ed = hh_max_sei1 * imm1_pfrnbrn_ed,
    s_imm1_pfrnbrn_ed_sq = hh_max_sei1 * imm1_pfrnbrn_ed_sq,
    s_imm1_msei_ed = hh_max_sei1 * imm1_msei_ed,
    s_imm1_msei_ed_sq = hh_max_sei1 * imm1_msei_ed_sq,
    s_imm1_dist = hh_max_sei1 * imm1_dist,
    s_imm1_dist_sq = hh_max_sei1 * imm1_dist_sq,
    # ED mean SEI
    s_sei1_pblack_ed = hh_max_sei1 * sei1_pblack_ed,
    s_sei1_pblack_ed_sq = hh_max_sei1 * sei1_pblack_ed_sq,
    s_sei1_pfrnbrn_ed = hh_max_sei1 * sei1_pfrnbrn_ed,
    s_sei1_pfrnbrn_ed_sq = hh_max_sei1 * sei1_pfrnbrn_ed_sq,
    s_sei1_msei_ed = hh_max_sei1 * sei1_msei_ed,
    s_sei1_msei_ed_sq = hh_max_sei1 * sei1_msei_ed_sq,
    s_sei1_dist = hh_max_sei1 * sei1_dist,
    s_sei1_dist_sq = hh_max_sei1 * sei1_dist_sq
  ) %>% 
  # keep only variables used in models
  select(
    # variables for identifying households
    serial1, year1, race_cat, ed1, dest_ed, 
    # original source variables for all model terms
    choice, hh_max_sei1, ed_pct_black1, ed_pct_frnbrn1, ed_mean_sei1,
    dest_ed_pct_black, dest_ed_pct_frnbrn, dest_ed_mean_sei, dist,
    # quadratic terms
    dest_ed_pct_black_sq, dest_ed_pct_frnbrn_sq, dest_ed_mean_sei_sq, dist_sq,
    # all interactions
    bla1_pblack_ed, bla1_pblack_ed_sq, bla1_pfrnbrn_ed, bla1_pfrnbrn_ed_sq,
    bla1_msei_ed, bla1_msei_ed_sq, bla1_dist, bla1_dist_sq,
    # 
    imm1_pblack_ed, imm1_pblack_ed_sq, imm1_pfrnbrn_ed, imm1_pfrnbrn_ed_sq,
    imm1_msei_ed, imm1_msei_ed_sq, imm1_dist, imm1_dist_sq,
    # 
    sei1_pblack_ed, sei1_pblack_ed_sq, sei1_pfrnbrn_ed, sei1_pfrnbrn_ed_sq,
    sei1_msei_ed, sei1_msei_ed_sq, sei1_dist, sei1_dist_sq,
    # Individual SEI
    s_pblack_ed, s_pblack_ed_sq, s_pfrnbrn_ed, s_pfrnbrn_ed_sq,
    s_msei_ed, s_msei_ed_sq, s_dist, s_dist_sq,
    # Individual SEI with all time 1 ED interactions
    s_bla1_pblack_ed, s_bla1_pblack_ed_sq, s_bla1_pfrnbrn_ed, s_bla1_pfrnbrn_ed_sq,
    s_bla1_msei_ed, s_bla1_msei_ed_sq, s_bla1_dist, s_bla1_dist_sq,
    # ED % immigrant
    s_imm1_pblack_ed, s_imm1_pblack_ed_sq, s_imm1_pfrnbrn_ed, s_imm1_pfrnbrn_ed_sq,
    s_imm1_msei_ed, s_imm1_msei_ed_sq, s_imm1_dist, s_imm1_dist_sq,
    # ED mean SEI
    s_sei1_pblack_ed, s_sei1_pblack_ed_sq, s_sei1_pfrnbrn_ed, s_sei1_pfrnbrn_ed_sq,
    s_sei1_msei_ed, s_sei1_msei_ed_sq, s_sei1_dist, s_sei1_dist_sq
    
  )



# export choice data by race category
choices_full %>% 
  filter(race_cat == "Black") %>% 
  write_csv(here("data", "for_models", "final-phl_discrete_choice_black_40.csv"))

choices_full %>% 
  filter(race_cat == "White Imm") %>% 
  write_csv(here("data", "for_models", "final-phl_discrete_choice_wimm_40.csv"))

choices_full %>% 
  filter(race_cat == "White NB") %>% 
  write_csv(here("data", "for_models", "final-phl_discrete_choice_wnb_40.csv"))



