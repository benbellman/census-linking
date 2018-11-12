library(dplyr)
library(rio)
library(here)

source(here("R", "functions", "load_linked_sample.R"))

# summarize microdata into ED characteristics in a given decade
aggregate_microdata <- function(m, unit){
  unit <- enquo(unit)
  
  # Make vector of USA birthplaces
  
  
  test <- load_microdata(m) %>% 
  # create dummy chars for different categories/groups (race, sex, age, occ/ind, married)
  mutate(
    # race
    black = if_else(race_grp == "Black", 1, 0),
    white = if_else(race_grp == "White", 1, 0),
    other = if_else(race_grp == "Other", 1, 0),
    # sex
    male = if_else(sex == "Male", 1, 0),
    female = if_else(sex == "Female", 1, 0),
    # age
    age_0_4 = if_else(between(age, 0, 4), 1, 0),
    age_5_9 = if_else(between(age, 5, 9), 1, 0),
    age_10_14 = if_else(between(age, 10, 14), 1, 0),
    age_20_24 = if_else(between(age, 20, 24), 1, 0),
    age_25_29 = if_else(between(age, 25, 29), 1, 0),
    age_30_34 = if_else(between(age, 30, 34), 1, 0),
    age_35_39 = if_else(between(age, 35, 39), 1, 0),
    age_40_44 = if_else(between(age, 40, 44), 1, 0),
    age_45_49 = if_else(between(age, 45, 49), 1, 0),
    age_50_54 = if_else(between(age, 50, 54), 1, 0),
    age_55_59 = if_else(between(age, 55, 59), 1, 0),
    age_60_64 = if_else(between(age, 60, 64), 1, 0),
    age_65_69 = if_else(between(age, 65, 69), 1, 0),
    age_70_74 = if_else(between(age, 70, 74), 1, 0),
    age_75_up = if_else(age > 74, 1, 0)
    # nativity
    #frn_brn = if_else()
  ) %>% 
  group_by(!!unit) %>% 
  #group_by(enumdist) %>% 
  summarise(
    total_pop = n(),
    n_white = sum(white),
    n_black = sum(black),
    n_under_15 = sum(age_0_4) + sum(age_5_9) + sum(age_10_14),
    n_over_64 = sum(age_65_69) + sum(age_70_74) + sum(age_75_up),
    pct_white = n_white / total_pop * 100,
    pct_black = n_black / total_pop * 100,
    pct_under_15 = n_under_15 / total_pop * 100,
    pct_over_64 = n_over_64 / total_pop * 100,
    #mean_sei = mean(sei),
    mean_age = mean(age)
  )
}