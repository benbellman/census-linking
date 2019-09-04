library(here)
library(rio)
library(dplyr)
library(mclogit)
library(broom)
library(readr)
library(purrr)

# load test data
choices <- import(here("data", "for_models", "dev-test-dc.csv"))

# select data for models
choices_sel <- select(choices, choice, serial1, ed1, dest_ed, black, nativity1, 
                  ed_pct_black1, ed_pct_frnbrn1, ed_mean_sei1, 
                  dest_ed_pct_black, dest_ed_pct_frnbrn, dest_ed_mean_sei, dist) %>% 
  # generate new variables and interactions
  mutate(dist2 = dist*dist,
         edb1_edb2 = ed_pct_black1*dest_ed_pct_black,
         edb1_edf2 = ed_pct_black1*dest_ed_pct_frnbrn,
         edb1_edsei2 = ed_pct_black1*dest_ed_mean_sei,
         edb1_dist = ed_pct_black1*dist,
         edb1_dist2 = ed_pct_black1*dist2,
         edf1_edb2 = ed_pct_frnbrn1*dest_ed_pct_black,
         edf1_edf2 = ed_pct_frnbrn1*dest_ed_pct_frnbrn,
         edf1_edsei2 = ed_pct_frnbrn1*dest_ed_mean_sei,
         edf1_dist = ed_pct_frnbrn1*dist,
         edf1_dist2 = ed_pct_frnbrn1*dist2,
         edsei1_edb2 = ed_mean_sei1*dest_ed_pct_black,
         edsei1_edf2 = ed_mean_sei1*dest_ed_pct_frnbrn,
         edsei1_edsei2 = ed_mean_sei1*dest_ed_mean_sei,
         edsei1_dist = ed_mean_sei1*dist,
         edsei1_dist2 = ed_mean_sei1*dist2)

# set model formula
dc_spec <- cbind(choice, serial1) ~  
  # time 2 neighborhood variables
  dest_ed_pct_black + dest_ed_pct_frnbrn + dest_ed_mean_sei + dist + dist2 +
  # interactions with % black in T1
  edb1_edb2 + edb1_edf2 + edb1_edsei2 + edb1_dist + edb1_dist2 +
  # interactions with % foreign born in T1
  edf1_edb2 + edf1_edf2 + edf1_edsei2 + edf1_dist + edf1_dist2 +
  # interactions with mean SEI in T1
  edsei1_edb2 + edsei1_edf2 + edsei1_edsei2 + edsei1_dist + edsei1_dist2
  
# run conditional logit model
test_model <- mclogit(dc_spec, data = choices_sel)


summary(test_model)
str(test_model)
names(test_model)

