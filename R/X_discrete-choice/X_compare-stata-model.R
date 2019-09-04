library(here)
library(rio)
library(mclogit)
library(survival)
library(bife)
library(dplyr)
library(readr)
library(RcppNumerical)

#black_data <- import(here("data", "for_models", "test-stata-black_03-25.csv"))
black_data <- import(here("data", "for_models", "b_sample_4-4.csv"))

choices <- select(black_data, choice, serial1, ed1, dest_ed, black, nativity1, 
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
#dc_spec <- cbind(choice, serial1) ~ 
dc_spec <- choice ~
  # time 2 neighborhood variables
  dest_ed_pct_black + dest_ed_pct_frnbrn + dest_ed_mean_sei + dist + dist2 +
  # interactions with % black in T1
  edb1_edb2 + edb1_edf2 + edb1_edsei2 + edb1_dist + edb1_dist2 +
  # interactions with % foreign born in T1
  edf1_edb2 + edf1_edf2 + edf1_edsei2 + edf1_dist + edf1_dist2 +
  # interactions with mean SEI in T1
  edsei1_edb2 + edsei1_edf2 + edsei1_edsei2 + edsei1_dist + edsei1_dist2 + strata(serial1)

# set alternate formula for a simple logit model with a fixed effect
dc_spec_fe <- choice ~
  # time 2 neighborhood variables
  dest_ed_pct_black + dest_ed_pct_frnbrn + dest_ed_mean_sei + dist + dist2 +
  # interactions with % black in T1
  edb1_edb2 + edb1_edf2 + edb1_edsei2 + edb1_dist + edb1_dist2 +
  # interactions with % foreign born in T1
  edf1_edb2 + edf1_edf2 + edf1_edsei2 + edf1_dist + edf1_dist2 +
  # interactions with mean SEI in T1
  edsei1_edb2 + edsei1_edf2 + edsei1_edsei2 + edsei1_dist + edsei1_dist2 + factor(serial1)

### Spec for bife package
bife_spec <- choice ~
  # time 2 neighborhood variables
  dest_ed_pct_black + dest_ed_pct_frnbrn + dest_ed_mean_sei + dist + dist2 +
  # interactions with % black in T1
  edb1_edb2 + edb1_edf2 + edb1_edsei2 + edb1_dist + edb1_dist2 +
  # interactions with % foreign born in T1
  edf1_edb2 + edf1_edf2 + edf1_edsei2 + edf1_dist + edf1_dist2 +
  # interactions with mean SEI in T1
  edsei1_edb2 + edsei1_edf2 + edsei1_edsei2 + edsei1_dist + edsei1_dist2 | serial1

# run conditional logit model
cl_model <- clogit(dc_spec, data = choices)

# run logit FE model
fe_model <- glm(dc_spec_fe, data = choices, family = binomial(link = "logit"))

# run logit FE with bife
bife_model <- bife(bife_spec, data = choices, bias_corr = "no")

# running a simpel fast logistic regression (no FE) with fastLR
fastlr_model <- fastLR(x = as.matrix(choices[,3:29]), y = choices$choice)


