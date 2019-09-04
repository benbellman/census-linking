library(here)
library(rio)
library(dplyr)
library(mclogit)
library(broom)
library(readr)
library(purrr)

# Load discrete choice data
choice_data <- list(import(here("data", "for_models", "phl_discrete_choice_20.csv")),
                    import(here("data", "for_models", "phl_discrete_choice_30.csv")),
                    import(here("data", "for_models", "phl_discrete_choice_40.csv")))
names(choice_data[[1]])

choice_data <- choice_data %>% 
  map(mutate, ed1 = as.character(ed1), ed2 = as.character(ed2), ed_dest = as.character(ed_dest)) %>% 
  # combine into single table (will split later)
  bind_rows()



dc_full <- cbind(choice,serial1) ~ 
  # destination variables
  dest_lag_pct_black + lag_pct_black_diff +
  dest_lag_pct_frnbrn + lag_pct_frnbrn_diff +
  dest_lag_mean_sei + lag_mean_sei_diff +
  dist + dist_sq +
  # interactions with max household SEI in time 1
  hh_max_sei1*dest_lag_pct_black + hh_max_sei1*lag_pct_black_diff +
  hh_max_sei1*dest_lag_pct_frnbrn + hh_max_sei1*lag_pct_frnbrn_diff +
  hh_max_sei1*dest_lag_mean_sei + hh_max_sei1*lag_mean_sei_diff +
  hh_max_sei1*dist + hh_max_sei1*dist_sq

# create modelling function to apply across data column
dc_func <- function(data, model_spec){
  mclogit(dc_spec, data = data)
}

choice_models <- choice_data %>% 
  # create needed variables
  mutate(lag_pct_black_diff = dest_lag_pct_black - lag_pct_black1,
         lag_pct_frnbrn_diff = dest_lag_pct_frnbrn - lag_pct_frnbrn1,
         lag_mean_sei_diff = dest_lag_mean_sei - lag_mean_sei1,
         dist_sq = dist*dist) %>% 
  # keep only variables used in models
  select(serial1, year1, race_cat, ed1, dest_ed, choice, 
         hh_max_sei1, dest_lag_pct_black, lag_pct_black_diff,
         dest_lag_pct_frnbrn, lag_pct_frnbrn_diff, 
         dest_lag_mean_sei, lag_mean_sei_diff, dist, dist_sq) %>% 
  # create nested data frame
  group_by(race_cat, year1) %>% 
  nest() %>% 
  # apply dc_func using spec object as extra argument
  mutate(model = map(data, dc_func, model_spec = dc_spec))
  


########


  
full <- import(here("data", "for_models", "phl_discrete_choice_20.csv"))

test <- filter(full, race_cat == "White NB") %>% 
  # create needed variables
  mutate(
    # create squared terms
    dest_ed_pct_black_sq = dest_ed_pct_black * dest_ed_pct_black,
    dest_ed_pct_frnbrn_sq = dest_ed_pct_frnbrn * dest_ed_pct_frnbrn,
    dest_ed_mean_sei_sq = dest_ed_mean_sei * dest_ed_mean_sei,
    dist_sq = dist * dist,
    # create all interactions
    # lags in time1
    bla1_pblack_ed = ed_pct_black1 * dest_ed_pct_black,
    bla1_pblack_ed_sq = ed_pct_black1 * dest_ed_pct_black_sq,
    bla1_dist = ed_pct_black1 * dist,
    bla1_dist_sq = ed_pct_black1 * dist_sq,
    imm1_pfrnbrn_ed = ed_pct_frnbrn1 * dest_ed_pct_frnbrn,
    imm1_pfrnbrn_ed_sq = ed_pct_frnbrn1 * dest_ed_pct_frnbrn_sq,
    imm1_dist = ed_pct_frnbrn1 * dist,
    imm1_dist_sq = ed_pct_frnbrn1 * dist_sq,
    sei1_msei_ed = ed_mean_sei1 * dest_ed_mean_sei,
    sei1_msei_ed_sq = ed_mean_sei1 * dest_ed_mean_sei_sq,
    sei1_dist = ed_mean_sei1 * dist,
    sei1_dist_sq = ed_mean_sei1 * dist_sq,
    # sei
    sei_pblack_ed = hh_max_sei1 * dest_ed_pct_black,
    sei_pblack_ed_sq = hh_max_sei1 * dest_ed_pct_black_sq,
    sei_pfrnbrn_ed = hh_max_sei1 * dest_ed_pct_frnbrn,
    sei_pfrnbrn_ed_sq = hh_max_sei1 * dest_ed_pct_frnbrn_sq,
    sei_msei_ed = hh_max_sei1 * dest_ed_mean_sei,
    sei_msei_ed_sq = hh_max_sei1 * dest_ed_mean_sei_sq,
    sei_dist = hh_max_sei1 * dist,
    sei_dist_sq = hh_max_sei1 * dist_sq
  ) %>% 
  # keep only variables used in models
  select(
    # variables for identifying households
    serial1, year1, race_cat, ed1, dest_ed, 
    # original source variables for all model terms
    choice, hh_max_sei1, ed_pct_black, ed_pct_frnbrn, ed_mean_sei,
    dest_ed_pct_black, dest_ed_pct_frnbrn, dest_ed_mean_sei, dist,
    # all linear interactions
    bla1_pblack_ed, bla1_dist,
    imm1_pfrnbrn_ed, imm1_dist,
    sei1_msei_ed, sei1_dist,
    sei_pblack_ed, sei_pfrnbrn_ed, sei_msei_ed, sei_dist,
    # all quadratic interactions
    dest_ed_pct_black_sq, dest_ed_pct_frnbrn_sq, dest_ed_mean_sei_sq, dist_sq,
    bla1_pblack_ed_sq, bla1_dist_sq,
    imm1_pfrnbrn_ed_sq, imm1_dist_sq,
    sei1_msei_ed_sq, sei1_dist_sq,
    sei_pblack_ed_sq, sei_pfrnbrn_ed_sq, sei_msei_ed_sq, sei_dist_sq
  ) %>% 
  # drop cases where geometry errors
  filter(is.na(dist) == F)


dc_full <- cbind(choice,serial1) ~ 
  # destination variables
  dest_ed_pct_black + dest_ed_pct_black_sq +
  dest_ed_pct_frnbrn + dest_ed_pct_frnbrn_sq +
  dest_ed_mean_sei + dest_ed_mean_sei_sq +
  dist + dist_sq +
  # interactions with % black in time 1
  bla1_pblack_ed + bla1_pblack_ed_sq +
  bla1_dist + bla1_dist_sq +
  imm1_pfrnbrn_ed + imm1_pfrnbrn_ed_sq +
  imm1_dist + imm1_dist_sq +
  sei1_msei_ed + sei1_msei_ed_sq +
  sei1_dist + sei1_dist_sq +
  # interactions with max household SEI in time 1
  sei_pblack_ed + sei_pblack_ed_sq +
  sei_pfrnbrn_ed + sei_pfrnbrn_ed_sq +
  sei_msei_ed + sei_msei_ed_sq +
  sei_dist + sei_dist_sq


model <- mclogit(dc_black, data = test)
model2 <- mclogit(dc_imm, data = test)
model3 <- mclogit(dc_sei, data = test)
model4 <- mclogit(dc_full, data = test)
model5 <- mclogit(dc_full, data = test)


test$preds <- predict(model5, test)



out_preds <- select(test, serial1, ed1, dest_ed, preds)
out_data <- select(full, serial1, ed1, dest_ed, hh_max_sei1, 
                   ed_pct_black1, dest_ed_pct_black, 
                   ed_pct_frnbrn1, dest_ed_pct_frnbrn,
                   ed_mean_sei1, dest_ed_mean_sei,
                   dist)

out <- inner_join(out_data, out_preds)







# household level vars
# hh_max_sei1, lag_pct_black1, lag_pct_frnbrn1

# destination vars
# dist, lag_pct_black2, lag_pct_frnbrn2

# split into three files based on race / nativity
white_data <- filter(choices, black == 0 & nativity1 < 5)
wimm_data <- filter(choices, black == 0 & nativity1 == 5)
black_data <- filter(choices, black == 1)

# save files for stata testing
write_csv(white_data, here("data", "for_models", "test-stata-white_03-25.csv"))
write_csv(wimm_data, here("data", "for_models", "test-stata-wimm_03-25.csv"))
write_csv(black_data, here("data", "for_models", "test-stata-black_03-25.csv"))
print("csv files written to use with stata")

# set model specification
dc_spec <- cbind(choice, serial1) ~  
  # time 1 neighborhood variables
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  # time 2 neighborhood variables
  dest_ed_pct_black + dest_ed_pct_frnbrn + dest_ed_mean_sei + dist + dist2 +
  # all interactions from each T1 variable to each T2 variable
  ed_pct_black1*dest_ed_pct_black + ed_pct_black1*dest_ed_pct_frnbrn + ed_pct_black1*dest_ed_mean_sei + ed_pct_black1*dist + ed_pct_black1*dist2 +
  ed_pct_frnbrn1*dest_ed_pct_black + ed_pct_frnbrn1*dest_ed_pct_frnbrn + ed_pct_frnbrn1*dest_ed_mean_sei + ed_pct_frnbrn1*dist + ed_pct_frnbrn1*dist2 +
  ed_mean_sei1*dest_ed_pct_black + ed_mean_sei1*dest_ed_pct_frnbrn + ed_mean_sei1*dest_ed_mean_sei + ed_mean_sei1*dist + ed_mean_sei1*dist2
  
  

# run model for each dataset
white_model <- mclogit(dc_spec, data = white_data)
wimm_model <- mclogit(dc_spec, data = wimm_data)
black_model <- mclogit(dc_spec, data = black_data)

# save model objects
saveRDS(white_model, here("models", "discrete-choice", "white-dc-model_03-25.rds"))
saveRDS(wimm_model, here("models", "discrete-choice", "wimm-dc-model_03-25.rds"))
saveRDS(black_model, here("models", "discrete-choice", "black-dc-model_03-25.rds"))

# create tidy tables for models and save
white_model %>% tidy() %>% write_csv(here("models", "discrete-choice", "white-dc-coefs_03-25.csv"))
white_model %>% glance() %>% write_csv(here("models", "discrete-choice", "white-dc-fit_03-25.csv"))

wimm_model %>% tidy() %>% write_csv(here("models", "discrete-choice", "wimm-dc-coefs_03-25.csv"))
wimm_model %>% glance() %>% write_csv(here("models", "discrete-choice", "wimm-dc-fit_03-25.csv"))

black_model %>% tidy() %>% write_csv(here("models", "discrete-choice", "black-dc-coefs_03-25.csv"))
black_model %>% glance() %>% write_csv(here("models", "discrete-choice", "black-dc-fit_03-25.csv"))


