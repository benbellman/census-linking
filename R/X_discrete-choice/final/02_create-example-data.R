library(here)
library(purrr)
library(rio)
library(dplyr)
library(ggplot2)

source(here("R", "functions", "all_row_combos.R"))

# get actual ED data to use as a guide
all_eds <- list.files(here("data", "ed_data"), full.names = T) %>% 
  map(import) %>% 
  map(mutate, ed = as.character(ed)) %>% 
  bind_rows()

# plot variables against each other
ggplot(all_eds) +
  geom_point(aes(x = ed_pct_black, y = ed_pct_frnbrn), alpha = 0.05) +
  facet_wrap(. ~ year)

ggplot(all_eds) +
  geom_point(aes(x = ed_pct_black, y = ed_mean_sei)) +
  geom_smooth(aes(x = ed_pct_black, y = ed_mean_sei), method = "lm")

ggplot(all_eds) +
  geom_point(aes(x = ed_pct_frnbrn, y = ed_mean_sei)) +
  geom_smooth(aes(x = ed_pct_frnbrn, y = ed_mean_sei), method = "lm")


# frequency plots
ggplot(all_eds) +
  geom_histogram(aes(x = ed_pct_black))

ggplot(all_eds) +
  geom_histogram(aes(x = ed_pct_frnbrn))

ggplot(all_eds) +
  geom_density(aes(x = ed_mean_sei, col = factor(year)))


# load file created in R

# example hh SEI values
#hh_low <- tibble(hh_max_sei1 = 10,
#                 sei_cat = "SEI = 10")
#hh_high <- tibble(hh_max_sei1 = 40,
#                  sei_cat = "SEI = 40")

# example ED vars in time 1
#white_ed <- tibble(ed_pct_frnbrn1 = 10,
#                   ed_pct_black1 = 1,
#                   ed_mean_sei1 = 40,
#                   ed1_cat = "White ED")
#black_ed <- tibble(ed_pct_frnbrn1 = 5,
#                   ed_pct_black1 = 50,
#                   ed_mean_sei1 = 20,
#                   ed1_cat = "Black ED")
#imm_ed <- tibble(ed_pct_frnbrn1 = 50,
#                 ed_pct_black1 = 5,
#                 ed_mean_sei1 = 20,
#                 ed1_cat = "Immigrant ED")

# create a single set of starting neighborhoods

origin <- tibble(
  #ed_pct_frnbrn1 = rep(15, 11),
  #ed_mean_sei1 = rep(13, 11),
  ed_pct_black1 = seq(0, 50, 5)
)

# example ED vars in time 2
dest_black <- tibble(dest_ed_pct_black = 1:100,
                     #dest_ed_pct_frnbrn = 15,
                     dest_ed_mean_sei = 13, 
                     dist = 1000,
                     dest_cat = "% black",
                     serial1 = 1)

dest_sei <- tibble(dest_ed_pct_black = 10,
                   #dest_ed_pct_frnbrn = 15,
                   dest_ed_mean_sei = (1:60)/2, 
                   dist = 1000, 
                   dest_cat = "Mean SEI",
                   serial1 = 2)

dest_dist <- tibble(dest_ed_pct_black = 10,
                   #dest_ed_pct_frnbrn = 15,
                   dest_ed_mean_sei = 13, 
                   dist = seq(25, 2500, 25), 
                   dest_cat = "Distance",
                   serial1 = 3)

# combine into a single data set of prediction examples

all_ex <- all_row_combos(
  origin,
  bind_rows(dest_black, dest_sei, dest_dist)
)


# add extra columns needed for model

all_ex <- all_ex %>% 
  mutate(
    # create squared terms
    dest_ed_pct_black_sq = dest_ed_pct_black * dest_ed_pct_black,
    #dest_ed_pct_frnbrn_sq = dest_ed_pct_frnbrn * dest_ed_pct_frnbrn,
    dest_ed_mean_sei_sq = dest_ed_mean_sei * dest_ed_mean_sei,
    dist_sq = dist * dist,
    # create all ED1 interactions
    bla1_pblack_ed = ed_pct_black1 * dest_ed_pct_black,
    bla1_pblack_ed_sq = ed_pct_black1 * dest_ed_pct_black_sq,
    #bla1_pfrnbrn_ed = ed_pct_black1 * dest_ed_pct_frnbrn,
    #bla1_pfrnbrn_ed_sq = ed_pct_black1 * dest_ed_pct_frnbrn_sq,
    bla1_msei_ed = ed_pct_black1 * dest_ed_mean_sei,
    bla1_msei_ed_sq = ed_pct_black1 * dest_ed_mean_sei_sq,
    bla1_dist = ed_pct_black1 * dist,
    bla1_dist_sq = ed_pct_black1 * dist_sq
    # ED % immigrant
    #imm1_pblack_ed = ed_pct_frnbrn1 * dest_ed_pct_black,
    #imm1_pblack_ed_sq = ed_pct_frnbrn1 * dest_ed_pct_black_sq,
    #imm1_pfrnbrn_ed = ed_pct_frnbrn1 * dest_ed_pct_frnbrn,
    #imm1_pfrnbrn_ed_sq = ed_pct_frnbrn1 * dest_ed_pct_frnbrn_sq,
    #imm1_msei_ed = ed_pct_frnbrn1 * dest_ed_mean_sei,
    #imm1_msei_ed_sq = ed_pct_frnbrn1 * dest_ed_mean_sei_sq,
    #imm1_dist = ed_pct_frnbrn1 * dist,
    #imm1_dist_sq = ed_pct_frnbrn1 * dist_sq,
    # ED mean SEI
    #sei1_pblack_ed = ed_mean_sei1 * dest_ed_pct_black,
    #sei1_pblack_ed_sq = ed_mean_sei1 * dest_ed_pct_black_sq,
    #sei1_pfrnbrn_ed = ed_mean_sei1 * dest_ed_pct_frnbrn,
    #sei1_pfrnbrn_ed_sq = ed_mean_sei1 * dest_ed_pct_frnbrn_sq,
    #sei1_msei_ed = ed_mean_sei1 * dest_ed_mean_sei,
    #sei1_msei_ed_sq = ed_mean_sei1 * dest_ed_mean_sei_sq,
    #sei1_dist = ed_mean_sei1 * dist,
    #sei1_dist_sq = ed_mean_sei1 * dist_sq
  )

# save example data
write_csv(all_ex, here("data", "for_models", "dc-pred-examples-final2.csv"))

# estimate model
# add conditional and unconditional predicted probabilities
# save observed data with predictions
# load standard file of example data 
    # (3 SEI values, 3 original EDs, 1 mean destination ED with ranges for each of 4 vars)
# predict values with saved clogit model
# export predicted file to be combined later for ggplot