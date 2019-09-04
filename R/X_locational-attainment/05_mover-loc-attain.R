library(here)
library(rio)
library(tibble)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(stringr)
library(sf)

source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "aggregate_microdata.R"))




#### Load Data ####

# load linked sample, restrict to those who hosuehold heads changed addresses
linked <- import(here("data", "for_models", "phl_loc_attain.csv")) %>% 
  as_tibble() %>% 
  # NEED TO DISCUSS THESE EXCLUSIONS
  filter(sei1 > 0 & not_hhh1 == 0) %>% 
  # if the address is the same in both years, the family did not move
  mutate(moved = if_else(both_same == 1, 0, 1),
         woman = sex1 - 1,
         married = if_else(marst1 < 3, 1, 0),
         #age_sq1 = age1 * age1,
         #hh_n_adults1 = hh_size1 - hh_n_kids1,
         kids_1_to_2 = if_else(hh_n_kids1 > 0 & hh_n_kids1 < 3, 1, 0),
         kids_3_up = if_else(hh_n_kids1 >= 3, 1, 0),
         # create squared term for ed % black and ed % foreign born
         ed_pct_black_sq1 = ed_pct_black1 * ed_pct_black1,
         ed_pct_frnbrn_sq1 = ed_pct_frnbrn1 * ed_pct_frnbrn1,
         lag_pct_black_sq1 = lag_pct_black1 * lag_pct_black1,
         lag_pct_frnbrn_sq1 = lag_pct_frnbrn1 * lag_pct_frnbrn1) %>% 
  filter(moved == 1)

data_list <- list(
  filter(linked, year1 == 1910),
  filter(linked, year1 == 1920),
  filter(linked, year1 == 1930)
)

# load pre-calculated distances between EDs and merge with data
# Currently only have these for 1920 to 1930
list(
  here("data", "ed_distances_10_20.csv"),
  here("data", "ed_distances_20_30.csv"),
  here("data", "ed_distances_30_40.csv")
) %>% 
  map(import) %>% 
  map(rename, ed2 = dest_ed) %>% 
  map(mutate, ed2 = as.character(ed2)) %>% 
  map2(data_list, inner_join) %>% 
  map(as_tibble) %>% 
  bind_rows() -> linked


# stratify the data by year and racial group
nested <- linked %>%
  group_by(race_cat, year1) %>% 
  nest() %>% 
  filter(race_cat != "Other") %>% 
  as_tibble()


#### Model specifications ####

# I'm using my preferred specification using only ED variables

# distance moved (measured by centroids of origin and destination EDs)
fm_dist <- dist ~
  woman + married + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1

# mean SEI in destination ED
fm_sei <- ed_mean_sei2 ~
  woman + married + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1

# % black in destination ED
fm_black <- ed_pct_black2 ~
  woman + married + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1

# % foreign-born in destination ED
fm_frnbrn <- ed_pct_frnbrn2 ~
  woman + married + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1



# Difference in mean SEI in destination ED
fm_sei_diff <- ed_mean_sei_diff ~
  woman + married + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1

# Difference in % black in destination ED
fm_black_diff <- ed_pct_black_diff ~
  woman + married + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1

# Difference in % foreign-born in destination ED
fm_frnbrn_diff <- ed_pct_frnbrn_diff ~
  woman + married + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1

# combine all data with all model specifications

all_data <- bind_cols(
  bind_rows(rep(list(nested), 7)),
  tibble(spec = combine(rep(list(fm_dist), 9), 
                        rep(list(fm_black), 9), 
                        rep(list(fm_frnbrn), 9),
                        rep(list(fm_sei), 9),
                        rep(list(fm_black_diff), 9), 
                        rep(list(fm_frnbrn_diff), 9),
                        rep(list(fm_sei_diff), 9)),
         spec_type = c(rep("Distance moved", 9), 
                       rep("Percent black", 9), 
                       rep("Percent immigrant", 9),
                       rep("Mean SEI", 9),
                       rep("Change in Percent black", 9), 
                       rep("Change in Percent immigrant", 9),
                       rep("Change in Mean SEI", 9)))
)


#### Run models ####

# define wrapper function to run a certain specification on a given dataset
run_ols <- function(data, spec){
  lm(spec, data = data)
}

# map model function to all data and specifications, save in data frame
all_data_models <- all_data %>% 
  mutate(model = map2(data, spec, run_ols),
         glance = map(model, glance),
         coefs = map(model, tidy),
         augmented = map(model, augment))

#### Examine models ####

# get a table of coefficients from all models
coefs <- select(all_data_models, race_cat, year1, spec_type, coefs) %>% 
  unnest(cols = c(coefs)) %>% 
  mutate(min = estimate - std.error,
         max = estimate + std.error)

# export coefficients into single tables according to dependent var
coef_table <- function(d){
  select(d, race_cat, year1, term, estimate) %>% 
    dcast(term ~ race_cat + year1) -> est
  
  select(d, race_cat, year1, term, p.value) %>% 
    dcast(term ~ race_cat + year1) -> pval
  
  names(est)[-1] <- paste0(names(est)[-1], "_est")
  names(pval)[-1] <- paste0(names(pval)[-1], "_pval")
  
  out <- right_join(est, pval)
  out <- cbind(out[,1], out[,names(out[,-1])[order(names(out[,-1]))]])
  out[c(1, 13, 12, 2, 10, 11, 6, 4, 5, 3, 8, 9, 7),]
}


coefs %>% 
  filter(spec_type == "Distance moved") %>% 
  coef_table() %>% 
  write_csv(here("tables", "chp2", "loc_attain", "distance_coefs.csv"))

coefs %>% 
  filter(spec_type == "Percent black") %>% 
  coef_table() %>% 
  write_csv(here("tables", "chp2", "loc_attain", "pblack_coefs.csv"))

coefs %>% 
  filter(spec_type == "Percent immigrant") %>% 
  coef_table() %>% 
  write_csv(here("tables", "chp2", "loc_attain", "pimm_coefs.csv"))

coefs %>% 
  filter(spec_type == "Mean SEI") %>% 
  coef_table() %>% 
  write_csv(here("tables", "chp2", "loc_attain", "msei_coefs.csv"))

coefs %>% 
  filter(spec_type == "Change in Percent black") %>% 
  coef_table() %>% 
  write_csv(here("tables", "chp2", "loc_attain", "chg_pblack_coefs.csv"))

coefs %>% 
  filter(spec_type == "Change in Percent immigrant") %>% 
  coef_table() %>% 
  write_csv(here("tables", "chp2", "loc_attain", "chg_pimm_coefs.csv"))

coefs %>% 
  filter(spec_type == "Change in Mean SEI") %>% 
  coef_table() %>% 
  write_csv(here("tables", "chp2", "loc_attain", "chg_msei_coefs.csv"))

# line plots plots of ED coeficients over time for each model
coefs %>% 
  filter(spec_type == "Distance moved") %>% 
  filter(str_detect(term, "^ed_")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing distance moved in meters")


coefs %>% 
  filter(spec_type == "Percent black") %>% 
  filter(str_detect(term, "^ed_")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing destination ED % black")

coefs %>% 
  filter(spec_type == "Change in Percent black") %>% 
  filter(str_detect(term, "^ed_")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing change in ED % black")


coefs %>% 
  filter(spec_type == "Percent immigrant") %>% 
  filter(str_detect(term, "^ed_")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing % immigrant of destination neighborhood")

coefs %>% 
  filter(spec_type == "Change in Percent immigrant") %>% 
  filter(str_detect(term, "^ed_")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing change in % immigrant of neighborhood")

coefs %>% 
  filter(spec_type == "Mean SEI") %>% 
  filter(str_detect(term, "^ed_")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing Mean SEI of destination neighborhood")

coefs %>% 
  filter(spec_type == "Change in Mean SEI") %>% 
  filter(str_detect(term, "^ed_")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing change in Mean SEI of neighborhood")




# these are the same coefficient plots as above, but for ED interactions with HH SEI
coefs %>% 
  filter(spec_type == "Distance moved") %>% 
  filter(str_detect(term, "\\:")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing distance moved in meters")


coefs %>% 
  filter(spec_type == "Percent black") %>% 
  filter(str_detect(term, "\\:")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing % black of destination neighborhood")


coefs %>% 
  filter(spec_type == "Change in Percent black") %>% 
  filter(str_detect(term, "\\:")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing change in % black of neighborhood")


coefs %>%  
  filter(spec_type == "Percent immigrant") %>% 
  filter(str_detect(term, "\\:")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing % immigrant of destination neighborhood")

coefs %>%  
  filter(spec_type == "Change in Percent immigrant") %>% 
  #filter(str_detect(term, "\\:")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing change in % immigrant of neighborhood")

coefs %>%  
  filter(spec_type == "Mean SEI") %>% 
  filter(str_detect(term, "\\:")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing Mean SEI of destination neighborhood")

coefs %>%  
  filter(spec_type == "Change in Mean SEI") %>% 
  #filter(str_detect(term, "\\:")) %>% 
  filter(p.value < 0.05) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = term)) +
  geom_point(aes(x = year1, y = estimate, col = term)) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = term), width = 0.5) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_wrap(~race_cat) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Coefficients describing change in Mean SEI of neighborhood")





#### Predicted values of attainment ####

# decide on "average" characteristics of a householder and family
model_vars <- c("moved", "woman", "married", "age1",
                "kids_1_to_2", "kids_3_up", "hh_max_sei1",
                "ed_pct_black1", "ed_pct_frnbrn1", "ed_mean_sei1")
model_interacts <- c("hh_max_sei1:ed_pct_black1", "hh_max_sei1:ed_pct_frnbrn1", "hh_max_sei1:ed_mean_sei1")
summary(select(linked, model_vars))

# export a table of mean values for lag models
#select(linked, race_cat, year1, model_vars) %>% 
#  filter(race_cat != "Other") %>% 
#  mutate(year1 = as.character(year1)) %>% 
#  group_by(race_cat, year1) %>% 
#  summarise_all(list(~mean(., na.rm = T))) %>% 
#  melt() %>% 
#  dcast(variable ~ race_cat + year1) %>% 
#  write_csv(here("tables", "chp2", "logit_var_means.csv"))


# create 1-row tibble containing example householder and household
ex1 <- tibble(
  woman = 0,
  married = 1,
  age1 = 35,
  kids_1_to_2 = 1,
  kids_3_up = 0,
  hh_max_sei1 = 20,
  ex = "Age = 35, SEI = 20"
)

ex2 <- tibble(
  woman = 0,
  married = 1,
  age1 = 50,
  kids_1_to_2 = 1,
  kids_3_up = 0,
  hh_max_sei1 = 55,
  ex = "Age = 50, SEI = 55"
)

# function for making and processing predicted probabilities
my_predict <- function(model, new_data){
  # grab the ed numbers from data if present
  if ("ed1" %in% names(new_data)){
    ed_col <- new_data$ed1
  }
  
  # generate predictions
  predict(model, new_data, type = "response") %>% 
    enframe(name = NULL) -> out
  # return with or without Ed numbers depending on inputs
  if (exists("ed_col")){
    mutate(out, ED = ed_col)
  } else {
    out
  }
}


#### True ED predictions ####

ed <- bind_rows(
  import(here("data", "ed_data", "ED_data_1910.csv")),
  import(here("data", "ed_data", "ED_data_1920.csv")),
  import(here("data", "ed_data", "ED_data_1930.csv"))
)
names(ed) <- paste0(names(ed), "1")

# load polygons of 1910-1930 EDs
phl10 <- st_read(here("data", "shertzer_eds", "Philadelphia_1910.shp"))
phl20 <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp"))
phl30 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp"))

# create a named list of polygon files to reference with funciton
#polygons <- list("1910" = phl10, "1920" = phl20, "1930" = phl30)

# generate lagged values for ED-level vars using queen contiguity (simple intersect)
#lag_var <- function(ed_num, year){
  # pull polygons for given year
#  poly <- polygons[[as.character(year)]]
  # get ED numbers of touching polygon main ED
#  neighs <- suppressWarnings(st_intersection(filter(poly, ED == ed_num), poly)$ED.1)
  # drop focal ED from the lagged calculations, want to measure distinct area diff. from ED
  #neighs <- neighs[!(neighs == ed_num)]
  # filter only these EDs from data and return lagged values (add ed1 and year1 for merging with ed data)
#  ed %>% 
#    filter(year1 == year & ed1 %in% neighs) %>% 
#    summarise(lag_pct_black1 = sum(ed_n_black1) / sum(ed_total_pop1) * 100,
#              lag_pct_frnbrn1 = sum(ed_n_frnbrn1) / sum(ed_total_pop1) * 100,
#              lag_pct_under_151 = sum(ed_n_under_151) / sum(ed_total_pop1) * 100,
#              lag_mean_age1 = weighted.mean(x = ed_mean_age1, w = ed_total_pop1),
#              lag_mean_sei1 = weighted.mean(x = ed_mean_sei1, w = ed_total_pop1)) %>% 
#    mutate(ed1 = ed_num, year1 = year)
#}

# map function to ed1 data to create all lagged values for all ED-years
#lagged <- map2_dfr(ed$ed1, ed$year1, lag_var)

# merged lagged values with ed1 data
#ed <- left_join(ed, lagged)

# add squared terms that appear in models
#ed <- mutate(ed,
#             ed_pct_black_sq1 = ed_pct_black1*ed_pct_black1,
#             lag_pct_black_sq1 = lag_pct_black1*ed_pct_black1,
#             ed_pct_frnbrn_sq1 = ed_pct_frnbrn1*ed_pct_frnbrn1,
#             lag_pct_frnbrn_sq1 = lag_pct_frnbrn1*lag_pct_frnbrn1)


# now merge all ED characteristics from each year with example data and predict moving probabilities
predictions1 <- cbind(ed, ex1) %>% 
  as_tibble() %>% 
  group_by(year1) %>% 
  nest() %>% 
  rename(ex_data = data) %>% 
  left_join(all_data_models, "year1") %>% 
  mutate(ex_pred = map2(model, ex_data, my_predict))

predictions2 <- cbind(ed, ex2) %>% 
  as_tibble() %>% 
  group_by(year1) %>% 
  nest() %>% 
  rename(ex_data = data) %>% 
  left_join(all_data_models, "year1") %>% 
  mutate(ex_pred = map2(model, ex_data, my_predict))

## select the predictions and unnest into a single data frame

all_ed_preds <- bind_rows(
  unnest(select(predictions1, year1, race_cat, spec_type, ex_pred), cols = ex_pred) %>% mutate(ex = "Age = 35, SEI = 20"),
  unnest(select(predictions2, year1, race_cat, spec_type, ex_pred), cols = ex_pred) %>% mutate(ex = "Age = 50, SEI = 55")
) 


# plot predicted distance for black households
all_ed_preds %>% 
  filter(race_cat == "Black" & spec_type == "Distance moved") %>% 
  filter(spec_type == "Distance moved") %>% 
  mutate(ED = as.factor(ED)) %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  mutate(value_cat = factor(case_when(
    value < 2000 ~ "Under 2km",
    value >= 2000 & value < 2500 ~ "2 - 2.5km",
    value >= 2500 & value < 3000 ~ "2.5 - 3km",
    value >= 3000 & value < 3500 ~ "3 - 3.5km",
    value >= 3500 ~ "Over 3.5km"
  ), levels = c("Under 2km", "2 - 2.5km", "2.5 - 3km", "3 - 3.5km", "Over 3.5km"))) %>% 
  ggplot() +
  geom_sf(aes(fill = value_cat), lwd = 0, col = "white") +
  scale_fill_brewer("Predicted distance moved", palette = "BuPu") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(ex ~ year1) +
  ggsave(here("figures", "chp2", "black_ex_predicted_mover_distance.png"), height = 4, width = 7)

# white native born
all_ed_preds %>% 
  filter(race_cat == "White NB" & spec_type == "Distance moved") %>% 
  filter(spec_type == "Distance moved") %>% 
  mutate(ED = as.factor(ED)) %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  mutate(value_cat = factor(case_when(
    value < 2000 ~ "Under 2km",
    value >= 2000 & value < 2500 ~ "2 - 2.5km",
    value >= 2500 & value < 3000 ~ "2.5 - 3km",
    value >= 3000 & value < 3500 ~ "3 - 3.5km",
    value >= 3500 ~ "Over 3.5km"
  ), levels = c("Under 2km", "2 - 2.5km", "2.5 - 3km", "3 - 3.5km", "Over 3.5km"))) %>% 
  ggplot() +
  geom_sf(aes(fill = value_cat), lwd = 0, col = "white") +
  scale_fill_brewer("Predicted distance moved", palette = "BuPu") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(ex ~ year1) +
  ggsave(here("figures", "chp2", "wnb_ex_predicted_mover_distance.png"), height = 4, width = 7)



## single-example maps
# compares across all three years and race groups

# distance
select(predictions1, year1, race_cat, spec_type, ex_pred) %>% 
  unnest() %>% 
  filter(spec_type == "Distance moved") %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  mutate(value_cat = factor(case_when(
    value < 2000 ~ "Under 2km",
    value >= 2000 & value < 2500 ~ "2 - 2.5km",
    value >= 2500 & value < 3000 ~ "2.5 - 3km",
    value >= 3000 & value < 3500 ~ "3 - 3.5km",
    value >= 3500 ~ "Over 3.5km"
  ), levels = c("Under 2km", "2 - 2.5km", "2.5 - 3km", "3 - 3.5km", "Over 3.5km"))) -> ed_dist_preds1

select(predictions1, year1, race_cat, spec_type, ex_pred) %>% 
  unnest() %>% 
  filter(spec_type == "Percent black") %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  mutate(value_cat = factor(case_when(
    value < 2.5 ~ "Under 2.5% black",
    value >= 2.5 & value < 10 ~ "2.5 - 10% black",
    value >= 10 & value < 30 ~ "10 - 30% black",
    value >= 30 ~ "Over 30% black"
  ), levels = c("Under 2.5% black", "2.5 - 10% black", "10 - 30% black", "Over 30% black"))) -> ed_pblack_preds1

select(predictions1, year1, race_cat, spec_type, ex_pred) %>% 
  unnest() %>% 
  filter(spec_type == "Percent immigrant") %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20)) %>% 
  mutate(value_cat = factor(case_when(
    value < 10 ~ "Under 10% immigrant",
    value >= 10 & value < 20 ~ "10 - 20% immigrant",
    value >= 20 & value < 30 ~ "20 - 30% immigrant",
    value >= 30 ~ "Over 30% immigrant"
  ), levels = c("Under 10% immigrant", "10 - 20% immigrant", "20 - 30% immigrant", "Over 30% immigrant"))) -> ed_pfrnbrn_preds1

## Example 2

# distance
select(predictions2, year1, race_cat, spec_type, ex_pred) %>% 
  unnest() %>% 
  filter(spec_type == "Distance moved") %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  mutate(value_cat = factor(case_when(
    value < 2000 ~ "Under 2km",
    value >= 2000 & value < 2500 ~ "2 - 2.5km",
    value >= 2500 & value < 3000 ~ "2.5 - 3km",
    value >= 3000 & value < 3500 ~ "3 - 3.5km",
    value >= 3500 ~ "Over 3.5km"
  ), levels = c("Under 2km", "2 - 2.5km", "2.5 - 3km", "3 - 3.5km", "Over 3.5km"))) -> ed_dist_preds2

select(predictions2, year1, race_cat, spec_type, ex_pred) %>% 
  unnest() %>% 
  filter(spec_type == "Percent black") %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  mutate(value_cat = factor(case_when(
    value < 2.5 ~ "Under 2.5% black",
    value >= 2.5 & value < 10 ~ "2.5 - 10% black",
    value >= 10 & value < 30 ~ "10 - 30% black",
    value >= 30 ~ "Over 30% black"
  ), levels = c("Under 2.5% black", "2.5 - 10% black", "10 - 30% black", "Over 30% black"))) -> ed_pblack_preds2


select(predictions2, year1, race_cat, spec_type, ex_pred) %>% 
  unnest() %>% 
  filter(spec_type == "Percent immigrant") %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20)) %>% 
  mutate(value_cat = factor(case_when(
    value < 10 ~ "Under 10% immigrant",
    value >= 10 & value < 20 ~ "10 - 20% immigrant",
    value >= 20 & value < 30 ~ "20 - 30% immigrant",
    value >= 30 ~ "Over 30% immigrant"
  ), levels = c("Under 10% immigrant", "10 - 20% immigrant", "20 - 30% immigrant", "Over 30% immigrant"))) -> ed_pfrnbrn_preds2



# Example 1 distance
ggplot(ed_dist_preds1) +
  geom_sf(aes(fill = value_cat), lwd = 0, col = "white") +
  scale_fill_brewer("Predicted distance moved", palette = "BuPu") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(race_cat ~ year1) +
  ggsave(here("figures", "chp2", "predicted_mover_distance_ex1.png"), height = 7, width = 10)

# Example 1 black
ggplot(ed_pblack_preds1) +
  geom_sf(aes(fill = value_cat), lwd = 0, col = "white") +
  scale_fill_brewer("", palette = "BuPu") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(race_cat ~ year1) +
  ggsave(here("figures", "chp2", "predicted_mover_pblack_ex1.png"), height = 7, width = 10)

# Example 1 immigrant
ggplot(ed_pfrnbrn_preds1) +
  geom_sf(aes(fill = value_cat), lwd = 0, col = "white") +
  scale_fill_brewer("", palette = "BuPu") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(race_cat ~ year1) +
  ggsave(here("figures", "chp2", "predicted_mover_pfrnbrn_ex1.png"), height = 7, width = 10)

# Example 2 distance moved
ggplot(ed_dist_preds2) +
  geom_sf(aes(fill = value_cat), lwd = 0, col = "white") +
  scale_fill_brewer("Predicted distance moved", palette = "BuPu") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(race_cat ~ year1) +
  ggsave(here("figures", "chp2", "predicted_mover_distance_ex1.png"), height = 7, width = 10)

# Example 2 black
ggplot(ed_pblack_preds2) +
  geom_sf(aes(fill = value_cat), lwd = 0, col = "white") +
  scale_fill_brewer("", palette = "BuPu") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(race_cat ~ year1) +
  ggsave(here("figures", "chp2", "predicted_mover_pblack_ex2.png"), height = 7, width = 10)

# Example 2 immigrant
ggplot(ed_pfrnbrn_preds2) +
  geom_sf(aes(fill = value_cat), lwd = 0, col = "white") +
  scale_fill_brewer("", palette = "BuPu") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(race_cat ~ year1) +
  ggsave(here("figures", "chp2", "predicted_mover_pfrnbrn_ex2.png"), height = 7, width = 10)



#### Predictions along single variables ####

# Ok, now lets examine the overall distributions of ED variables and their neighboring areas
summary(select(ed, ed_pct_black1, ed_pct_frnbrn1, ed_mean_sei1))

group_by(linked, year2) %>% summarise(pblack = mean(ed_pct_black2, na.rm = T),
                                  pimm = mean(ed_pct_frnbrn2, na.rm = T),
                                  msei = mean(ed_mean_sei2, na.rm = T))

# create an example "average" ED that varies % black fomr 0% to 100%
ex_ed_pblack <- tibble(
  ed_pct_black1 = seq(0, 100, 2),
  ed_pct_frnbrn1 = rep(15, 51),
  ed_mean_sei1 = rep(12, 51)
)

ex_ed_pfrnbrn <- tibble(
  ed_pct_black1 = rep(2, 51),
  ed_pct_frnbrn1 = seq(0, 100, 2),
  ed_mean_sei1 = rep(12, 51)
)

ex_ed_msei <- tibble(
  ed_pct_black1 = rep(2, 51),
  ed_pct_frnbrn1 = rep(15, 51),
  ed_mean_sei1 = seq(0, 50, 1)
)

# combine hypothetical EDs and households
ex_ed_pblack <- all_row_combos(rbind(ex1, ex2), ex_ed_pblack)
ex_ed_pfrnbrn <- all_row_combos(rbind(ex1, ex2), ex_ed_pfrnbrn)
ex_ed_msei <- all_row_combos(rbind(ex1, ex2), ex_ed_msei)

# combine with all models for groups, dep vars, and years
all_data_models %>% 
  #filter(spec_type == "Distance moved") %>% 
  select(race_cat, spec_type, year1, model) %>% 
  mutate(ex_data = rep(list(ex_ed_pblack), n()),
         pred = map2(model, ex_data, my_predict)) %>% 
  select(race_cat, spec_type, year1, ex_data, pred) %>% 
  unnest(cols = c(ex_data, pred)) -> ex_all_pblack_preds

all_data_models %>% 
  #filter(spec_type == "Lagged") %>% 
  select(race_cat, spec_type, year1, model) %>% 
  mutate(ex_data = rep(list(ex_ed_pfrnbrn), n()),
         pred = map2(model, ex_data, my_predict)) %>% 
  select(race_cat, spec_type, year1, ex_data, pred) %>% 
  unnest(cols = c(ex_data, pred)) -> ex_all_pfrnbrn_preds

all_data_models %>% 
  #filter(spec_type == "Lagged") %>% 
  select(race_cat, spec_type, year1, model) %>% 
  mutate(ex_data = rep(list(ex_ed_msei), n()),
         pred = map2(model, ex_data, my_predict)) %>% 
  select(race_cat, spec_type, year1, ex_data, pred) %>% 
  unnest(cols = c(ex_data, pred)) -> ex_all_msei_preds





# create line plots of predictions, facet by year
# % black predicting distance
filter(ex_all_pblack_preds, spec_type == "Distance moved") %>% 
ggplot() +
  geom_line(aes(x = ed_pct_black1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED % black",
       y = "Distance moved",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "dist_pblack_ex_preds.pdf"), height = 3, width = 6)

# % immigrant predicting distance
filter(ex_all_pfrnbrn_preds, spec_type == "Distance moved") %>% 
  ggplot() +
  geom_line(aes(x = ed_pct_frnbrn1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED % immigrant",
       y = "Distance moved",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "dist_pfrnbrn_ex_preds.pdf"), height = 3, width = 6)

# mean sei predicting distance
filter(ex_all_msei_preds, spec_type == "Distance moved") %>% 
  ggplot() +
  geom_line(aes(x = ed_mean_sei1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED Mean SEI",
       y = "Distance moved",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "dist_msei_ex_preds.pdf"), height = 3, width = 6)


# % black predicting % black
#filter(ex_all_pblack_preds, spec_type == "Percent black") %>% 
#  ggplot() +
#  geom_line(aes(x = ed_pct_black1, y = value, col = race_cat, lty = ex)) +
#  labs(x = "ED % black",
#       y = "Destination ED % black",
#       col = "",
#       lty = "") +
#  facet_wrap(~ year1) +
#  ggsave(here("figures", "chp2", "pblack_pblack_ex_preds.pdf"), height = 3, width = 6)

# % immigrant predicting % immigrant
#filter(ex_all_pfrnbrn_preds, spec_type == "Percent immigrant") %>% 
#  ggplot() +
#  geom_line(aes(x = ed_pct_frnbrn1, y = value, col = race_cat, lty = ex)) +
#  labs(x = "ED % immigrant",
#       y = "Destination ED % immigrant",
#       col = "",
#       lty = "") +
#  facet_wrap(~ year1) +
#  ggsave(here("figures", "chp2", "pimm_pimm_ex_preds.pdf"), height = 3, width = 6)

# mean SEI predicting mean SEI
#filter(ex_all_msei_preds, spec_type == "Mean SEI") %>% 
#  ggplot() +
#  geom_line(aes(x = ed_mean_sei1, y = value, col = race_cat, lty = ex)) +
#  labs(x = "ED mean SEI",
#       y = "Destination ED mean SEI",
#       col = "",
#       lty = "") +
#  facet_wrap(~ year1) +
#  ggsave(here("figures", "chp2", "msei_msei_ex_preds.pdf"), height = 3, width = 6)



# % black predicting change in % black
filter(ex_all_pblack_preds, spec_type == "Percent black") %>% 
  mutate(value = value - ed_pct_black1) %>% 
  ggplot() +
  geom_line(aes(x = ed_pct_black1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED % black",
       y = "Change in ED % black",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "chg-pblack_pblack_ex_preds.pdf"), height = 3, width = 6)

# % imm predicting % black
filter(ex_all_pfrnbrn_preds, spec_type == "Percent black") %>% 
  ggplot() +
  geom_line(aes(x = ed_pct_frnbrn1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED % immigrant",
       y = "Destination ED % black",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "pblack_pimm_ex_preds.pdf"), height = 3, width = 6)


# % immigrant predicting change in % immigrant
filter(ex_all_pfrnbrn_preds, spec_type == "Percent immigrant") %>% 
  mutate(value = value - ed_pct_frnbrn1) %>% 
  ggplot() +
  geom_line(aes(x = ed_pct_frnbrn1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED % immigrant",
       y = "Change in ED % immigrant",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "chg-pimm_pimm_ex_preds.pdf"), height = 3, width = 6)

# % black predicting % imm
filter(ex_all_pblack_preds, spec_type == "Percent immigrant") %>% 
  ggplot() +
  geom_line(aes(x = ed_pct_black1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED % black",
       y = "Destination ED % immigrant",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "pimm_pblack_ex_preds.pdf"), height = 3, width = 6)

# mean SEI predicting % black
filter(ex_all_msei_preds, spec_type == "Percent black") %>% 
  ggplot() +
  geom_line(aes(x = ed_mean_sei1, y = value, col = race_cat, lty = ex)) +
  labs(x = "Mean ED SEI",
       y = "Destination ED % black",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "pblack_msei_ex_preds.pdf"), height = 3, width = 6)

# mean SEI predicting % immigrant
filter(ex_all_msei_preds, spec_type == "Percent immigrant") %>% 
  ggplot() +
  geom_line(aes(x = ed_mean_sei1, y = value, col = race_cat, lty = ex)) +
  labs(x = "Mean ED SEI",
       y = "Destination ED % immigrant",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "pimm_msei_ex_preds.pdf"), height = 3, width = 6)

#  % black  predicting mean SEI
filter(ex_all_pblack_preds, spec_type == "Mean SEI") %>% 
  ggplot() +
  geom_line(aes(x = ed_pct_black1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED % black",
       y = "Destination Mean ED SEI",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "msei_pblack_ex_preds.pdf"), height = 3, width = 6)

# % immigrant predicting mean SEI 
filter(ex_all_pfrnbrn_preds, spec_type == "Percent immigrant") %>% 
  ggplot() +
  geom_line(aes(x = ed_pct_frnbrn1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED % immigrant",
       y = "Destination Mean ED SEI",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "final_loc_attain", "msei_pimm_ex_preds.pdf"), height = 3, width = 6)

################### STOP HERE



# % immigrant predicting change in % black
filter(ex_all_pfrnbrn_preds, spec_type == "Change in Percent black") %>% 
  ggplot() +
  geom_line(aes(x = ed_pct_frnbrn1, y = value, col = race_cat, lty = ex)) +
  labs(x = "ED % immigrant",
       y = "Change in ED % black",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "chg-pblack_pfrnbrn_ex_preds.pdf"), height = 4, width = 6)


###
# % black predicting % immigrant
filter(ex_all_pblack_preds, spec_type == "Percent immigrant") %>% 
  ggplot() +
  geom_line(aes(x = lag_pct_black1, y = value, col = race_cat, lty = ex)) +
  labs(x = "Lagged ED % black",
       y = "Predicted destination percent immigrant",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "pfrnbrn_pblack_ex_preds.pdf"), height = 4, width = 6)

# % immigrant predicting % immigrant
filter(ex_all_pfrnbrn_preds, spec_type == "Percent immigrant") %>% 
  ggplot() +
  geom_line(aes(x = lag_pct_frnbrn1, y = value, col = race_cat, lty = ex)) +
  labs(x = "Lagged ED % black",
       y = "Predicted destination percent immigrant",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "pfrnbrn_pfrnbrn_ex_preds.pdf"), height = 4, width = 6)



# % black predicting change in % immigrant
filter(ex_all_pblack_preds, spec_type == "Change in Percent immigrant") %>% 
  ggplot() +
  geom_line(aes(x = lag_pct_black1, y = value, col = race_cat, lty = ex)) +
  labs(x = "Lagged ED % black",
       y = "Predicted change in percent immigrant",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "chg-pfrnbrn_pblack_ex_preds.pdf"), height = 4, width = 6)

# % immigrant predicting change in % immigrant
filter(ex_all_pfrnbrn_preds, spec_type == "Change in Percent immigrant") %>% 
  ggplot() +
  geom_line(aes(x = lag_pct_frnbrn1, y = value, col = race_cat, lty = ex)) +
  labs(x = "Lagged ED % immigrant",
       y = "Predicted change in percent immigrant",
       col = "",
       lty = "") +
  facet_wrap(~ year1) +
  ggsave(here("figures", "chp2", "chg-pfrnbrn_pfrnbrn_ex_preds.pdf"), height = 4, width = 6)










