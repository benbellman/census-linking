library(here)
library(rio)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(broom)
library(data.table)
library(ggplot2)
library(sf)
library(car)
library(readr)
library(tidyr)
library(ghibli)
library(RColorBrewer)

#source(here("R", "functions", "all_row_combos.R"))
#source(here("R", "functions", "aggregate_microdata.R"))


# import linked data for analysis
linked <- import(here("data", "analysis", "links-full-info.csv")) %>% 
  as_tibble() %>% 
  # drop households with no occupational response or with heads that were not heads in time 1
  filter(hh_max_sei1 > 0 & not_hhh1 == 0) %>% 
  # constuct specific variables for models
  mutate(
    moved = if_else(move_type == "Left Neighborhood", 1, 0),
    woman = sex1 - 1,
    married = if_else(marst1 < 3, 1, 0),
    age_sq1 = age1 * age1,
    homeowner = if_else(home_type1 == "Owner", 1, 0),
    kids_1_to_2 = if_else(hh_n_kids1 > 0 & hh_n_kids1 < 3, 1, 0),
    kids_3_up = if_else(hh_n_kids1 >= 3, 1, 0),
    ed_pct_black_sq1 = ed_pct_black1 * ed_pct_black1,
    ed_pct_frnbrn_sq1 = ed_pct_frnbrn1 * ed_pct_frnbrn1,
    ed_mean_sei_sq1 = ed_mean_sei1 * ed_mean_sei1
  )

# nest data into groups for separate model estimation
nested <- linked %>%
  filter(race_cat != "Other") %>% 
  group_by(race_cat, year1) %>% 
  nest()


#### Model specifications ####

# linear ED terms, no interactions
fm_lin_edXno <- moved ~
  woman + married + homeowner + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1

# linear ED terms, ed attributes interact with hh SEI
fm_lin_edXsei <- moved ~
  woman + married + homeowner + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1

# linear ED terms, ed attributes interact with homeownership
fm_lin_edXown <- moved ~
  woman + married + homeowner + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  homeowner*ed_pct_black1 + homeowner*ed_pct_frnbrn1 + homeowner*ed_mean_sei1

# linear ED terms, ed attributes interact with both SEI and homeownership
fm_lin_edXboth <- moved ~
  woman + married + homeowner + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1 +
  homeowner*ed_pct_black1 + homeowner*ed_pct_frnbrn1 + homeowner*ed_mean_sei1

# quadratic ED terms, no interactions
fm_quad_edXno <- moved ~
  woman + married + homeowner + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_black_sq1 +
  ed_pct_frnbrn1 + ed_pct_frnbrn_sq1 +
  ed_mean_sei1 + ed_mean_sei_sq1

# quadratic ED terms, ed attributes interact with hh SEI
fm_quad_edXsei <- moved ~
  woman + married + homeowner + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_black_sq1 +
  ed_pct_frnbrn1 + ed_pct_frnbrn_sq1 +
  ed_mean_sei1 + ed_mean_sei_sq1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_black_sq1 + 
  hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_pct_frnbrn_sq1 + 
  hh_max_sei1*ed_mean_sei1 + hh_max_sei1*ed_mean_sei_sq1

# quadratic ED terms, ed attributes interact with homeownership
fm_quad_edXown <- moved ~
  woman + married + homeowner + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_black_sq1 +
  ed_pct_frnbrn1 + ed_pct_frnbrn_sq1 +
  ed_mean_sei1 + ed_mean_sei_sq1 +
  homeowner*ed_pct_black1 + homeowner*ed_pct_black_sq1 + 
  homeowner*ed_pct_frnbrn1 + homeowner*ed_pct_frnbrn_sq1 + 
  homeowner*ed_mean_sei1 + homeowner*ed_mean_sei_sq1

# quadratic ED terms, ed attributes interact with both SEI homeownership
fm_quad_edXboth <- moved ~
  woman + married + homeowner + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_black_sq1 +
  ed_pct_frnbrn1 + ed_pct_frnbrn_sq1 +
  ed_mean_sei1 + ed_mean_sei_sq1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_black_sq1 + 
  hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_pct_frnbrn_sq1 + 
  hh_max_sei1*ed_mean_sei1 + hh_max_sei1*ed_mean_sei_sq1 +
  homeowner*ed_pct_black1 + homeowner*ed_pct_black_sq1 + 
  homeowner*ed_pct_frnbrn1 + homeowner*ed_pct_frnbrn_sq1 + 
  homeowner*ed_mean_sei1 + homeowner*ed_mean_sei_sq1

# combine all data with all model specifications
all_data <- bind_cols(
  bind_rows(rep(list(nested), 8)),
  tibble(spec = c(rep(list(fm_lin_edXno), 9),
                  rep(list(fm_lin_edXsei), 9), 
                  rep(list(fm_lin_edXown), 9),
                  rep(list(fm_lin_edXboth), 9),
                  rep(list(fm_quad_edXno), 9),
                  rep(list(fm_quad_edXsei), 9),
                  rep(list(fm_quad_edXown), 9),
                  rep(list(fm_quad_edXboth), 9)),
         spec_type = factor(c(rep("Linear, none", 9),
                              rep("Linear, SEI", 9), 
                              rep("Linear, ownership", 9),
                              rep("Linear, both", 9),
                              rep("Quadratic, none", 9),
                              rep("Quadratic, SEI", 9),
                              rep("Quadratic, ownership", 9),
                              rep("Quadratic, both", 9)), 
                            levels = c("Linear, none", "Linear, SEI", "Linear, ownership", "Linear, both",
                                       "Quadratic, none", "Quadratic, SEI", "Quadratic, ownership", "Quadratic, both")))
)


#### Run models ####

# define wrapper function to run a certain specification on a given dataset
run_logit <- function(data, spec){
  glm(spec, data = data, family = binomial(link = "logit"))
}

# define function to get each column VIF as a single row tibble
get_vif <- function(m){
  vifm <- vif(m)
  mat <- matrix(vifm, nrow = 1)
  viftbl <- as_tibble(mat)
  names(viftbl) <- names(vifm)
  viftbl
}

# map model function to all data and specifications, save in data frame
all_data_models <- all_data %>% 
  mutate(model = map2(data, spec, run_logit),
         glance = map(model, glance),
         coefs = map(model, tidy),
         augmented = map(model, augment),
         vif = map(model, get_vif))


#### Examine models ####

# get a table of fit statistics
fit <- select(all_data_models, race_cat, year1, spec_type, glance) %>% 
  unnest(cols = glance) %>% 
  group_by(year1, race_cat) %>% 
  mutate(
    ps_R2 = 1 - (deviance / null.deviance),
    min_aic = if_else(AIC == min(AIC), 1, 0),
    min_bic = if_else(BIC == min(BIC), 1, 0),
    max_R2 = if_else(ps_R2 == max(ps_R2), 1, 0)
  ) %>% 
  ungroup() %>% 
  arrange(race_cat, year1)

# create table tracking # of times best in fit stats for group-year
best_fit <- fit %>% 
  mutate(
    race_year = paste(race_cat, year1, sep = " - "),
    n_min = min_aic + min_bic + max_R2
  ) %>% 
  select(spec_type, race_year, n_min) %>% 
  spread(spec_type, n_min)

# lets look at overall distributions of R2 for specs within group-year
fit %>% 
  ggplot() +
  geom_point(aes(x = spec_type, y = ps_R2, col = spec_type)) +
  facet_grid(year1 ~ race_cat) +
  theme(axis.text.x = element_text(angle = 90))
# increases in R2 from adding interactions are pretty negligible
# but given my new theoretical take on homeownership, including those seems to be worth it
# on balance given the lack of inflated intercepts

# get a dataframe of VIF
vif_tbl <- select(all_data_models, race_cat, year1, spec_type, vif) %>% 
  unnest(cols = vif) %>% 
  mutate(race_year = paste(race_cat, year1, sep = " - ")) %>% 
  select(-c(race_cat, year1))

# summarize all vif generated by each specificaiton
vif_tbl %>% 
  melt() %>% 
  #select(spec_type, variable, value) %>% 
  spread(spec_type, value) %>% 
  as_tibble() %>% 
  summary()
# even linear with single interactions make VIF skyrocket

# get a table of coefficients from all models
coefs <- select(all_data_models, race_cat, year1, spec_type, coefs) %>% 
  unnest(cols = coefs) %>% 
  mutate(or = exp(estimate))

# let's look at ranges of exponentiated intercepts
coefs %>% 
  filter(term == "(Intercept)") %>% 
  ggplot() +
  geom_violin(aes(x = spec_type, y = or, col = spec_type)) +
  facet_grid(race_cat ~ .) +
  coord_flip()
# only linear models with no interactions or ownership interactions give reasonable intercept OR for black households

#### Presenting results

# Table of coefficients for black households
# Inlucing both "Linear, none" and "Linear, ownership" specs
coefs %>% 
  filter(race_cat == "Black" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  select(spec_type, year1, term, or) %>% 
  dcast(term ~ spec_type + year1) -> black_est
names(black_est)[-1] <- paste0(names(black_est)[-1], "_est")

coefs %>% 
  filter(race_cat == "Black" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  select(spec_type, year1, term, p.value) %>% 
  dcast(term ~ spec_type + year1) -> black_pval
names(black_pval)[-1] <- paste0(names(black_pval)[-1], "_pval")

black_coefs <- right_join(black_est, black_pval)
black_coefs <- cbind(black_coefs[,1], black_coefs[,names(black_coefs[,-1])[order(names(black_coefs[,-1]))]])
black_coefs <- black_coefs[c(1, 14, 13, 7, 
                             2, 11, 12, 6,
                             3, 4, 5,
                             8, 9, 10),]

write_csv(black_coefs, here("tables", "new_chp3", "black_logit_coefs.csv"))

# get R2 for each model
fit %>% 
  filter(race_cat == "Black" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  select(year1, spec_type, ps_R2)

# white immigrants
coefs %>% 
  filter(race_cat == "White Imm" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  select(spec_type, year1, term, or) %>% 
  dcast(term ~ spec_type + year1) -> wimm_est
names(wimm_est)[-1] <- paste0(names(wimm_est)[-1], "_est")

coefs %>% 
  filter(race_cat == "White Imm" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  select(spec_type, year1, term, p.value) %>% 
  dcast(term ~ spec_type + year1) -> wimm_pval
names(wimm_pval)[-1] <- paste0(names(wimm_pval)[-1], "_pval")

wimm_coefs <- right_join(wimm_est, wimm_pval)
wimm_coefs <- cbind(wimm_coefs[,1], wimm_coefs[,names(wimm_coefs[,-1])[order(names(wimm_coefs[,-1]))]])
wimm_coefs <- wimm_coefs[c(1, 14, 13, 7, 
                           2, 11, 12, 6,
                           3, 4, 5,
                           8, 9, 10),]

write_csv(wimm_coefs, here("tables", "new_chp3", "wimm_logit_coefs.csv"))

# get R2 for each model
fit %>% 
  filter(race_cat == "White Imm" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  select(year1, spec_type, ps_R2)

# native-born whites
coefs %>% 
  filter(race_cat == "White NB" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  select(spec_type, year1, term, or) %>% 
  dcast(term ~ spec_type + year1) -> wnb_est
names(wnb_est)[-1] <- paste0(names(wnb_est)[-1], "_est")

coefs %>% 
  filter(race_cat == "White NB" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  select(spec_type, year1, term, p.value) %>% 
  dcast(term ~ spec_type + year1) -> wnb_pval
names(wnb_pval)[-1] <- paste0(names(wnb_pval)[-1], "_pval")

wnb_coefs <- right_join(wnb_est, wnb_pval)
wnb_coefs <- cbind(wnb_coefs[,1], wnb_coefs[,names(wnb_coefs[,-1])[order(names(wnb_coefs[,-1]))]])
wnb_coefs <- wnb_coefs[c(1, 14, 13, 7, 
                         2, 11, 12, 6,
                         3, 4, 5,
                         8, 9, 10),]

write_csv(wnb_coefs, here("tables", "new_chp3", "wnb_logit_coefs.csv"))

# get R2 for each model
fit %>% 
  filter(race_cat == "White NB" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  select(year1, spec_type, ps_R2)

# keep only the ED-level models for predictions
all_data_models <- filter(all_data_models, spec_type %in% c("Linear, none", "Linear, ownership"))

# get N for each data set
all_data %>% 
  mutate(N = map(data, nrow)) %>% 
  select(race_cat, year1, N) %>% 
  unique()


#### Predicted probabilities of moving ####

# decide on "average" characteristics of a householder and family
model_vars <- c("moved", "woman", "married", "homeowner", "age1",
                "kids_1_to_2", "kids_3_up", "hh_max_sei1",
                "ed_pct_black1", "ed_pct_frnbrn1", "ed_mean_sei1")
#model_interacts <- c("hh_max_sei1:ed_pct_black1", "hh_max_sei1:ed_pct_frnbrn1", "hh_max_sei1:ed_mean_sei1")
summary(select(linked, model_vars))

# export a table of mean values for lag models
select(linked, race_cat, year1, model_vars) %>% 
  filter(race_cat != "Other") %>% 
  mutate(year1 = as.character(year1)) %>% 
  group_by(race_cat, year1) %>% 
  summarise_all(list(~mean(., na.rm = T))) %>% 
  melt() %>% 
  dcast(variable ~ race_cat + year1) %>% 
  write_csv(here("tables", "new_chp3", "logit_var_means.csv"))
  

# create 1-row tibble containing example householder and household
ex_own <- tibble(
  woman = 0,
  married = 1,
  homeowner = 1,
  age1 = 40,
  kids_1_to_2 = 1,
  kids_3_up = 0,
  hh_max_sei1 = 25,
  ex = "Owner"
)

ex_rent <- tibble(
  woman = 0,
  married = 1,
  homeowner = 0,
  age1 = 40,
  kids_1_to_2 = 1,
  kids_3_up = 0,
  hh_max_sei1 = 25,
  ex = "Renter"
)

#ex <- bind_rows(ex_own, ex_rent)

# function for making and processing predicted probabilities
my_predict <- function(model, new_data){
  # grab the ed numbers from data if present
  if ("ed1" %in% names(new_data)){
    ed_col <- new_data$ed1
  }
  
  # generate predictions
  predict(model, new_data, type = "response") %>% 
    enframe(name = NULL) %>% 
    rename(prob = value) -> out
  # return with or without Ed numbers depending on inputs
  if (exists("ed_col")){
    mutate(out, ED = ed_col)
  } else {
    out
  }
}

ed <- c(
  here("data", "ed_data", "ED_data_1910.csv"),
  here("data", "ed_data", "ED_data_1920.csv"),
  here("data", "ed_data", "ED_data_1930.csv")
) %>% 
  map(import) %>% 
  bind_rows() %>% 
  mutate(
    ed_pct_black_sq = ed_pct_black * ed_pct_black,
    ed_pct_frnbrn_sq = ed_pct_frnbrn * ed_pct_frnbrn,
    ed_mean_sei_sq = ed_mean_sei * ed_mean_sei
  )
names(ed) <- paste0(names(ed), "1")

# load polygons of 1910-1930 EDs
phl10 <- st_read(here("data", "shertzer_eds", "Philadelphia_1910.shp")) %>% mutate(year1 = 1910)
phl20 <- st_read(here("data", "shertzer_eds", "Philadelphia_1920.shp")) %>% mutate(year1 = 1920)
phl30 <- st_read(here("data", "shertzer_eds", "Philadelphia_1930.shp")) %>% mutate(year1 = 1930)

# now merge all ED characteristics from each year with example data and predict moving probabilities
predictions <- rbind(
    cbind(ed, ex_own), 
    cbind(ed, ex_rent)
  ) %>%
  as_tibble() %>% 
  group_by(year1) %>% 
  nest() %>% 
  rename(ex_data = data) %>% 
  left_join(all_data_models, "year1") %>% 
  mutate(
    # create example predictions
    ex_pred = map2(model, ex_data, my_predict),
    # extract predictions of observed data
    obs_pred = map2(model, data, my_predict)
  )

#predictions_rent <- cbind(ed, ex_rent) %>% 
#  as_tibble() %>% 
#  group_by(year1) %>% 
#  nest() %>% 
#  rename(ex_data = data) %>% 
#  left_join(all_data_models, "year1") %>% 
#  mutate(ex_pred = map2(model, ex_data, my_predict))

# select the example predictions and unnest into a single data frame
select(predictions, spec_type, year1, race_cat, ex_data, ex_pred) %>% 
  unnest(c(ex_data, ex_pred)) %>% 
  mutate(ED = as.character(ED)) %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  # add column of intervals for choropleth maps
  mutate(
    prob_map = factor(
      case_when(
        prob < 0.25 ~ 1,
        prob >= 0.25 & prob < 0.375 ~ 2,
        prob >= 0.375 & prob < 0.5 ~ 3,
        prob >= 0.5 & prob < 0.625 ~ 4,
        prob >= 0.625 ~ 5
      )
    )
  ) -> ex_predictions


# make separate table of observed data predictions
select(predictions, spec_type, year1, race_cat, data, obs_pred) %>% 
  unnest(c(data, obs_pred)) %>% 
  mutate(ED = as.character(ED)) %>% 
  # group by ED and other ID vars and calculate actual move rates vs. mean predicted probabilities
  group_by(ED, year1, race_cat, spec_type) %>% 
  summarise(
    N = n(),
    move_rate = sum(moved) / N,
    mean_pred = mean(prob),
    dev = mean_pred - move_rate
  ) %>% 
  ungroup() %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  # add column of intervals for choropleth maps
  mutate(
    dev_map = factor(case_when(
      N < 6 ~ "Low N",
      dev <= -0.25 ~ "-0.25 and below",
      dev > -0.25 & dev <= -0.10 ~ "-0.25 to -0.1",
      dev > -0.1 & dev <= -0.05 ~ "-0.1 to -0.05",
      dev > -0.05 & dev <= 0.05 ~ "-0.05 to 0.05",
      dev > 0.05 & dev <= 0.1 ~ "0.05 to 0.1",
      dev > 0.1 & dev <= 0.25 ~ "0.1 to 0.25",
      dev > 0.25 ~ "0.25 and above"
    ),
    levels = c("-0.25 and below", "-0.25 to -0.1", "-0.1 to -0.05", "-0.05 to 0.05", 
               "0.05 to 0.1", "0.1 to 0.25", "0.25 and above", "Low N"))
  ) -> obs_predictions

# create faceted plot
# I've retained all the model specificiations in "predictions" object
# need to specificy which models to use for each group-year


predictions %>% 
  filter(race_cat == "White NB" & year1 == 1920) %>% nrow()
  pull(prob_map) %>% 
  table()

# compare specs using NB White in 1920-30 (White NB)
ex_predictions %>% 
  filter(race_cat == "White NB" & year1 == 1930) %>% 
ggplot() +
  geom_sf(aes(fill = prob_map), lwd = 0, col = "white") +
  scale_fill_brewer("Predicted\nProbability", 
                    palette = "BuPu", 
                    labels = c("< 0.25", "0.25 to 0.375", "0.375 to 0.5", "0.5 to 0.625", ">= 0.625")) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(spec_type ~ ex) +
  ggsave(here("figures", "new_chp3", "map_testing_owner_interactions_wnb_1910.png"), width = 9, height = 11)


# plot dev of mean predicted probabilities from observed rates(White NB)
obs_predictions %>% 
  filter(race_cat == "White NB" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  ggplot() +
  geom_sf(aes(fill = dev_map), lwd = 0, col = "white") +
  scale_fill_manual(values = c(brewer.pal(7, "RdBu"), "grey")) +
  theme_minimal() +
  labs(fill = "Predicted rate\nDeviation") +
  theme(axis.text = element_blank()) +
  facet_grid(year1 ~ spec_type) +
  ggsave(here("figures", "new_chp3", "map_dev_owner_interactions_wnb.png"), width = 9, height = 11)


obs_predictions %>% 
  filter(race_cat == "White Imm" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  ggplot() +
  geom_sf(aes(fill = dev_map), lwd = 0, col = "white") +
  scale_fill_manual(values = c(brewer.pal(7, "RdBu"), "grey")) +
  theme_minimal() +
  labs(fill = "Predicted rate\nDeviation") +
  theme(axis.text = element_blank()) +
  facet_grid(year1 ~ spec_type) +
  ggsave(here("figures", "new_chp3", "map_dev_owner_interactions_wimm.png"), width = 9, height = 11)


obs_predictions %>% 
  filter(race_cat == "Black" & spec_type %in% c("Linear, none", "Linear, ownership")) %>% 
  ggplot() +
  geom_sf(aes(fill = dev_map), lwd = 0, col = "white") +
  scale_fill_manual(values = c(brewer.pal(7, "RdBu"), "grey")) +
  theme_minimal() +
  labs(fill = "Predicted rate\nDeviation") +
  theme(axis.text = element_blank()) +
  facet_grid(year1 ~ spec_type) +
  ggsave(here("figures", "new_chp3", "map_dev_owner_interactions_black.png"), width = 9, height = 11)



#### Predictions along single variables ####

# Ok, now lets examine the overall distributions of ED variables and their neighboring areas
summary(select(ed, ed_pct_black1, ed_pct_frnbrn1, ed_mean_sei1))

# create an example "average" ED that varies % black fomr 0% to 100%
# % black = 2
# % immigrant = 15
# mean SEI = 12
ex_ed_pblack <- tibble(
  ed_pct_black1 = seq(0, 100, 2),
  ed_pct_frnbrn1 = rep(15, 51),
  ed_mean_sei1 = rep(12, 51)
) %>% 
  mutate(
    ed_pct_black_sq1 = ed_pct_black1 * ed_pct_black1,
    ed_pct_frnbrn_sq1 = ed_pct_frnbrn1 * ed_pct_frnbrn1,
    ed_mean_sei_sq1 = ed_mean_sei1 * ed_mean_sei1
  )

ex_ed_pfrnbrn <- tibble(
  ed_pct_black1 = rep(2, 51),
  ed_pct_frnbrn1 = seq(0, 100, 2),
  ed_mean_sei1 = rep(12, 51)
) %>% 
  mutate(
    ed_pct_black_sq1 = ed_pct_black1 * ed_pct_black1,
    ed_pct_frnbrn_sq1 = ed_pct_frnbrn1 * ed_pct_frnbrn1,
    ed_mean_sei_sq1 = ed_mean_sei1 * ed_mean_sei1
  )

ex_ed_msei <- tibble(
  ed_pct_black1 = rep(2, 51),
  ed_pct_frnbrn1 = rep(15, 51),
  ed_mean_sei1 = seq(0, 50, 1)
) %>% 
  mutate(
    ed_pct_black_sq1 = ed_pct_black1 * ed_pct_black1,
    ed_pct_frnbrn_sq1 = ed_pct_frnbrn1 * ed_pct_frnbrn1,
    ed_mean_sei_sq1 = ed_mean_sei1 * ed_mean_sei1
  )

# combine hypothetical EDs and households
ex_ed_pblack <- all_row_combos(rbind(ex1, ex2), ex_ed_pblack)
ex_ed_pfrnbrn <- all_row_combos(rbind(ex1, ex2), ex_ed_pfrnbrn)
ex_ed_msei <- all_row_combos(rbind(ex1, ex2), ex_ed_msei)

# combine with 9 models for groups and years
all_data_models %>% 
  #filter(spec_type == "ED") %>% 
  select(race_cat, year1, model) %>% 
  mutate(ex_data = rep(list(ex_ed_pblack), 9),
         pred = map2(model, ex_data, my_predict)) %>% 
  select(race_cat, year1, ex_data, pred) %>% 
  unnest(cols = c(ex_data, pred)) -> ed_pblack_preds

all_data_models %>% 
  #filter(spec_type == "ED") %>% 
  select(race_cat, year1, model) %>% 
  mutate(ex_data = rep(list(ex_ed_pfrnbrn), 9),
         pred = map2(model, ex_data, my_predict)) %>% 
  select(race_cat, year1, ex_data, pred) %>% 
  unnest(cols = c(ex_data, pred)) -> ed_pfrnbrn_preds

all_data_models %>% 
  #filter(spec_type == "ED") %>% 
  select(race_cat, year1, model) %>% 
  mutate(ex_data = rep(list(ex_ed_msei), 9),
         pred = map2(model, ex_data, my_predict)) %>% 
  select(race_cat, year1, ex_data, pred) %>% 
  unnest(cols = c(ex_data, pred)) -> ed_msei_preds





# create line plots of predictions, facet by year
# % black
ggplot(ed_pblack_preds) +
  geom_line(aes(x = ed_pct_black1, y = prob, col = race_cat, lty = ex)) +
  labs(x = "ED % black",
       y = "Probability of moving",
       col = "",
       lty = "") +
  facet_grid(.~ year1) +
  xlim(0, 50) +
  ggsave(here("figures", "chp2", "logit", "pblack_ex_preds_ed.pdf"), height = 3, width = 6)

ggplot(ed_pfrnbrn_preds) +
  geom_line(aes(x = ed_pct_frnbrn1, y = prob, col = race_cat, lty = ex)) +
  labs(x = "ED % immigrant",
       y = "Probability of moving",
       col = "",
       lty = "") +
  facet_grid(. ~ year1) +
  xlim(0, 65) +
  ggsave(here("figures", "chp2", "logit", "pfrnbrn_ex_preds_ed.pdf"), height = 3, width = 6)

ggplot(ed_msei_preds) +
  geom_line(aes(x = ed_mean_sei1, y = prob, col = race_cat, lty = ex)) +
  labs(x = "ED mean SEI",
       y = "Probability of moving",
       col = "",
       lty = "") +
  facet_grid(. ~ year1) +
  xlim(0, 30) +
  ggsave(here("figures", "chp2", "logit", "msei_ex_preds_ed.pdf"), height = 3, width = 6)








### let's make a map giving visual context for move distance of 750 meters
# can I even make axis limits work with sf objects?
ggplot() +
  geom_sf(data = phl10) +
  coord_sf(xlim = c(-75.25, -75.15), ylim = c(39.95, 40.05))








