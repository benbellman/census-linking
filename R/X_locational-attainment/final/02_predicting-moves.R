library(here)
library(rio)
library(tibble)
library(dplyr)
library(tidyr)
library(broom)
library(data.table)
library(ggplot2)
library(sf)
library(car)
library(readr)
library(tidyr)
library(ghibli)

source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "aggregate_microdata.R"))

linked <- import(here("data", "for_models", "phl_loc_attain.csv")) %>% 
  as_tibble() %>% 
  filter(sei1 > 0 & not_hhh1 == 0) %>% 
  # if the address is the same in both years, the family did not move
  mutate(#moved = if_else(both_same == 1, 0, 1),
         woman = sex1 - 1,
         married = if_else(marst1 < 3, 1, 0),
         age_sq1 = age1 * age1,
         #hh_n_adults1 = hh_size1 - hh_n_kids1,
         kids_1_to_2 = if_else(hh_n_kids1 > 0 & hh_n_kids1 < 3, 1, 0),
         kids_3_up = if_else(hh_n_kids1 >= 3, 1, 0),
         # create squared term for ed % black and ed % foreign born
         ed_pct_black_sq1 = ed_pct_black1 * ed_pct_black1,
         ed_pct_frnbrn_sq1 = ed_pct_frnbrn1 * ed_pct_frnbrn1,
         ed_mean_sei_sq1 = ed_mean_sei1 * ed_mean_sei1,
         lag_pct_black_sq1 = lag_pct_black1 * lag_pct_black1,
         lag_pct_frnbrn_sq1 = lag_pct_frnbrn1 * lag_pct_frnbrn1)

# split data by year for merging with ED distances
data_list <- list(
  filter(linked, year1 == 1910),
  filter(linked, year1 == 1920),
  filter(linked, year1 == 1930)
)

# merge with pre-calcualted ED distances
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

# mark as moved if distance is greater than 750 m
linked <- linked %>% 
  mutate(moved = if_else(dist > 750, 1, 0))

nested <- linked %>%
  group_by(race_cat, year1) %>% 
  nest() %>% 
  filter(race_cat != "Other")

#select(nested, race_cat, year1)


# split data according to race category
#white_nb <- filter(linked, race_cat == "White NB")
#white_imm <- filter(linked, race_cat == "White Imm")
#black <- filter(linked, race_cat == "Black")


#### Model specifications ####

ed_fm <- moved ~
  woman + married + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_mean_sei1

ed_fm_sq <- moved ~
  woman + married + age1 + age_sq1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_black_sq1 +
  ed_pct_frnbrn1 + ed_pct_frnbrn_sq1 +
  ed_mean_sei1 + ed_mean_sei_sq1 +
  hh_max_sei1*ed_pct_black1 + hh_max_sei1*ed_pct_black_sq1 + 
  hh_max_sei1*ed_pct_frnbrn1 + hh_max_sei1*ed_pct_frnbrn_sq1 + 
  hh_max_sei1*ed_mean_sei1 + hh_max_sei1*ed_mean_sei_sq1

ed_lag_fm <- moved ~
  woman + married + age1 + hh_max_sei1 + kids_1_to_2 + kids_3_up +
  ed_pct_black1 + ed_pct_frnbrn1 + ed_mean_sei1 +
  lag_pct_black1 + lag_pct_frnbrn1 + lag_mean_sei1 +
  hh_max_sei1*lag_pct_black1 + hh_max_sei1*lag_pct_frnbrn1 + hh_max_sei1*lag_mean_sei1

# combine all data with all model specifications
all_data <- bind_cols(
  bind_rows(rep(list(nested), 3)),
  tibble(spec = c(rep(list(ed_fm), 9), rep(list(ed_fm_sq), 9), rep(list(ed_lag_fm), 9)),
         spec_type = factor(c(rep("ED", 9), rep("ED quad", 9), rep("ED and Lagged", 9)), 
                            levels = c("ED", "ED quad", "ED and Lagged")))
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
  unnest(cols = glance)

# manually look at fit statistics within groups and across years
#filter(fit, race_cat == "White Imm" & year1 == 1930)

# get a dataframe of VIF
#vif_tbl <- select(all_data_models, race_cat, year1, spec_type, vif) %>% 
#  unnest(cols = vif)

# get a table of coefficients from all models
coefs <- select(all_data_models, race_cat, year1, spec_type, coefs) %>% 
  unnest(cols = coefs) %>% 
  mutate(or = exp(estimate))

#### Presenting results

# lets make a faceted plot of ED-level terms over time
coefs %>% 
  filter(term %in% c("ed_pct_black1", "ed_pct_frnbrn1", "lag_pct_black1", "lag_pct_frnbrn1")) %>% 
  filter(p.value < 0.05) %>% 
  mutate(min = estimate - std.error,
         max = estimate + std.error,
         Term = case_when(
           term == "ed_pct_black1" ~ "ED % black",
           term == "ed_pct_frnbrn1" ~ "ED % immigrant",
           term == "lag_pct_black1" ~ "Lag % black",
           term == "lag_pct_frnbrn1" ~ "Lag % immigrant"
         )) %>% 
  ggplot() +
  geom_line(aes(x = year1, y = estimate, col = Term)) +
  geom_point(aes(x = year1, y = estimate, col = Term), size = 0.75) +
  geom_errorbar(aes(x = year1, ymin = min, ymax = max, col = Term), width = 1) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  scale_x_continuous(name = "Year",
                     breaks = c(1910, 1920, 1930)) +
  facet_grid(race_cat ~ spec_type) +
  labs(y = "Estimate") +
  theme(axis.text.x =  element_text(angle = 30, hjust = 1)) +
  ggsave(here("figures", "chp2", "compare_logit_spec.pdf"), height = 4, width = 7)


# table of all AIC fit statistics for all specifications
fit %>% 
  select(race_cat, year1, spec_type, AIC) %>% 
  group_by(race_cat) %>% 
  group_split() %>% 
  map(select, -race_cat) %>% 
  map(dcast, spec_type ~ year1) -> aic_tables

write_csv(aic_tables[[1]], here("tables", "chp2", "black_logit_spec_aic.csv"))
write_csv(aic_tables[[2]], here("tables", "chp2", "wimm_logit_spec_aic.csv"))
write_csv(aic_tables[[3]], here("tables", "chp2", "wnb_logit_spec_aic.csv"))

# full table of coefficients of lag spec for all groups + years
coefs %>% 
  filter(spec_type == "ED quad") %>% 
  select(race_cat, year1, term, or) %>% 
  dcast(term ~ race_cat + year1) -> lag_est
names(lag_est)[-1] <- paste0(names(lag_est)[-1], "_est")

coefs %>% 
  filter(spec_type == "ED quad") %>% 
  select(race_cat, year1, term, p.value) %>% 
  dcast(term ~ race_cat + year1) -> lag_pval
names(lag_pval)[-1] <- paste0(names(lag_pval)[-1], "_pval")

lag_coefs <- right_join(lag_est, lag_pval)
lag_coefs <- cbind(lag_coefs[,1], lag_coefs[,names(lag_coefs[,-1])[order(names(lag_coefs[,-1]))]])
lag_coefs <- lag_coefs[c(1, 20, 19, 3, 2, 
                         17, 18, 10, 
                         7, 6, 9, 8, 5, 4,
                         14, 13, 16, 15, 12, 11),]

write_csv(lag_coefs, here("tables", "chp2", "logit_coefs_or_750m_sq.csv"))


# keep only the ED-level models for predictions
all_data_models <- filter(all_data_models, spec_type == "ED quad")


#### Predicted probabilities of moving ####

# decide on "average" characteristics of a householder and family
model_vars <- c("moved", "woman", "married", "age1",
                "kids_1_to_2", "kids_3_up", "hh_max_sei1",
                "ed_pct_black1", "ed_pct_frnbrn1", "ed_mean_sei1")
model_interacts <- c("hh_max_sei1:ed_pct_black1", "hh_max_sei1:ed_pct_frnbrn1", "hh_max_sei1:ed_mean_sei1")
summary(select(linked, model_vars))

# export a table of mean values for lag models
select(linked, race_cat, year1, model_vars) %>% 
  filter(race_cat != "Other") %>% 
  mutate(year1 = as.character(year1)) %>% 
  group_by(race_cat, year1) %>% 
  summarise_all(list(~mean(., na.rm = T))) %>% 
  melt() %>% 
  dcast(variable ~ race_cat + year1) %>% 
  write_csv(here("tables", "chp2", "logit_var_means.csv"))
  

# create 1-row tibble containing example householder and household
ex1 <- tibble(
  woman = 0,
  married = 1,
  age1 = 40,
  age_sq1 = 35*35,
  kids_1_to_2 = 1,
  kids_3_up = 0,
  hh_max_sei1 = 20,
  ex = "SEI = 20"
)

ex2 <- tibble(
  woman = 0,
  married = 1,
  age1 = 40,
  age_sq1 = 50*50,
  kids_1_to_2 = 1,
  kids_3_up = 0,
  hh_max_sei1 = 55,
  ex = "SEI = 55"
)

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


# collect all time 1 ED and lagged-ED variables
# load and aggregate data to ED level
#ed <- c(10, 20, 30) %>% 
#  map(aggregate_microdata, ed) %>% 
#  map(mutate, ed = as.character(ed)) %>% 
#  bind_rows()
#names(ed)[-(1:2)] <- paste0("ed_", names(ed)[-(1:2)])
#names(ed) <- paste0(names(ed), "1")

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

# select the predictions and unnest into a single data frame
select(predictions1, year1, race_cat, ex_pred) %>% 
  unnest(cols = c(ex_pred)) %>% 
  mutate(ED = as.character(ED)) %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  # add column of intervals for choropleth maps
  mutate(
    prob_int = factor(
      case_when(
        prob < 0.25 ~ 1,
        prob >= 0.25 & prob < 0.375 ~ 2,
        prob >= 0.375 & prob < 0.5 ~ 3,
        prob >= 0.5 & prob < 0.625 ~ 4,
        prob >= 0.625 ~ 5
      )
    )
  ) -> ed_move_preds1

select(predictions2, year1, race_cat, ex_pred) %>% 
  unnest(cols = c(ex_pred)) %>% 
  mutate(ED = as.character(ED)) %>% 
  # combine ED polygons from all years and merge with all predictions
  left_join(rbind(phl10, phl20, phl30)) %>% 
  # add column of intervals for choropleth maps
  mutate(
    prob_int = factor(
      case_when(
        prob < 0.25 ~ 1,
        prob >= 0.25 & prob < 0.375 ~ 2,
        prob >= 0.375 & prob < 0.5 ~ 3,
        prob >= 0.5 & prob < 0.625 ~ 4,
        prob >= 0.625 ~ 5
      )
    )
  ) -> ed_move_preds2

# slice off a test file
test <- filter(ed_move_preds, year1 == 1930 & race_cat == "White Imm")

# create faceted plot
# I've retained all the model specificiations in "predictions" object
# need to specificy which models to use for each group-year

ggplot(ed_move_preds1) +
  geom_sf(aes(fill = prob_int), lwd = 0, col = "white") +
  scale_fill_brewer("Predicted\nProbability", 
                    palette = "BuPu", 
                    labels = c("< 0.25", "0.25 to 0.375", "0.375 to 0.5", "0.5 to 0.625", ">= 0.625")) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(race_cat ~ year1) +
  ggsave(here("figures", "chp2", "logit", "final-predicted_move_probabilities_ex1_ed_750sq.png"), height = 7, width = 7)


ggplot(ed_move_preds2) +
  geom_sf(aes(fill = prob_int), lwd = 0, col = "white") +
  scale_fill_brewer("Predicted\nProbability", 
                    palette = "BuPu", 
                    labels = c("< 0.25", "0.25 to 0.375", "0.375 to 0.5", "0.5 to 0.625", ">= 0.625")) +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_grid(race_cat ~ year1) +
  ggsave(here("figures", "chp2", "logit", "final-predicted_move_probabilities_ex2_ed_750sq.png"), height = 7, width = 7)



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








