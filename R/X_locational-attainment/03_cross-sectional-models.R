library(here)
library(rio)
library(dplyr)
library(estimatr)

source(here("R", "functions", "load_linked_sample.R"))
source(here("R", "functions", "aggregate_microdata.R"))


#### Set Up Data ####

#set individual variables to keep
ind_cols <- c("uniqueid", "pernum", "year", "serial", "relate", "ed", 
              "age", "sex", "race", "race_grp", "marst", "nchild", "bpl", "nativity", "citizen", "mtongue",
              "school", "occ1950", "occscore", "sei", "ind1950", "erscor50", "edscor50", "npboss50")

# load all years
p10 <- load_microdata(10, formatted = F) %>% 
  select(ind_cols) %>% 
  filter(relate == 101) %>% 
  mutate(black = if_else(race_grp == "Black", 1, 0),
         other = if_else(race_grp == "Other", 1, 0),
         white = if_else(black == 0 & other == 0, 1, 0),
         split_grp = case_when(
           black == 1 ~ "Black",
           other == 1 ~ "Other",
           white == 1 & nativity != 5 ~ "White Nat.",
           white == 1 & nativity == 5 ~ "White Imm."
         ),
         woman = sex - 1,
         married = if_else(marst < 3, 1, 0),
         frnbrn = if_else(nativity == 5, 1, 0),
         pa_born = if_else(bpl == 4200, 1, 0),
         age_sq = age * age,
         ed = as.character(ed))
h10 <- load_microdata(10, formatted = F) %>% 
  group_by(serial) %>% 
  summarize(hh_size = n(),
            hh_max_sei = max(sei),
            hh_sum_sei = sum(sei, na.rm = T),
            hh_n_kids = sum(if_else(age < 18, 1, 0)),
            hh_n_adults = hh_size - hh_n_kids,
            hh_n_labforce = sum(if_else(labforce == 2, 1, 0)),
            hh_p_labforce = hh_n_labforce / hh_size,
            hh_mean_sei = hh_sum_sei / hh_n_labforce) %>% 
  ungroup()
ed10 <- aggregate_microdata(10, ed) %>% 
  mutate(ed = as.character(ed))
phl10 <- left_join(p10, h10) %>% left_join(ed10)

#
p20 <- load_microdata(20, formatted = F) %>% 
  select(ind_cols) %>% 
  filter(relate == 101) %>% 
  mutate(black = if_else(race_grp == "Black", 1, 0),
         other = if_else(race_grp == "Other", 1, 0),
         white = if_else(black == 0 & other == 0, 1, 0),
         split_grp = case_when(
           black == 1 ~ "Black",
           other == 1 ~ "Other",
           white == 1 & nativity != 5 ~ "White Nat.",
           white == 1 & nativity == 5 ~ "White Imm."
         ),
         woman = sex - 1,
         married = if_else(marst < 3, 1, 0),
         frnbrn = if_else(nativity == 5, 1, 0),
         pa_born = if_else(bpl == 4200, 1, 0),
         age_sq = age * age,
         ed = as.character(ed))
h20 <- load_microdata(20, formatted = F) %>% 
  group_by(serial) %>% 
  summarize(hh_size = n(),
            hh_max_sei = max(sei),
            hh_sum_sei = sum(sei, na.rm = T),
            hh_n_kids = sum(if_else(age < 18, 1, 0)),
            hh_n_adults = hh_size - hh_n_kids,
            hh_n_labforce = sum(if_else(labforce == 2, 1, 0)),
            hh_p_labforce = hh_n_labforce / hh_size,
            hh_mean_sei = hh_sum_sei / hh_n_labforce) %>% 
  ungroup()
ed20 <- aggregate_microdata(20, ed) %>% 
  mutate(ed = as.character(ed))
phl20 <- left_join(p20, h20) %>% left_join(ed20)

#
p30 <- load_microdata(30, formatted = F) %>% 
  select(ind_cols) %>% 
  filter(relate == 101) %>% 
  mutate(black = if_else(race_grp == "Black", 1, 0),
         other = if_else(race_grp == "Other", 1, 0),
         white = if_else(black == 0 & other == 0, 1, 0),
         split_grp = case_when(
           black == 1 ~ "Black",
           other == 1 ~ "Other",
           white == 1 & nativity != 5 ~ "White Nat.",
           white == 1 & nativity == 5 ~ "White Imm."
         ),
         woman = sex - 1,
         married = if_else(marst < 3, 1, 0),
         frnbrn = if_else(nativity == 5, 1, 0),
         pa_born = if_else(bpl == 4200, 1, 0),
         age_sq = age * age,
         ed = as.character(ed))
h30 <- load_microdata(30, formatted = F) %>% 
  group_by(serial) %>% 
  summarize(hh_size = n(),
            hh_max_sei = max(sei),
            hh_sum_sei = sum(sei, na.rm = T),
            hh_n_kids = sum(if_else(age < 18, 1, 0)),
            hh_n_adults = hh_size - hh_n_kids,
            hh_n_labforce = sum(if_else(labforce == 2, 1, 0)),
            hh_p_labforce = hh_n_labforce / hh_size,
            hh_mean_sei = hh_sum_sei / hh_n_labforce) %>% 
  ungroup()
ed30 <- aggregate_microdata(30, ed) %>% 
  mutate(ed = as.character(ed))
phl30 <- left_join(p30, h30) %>% left_join(ed30)

#
p40 <- load_microdata(40, formatted = F) %>% 
  select(ind_cols) %>% 
  filter(relate == 101) %>% 
  mutate(black = if_else(race_grp == "Black", 1, 0),
         other = if_else(race_grp == "Other", 1, 0),
         white = if_else(black == 0 & other == 0, 1, 0),
         split_grp = case_when(
           black == 1 ~ "Black",
           other == 1 ~ "Other",
           white == 1 & bpl < 15000 ~ "White Nat.",
           white == 1 & bpl >= 15000  ~ "White Imm."
         ),
         woman = sex - 1,
         married = if_else(marst < 3, 1, 0),
         frnbrn = if_else(bpl >= 15000, 1, 0),
         pa_born = if_else(bpl == 4200, 1, 0),
         age_sq = age * age,
         ed = as.character(ed))
h40 <- load_microdata(40, formatted = F) %>% 
  group_by(serial) %>% 
  summarize(hh_size = n(),
            hh_max_sei = max(sei),
            hh_sum_sei = sum(sei, na.rm = T),
            hh_n_kids = sum(if_else(age < 18, 1, 0)),
            hh_n_adults = hh_size - hh_n_kids,
            hh_n_labforce = sum(if_else(labforce == 2, 1, 0)),
            hh_p_labforce = hh_n_labforce / hh_size,
            hh_mean_sei = hh_sum_sei / hh_n_labforce) %>% 
  ungroup()
ed40 <- aggregate_microdata(40, ed) %>% 
  mutate(ed = as.character(ed))
phl40 <- left_join(p40, h40) %>% left_join(ed40)

# remove source files to clear clutter
rm(ed10, ed20, ed30, ed40)
rm(h10, h20, h30, h40)
rm(p10, p20, p30, p40)


#### Models ####

# create formulas
pb_formula <- pct_black ~ age + age_sq + woman + married + pa_born + sei + hh_n_adults + hh_n_kids + hh_max_sei
sei_formula <- mean_sei ~ age + age_sq + woman + married + pa_born + sei + hh_n_adults + hh_n_kids + hh_max_sei

## let's model % black for all three groups in 1910
# black
b_pb_10 <- lm_robust(formula = pb_formula,
                     data = filter(phl10, split_grp == "Black"))
b_pb_20 <- lm_robust(formula = pb_formula,
                     data = filter(phl20, split_grp == "Black"))
b_pb_30 <- lm_robust(formula = pb_formula,
                     data = filter(phl30, split_grp == "Black"))
b_pb_40 <- lm_robust(formula = pb_formula,
                     data = filter(phl40, split_grp == "Black"))

summary(b_pb_10)
summary(b_pb_20)
summary(b_pb_30)
summary(b_pb_40)


# white immigrants
wi_pb_10 <- lm_robust(formula = pb_formula,
                     data = filter(phl10, split_grp == "White Imm."))
wi_pb_20 <- lm_robust(formula = pb_formula,
                     data = filter(phl20, split_grp == "White Imm."))
wi_pb_30 <- lm_robust(formula = pb_formula,
                     data = filter(phl30, split_grp == "White Imm."))
wi_pb_40 <- lm_robust(formula = pb_formula,
                     data = filter(phl40, split_grp == "White Imm."))

summary(wi_pb_10)
summary(wi_pb_20)
summary(wi_pb_30)
summary(wi_pb_40)


# native-born whites
wn_pb_10 <- lm_robust(formula = pb_formula,
                      data = filter(phl10, split_grp == "White Nat."))
wn_pb_20 <- lm_robust(formula = pb_formula,
                      data = filter(phl20, split_grp == "White Nat."))
wn_pb_30 <- lm_robust(formula = pb_formula,
                      data = filter(phl30, split_grp == "White Nat."))
wn_pb_40 <- lm_robust(formula = pb_formula,
                      data = filter(phl40, split_grp == "White Nat."))

summary(wn_pb_10)
summary(wn_pb_20)
summary(wn_pb_30)
summary(wn_pb_40)


## now let's model mean SEI for all three groups in 1910
# black
b_sei_10 <- lm_robust(formula = sei_formula,
                     data = filter(phl10, split_grp == "Black"))
b_sei_20 <- lm_robust(formula = sei_formula,
                     data = filter(phl20, split_grp == "Black"))
b_sei_30 <- lm_robust(formula = sei_formula,
                     data = filter(phl30, split_grp == "Black"))
b_sei_40 <- lm_robust(formula = sei_formula,
                     data = filter(phl40, split_grp == "Black"))

summary(b_sei_10)
summary(b_sei_20)
summary(b_sei_30)
summary(b_sei_40)


# white immigrants
wi_sei_10 <- lm_robust(formula = sei_formula,
                      data = filter(phl10, split_grp == "White Imm."))
wi_sei_20 <- lm_robust(formula = sei_formula,
                      data = filter(phl20, split_grp == "White Imm."))
wi_sei_30 <- lm_robust(formula = sei_formula,
                      data = filter(phl30, split_grp == "White Imm."))
wi_sei_40 <- lm_robust(formula = sei_formula,
                      data = filter(phl40, split_grp == "White Imm."))

summary(wi_sei_10)
summary(wi_sei_20)
summary(wi_sei_30)
summary(wi_sei_40)


# native-born whites
wn_sei_10 <- lm_robust(formula = sei_formula,
                      data = filter(phl10, split_grp == "White Nat."))
wn_sei_20 <- lm_robust(formula = sei_formula,
                      data = filter(phl20, split_grp == "White Nat."))
wn_sei_30 <- lm_robust(formula = sei_formula,
                      data = filter(phl30, split_grp == "White Nat."))
wn_sei_40 <- lm_robust(formula = sei_formula,
                      data = filter(phl40, split_grp == "White Nat."))

summary(wn_sei_10)
summary(wn_sei_20)
summary(wn_sei_30)
summary(wn_sei_40)


