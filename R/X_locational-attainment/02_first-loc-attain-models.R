library(here)
library(rio)
library(dplyr)
library(tidyr)
library(readr)
library(estimatr)

# Load file for locational attainment models
# filter out records marked as "other" race, no recorded occupation/SEI
links <- import(here("data", "for_models", "phl_loc_attain.csv")) %>% 
  filter(other != 1) %>% 
  filter(sei1 > 0) %>% 
  mutate(woman = sex1 - 1,
         married = if_else(marst1 < 3, 1, 0),
         frnbrn = if_else(nativity1 == 5, 1, 0),
         age_sq1 = age1 * age1,
         hh_n_adults1 = hh_size1 - hh_n_kids1) %>% 
  as_tibble()

# create change dependent variables
links$ed_pct_black_chg <- links$ed_pct_black2 - links$ed_pct_black1
links$ed_mean_sei_chg <- links$ed_mean_sei2 - links$ed_mean_sei1

### common birthplace tabulation (including as descriptive table in Excel)
# tabulate by year1 as well
bpl_table <- links %>% 
  mutate(year1 = paste0("x", year1)) %>% 
  group_by(bpl1, year1) %>% 
  summarise(n = n()) %>% 
  spread(year1, n) %>% 
  mutate(total = sum(x1910, x1920, x1930, na.rm = T)) %>% 
  arrange(-total)
write_csv(bpl_table, here("tables", "links_bpl_freq.csv"))

# add variable for splitting into race/ethnicity groups
links <- mutate(links,
                # first determine race based on both time points
                black = if_else(race_grp1 == "Black" | race_grp2 == "Black", 1, 0),
                other = if_else(race_grp1 == "Other" | race_grp2 == "Other", 1, 0),
                white = if_else(black == 0 & other == 0, 1, 0),
                # create split variable
                split_grp = case_when(
                  black == 1 ~ "Black",
                  other == 1 ~ "Other",
                  white == 1 & nativity1 != 5 ~ "White Nat.",
                  white == 1 & nativity1 == 5 ~ "White Imm."
                ))

# split the data
data_list <- split(links, links$split_grp)


#### Black Modelling ####

## model objects are named according to:
# sample: b, wn, wi
# dep var: pctblack, sei (eventually family structure or population density)

# specify a full model for ED pct black (not change)
# SE clustered on T1 ED 
b_pctblack <- lm_robust(ed_pct_black2 ~ 
                             # vars of interest
                             ed_pct_black1 + sei1 + #ed_pct_black1*sei1 +
                             # control vars
                             age1 + age_sq1 + woman + married + not_hhh1 + hh_n_adults1 + hh_n_kids1 + hh_max_sei1 + factor(year1),
                           cluster = ed1,
                           data = data_list[["Black"]])

summary(b_pctblack)
print(b_pctblack)
tidy(b_pctblack)


# Same model but predicing change from T1
summary(data_list[["Black"]]$ed_pct_black_chg)
b_pctblackchg <- lm_robust(ed_pct_black_chg ~ 
                             # vars of interest
                             sei1 +
                             # control vars
                             age1 + age_sq1 + woman + married + not_hhh1 + hh_n_adults1 + hh_n_kids1,
                           cluster = ed1,
                           data = data_list[["Black"]])

summary(b_pctblackchg)
print(b_pctblackchg)
tidy(b_pctblackchg)



#### White Immigrant Modelling ####

# specify a full model for ED pct black (not change)
# SE clustered on T1 ED 
wi_pctblack <- lm_robust(ed_pct_black2 ~ 
                          # vars of interest
                          ed_pct_black1 + sei1 + #ed_pct_black1*sei1 +
                          # control vars
                          age1 + age_sq1 + woman + married + not_hhh1 + hh_n_adults1 + hh_n_kids1 + hh_max_sei1 + factor(year1),
                        cluster = ed1,
                        data = data_list[["White Imm."]])

summary(wi_pctblack)
print(wi_pctblack)
tidy(wi_pctblack)


# Same model but predicing change from T1
summary(data_list[["White Imm."]]$ed_pct_black_chg)
wi_pctblackchg <- lm_robust(ed_pct_black_chg ~ 
                             # vars of interest
                             sei1 +
                             # control vars
                             age1 + age_sq1 + woman + married + not_hhh1 + hh_n_adults1 + hh_n_kids1,
                           cluster = ed1,
                           data = data_list[["White Imm."]])

summary(wi_pctblackchg)
print(wi_pctblackchg)
tidy(wi_pctblackchg)


#### White Native Modelling ####

# specify a full model for ED pct black (not change)
# SE clustered on T1 ED 
wn_pctblack <- lm_robust(ed_pct_black2 ~ 
                           # vars of interest
                           ed_pct_black1 + sei1 + #ed_pct_black1*sei1 +
                           # control vars
                           age1 + age_sq1 + woman + married + not_hhh1 + hh_n_adults1 + hh_n_kids1 + hh_max_sei1 + factor(year1),
                         cluster = ed1,
                         data = data_list[["White Nat."]])

summary(wn_pctblack)
print(wn_pctblack)
tidy(wn_pctblack)


# Same model but predicing change from T1
summary(data_list[["White Nat."]]$ed_pct_black_chg)
wn_pctblackchg <- lm_robust(ed_pct_black_chg ~ 
                              # vars of interest
                              sei1 +
                              # control vars
                              age1 + age_sq1 + woman + married + not_hhh1 + hh_n_adults1 + hh_n_kids1,
                            cluster = ed1,
                            data = data_list[["White Nat."]])

summary(wn_pctblackchg)
print(wn_pctblackchg)
tidy(wn_pctblackchg)



### Modeling change in mean SEI
# black
summary(data_list[["Black"]]$ed_mean_sei_chg)
b_seichg <- lm_robust(ed_mean_sei_chg ~ 
                             # vars of interest
                             sei1 +
                             # control vars
                             age1 + age_sq1 + woman + married + not_hhh1 + hh_n_adults1 + hh_n_kids1,
                           cluster = ed1,
                           data = data_list[["Black"]])

summary(b_seichg)

# white immigrants
summary(data_list[["White Imm."]]$ed_mean_sei_chg)
wi_seichg <- lm_robust(ed_mean_sei_chg ~ 
                              # vars of interest
                              sei1 +
                              # control vars
                              age1 + age_sq1 + woman + married + not_hhh1 + hh_n_adults1 + hh_n_kids1,
                            cluster = ed1,
                            data = data_list[["White Imm."]])

summary(wi_seichg)

#white natives
summary(data_list[["White Nat."]]$ed_mean_sei_chg)
wn_seichg <- lm_robust(ed_mean_sei_chg ~ 
                              # vars of interest
                              sei1 +
                              # control vars
                              age1 + age_sq1 + woman + married + not_hhh1 + hh_n_adults1 + hh_n_kids1,
                            cluster = ed1,
                            data = data_list[["White Nat."]])

summary(wn_seichg)

