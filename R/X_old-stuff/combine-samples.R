### let's look at how much the Philly linked samples over lap!
# Can I construct longitudinal samples across all four censuses?

library(dplyr)
library(ggplot2)
library(rio)
library(here)

# load all years
# need to select and rename only those vars for matching
# shift to years, not time points in two-decade link
linked_10_20 <- import(here("data", "linked", "linked_10_20.csv")) %>% 
  select(serial1, serial2) %>% 
  rename(serial10 = serial1, serial20 = serial2)

linked_20_30 <- import(here("data", "linked", "linked_20_30.csv")) %>% 
  select(serial1, serial2) %>% 
  rename(serial20 = serial1, serial30 = serial2)

linked_30_40 <- import(here("data", "linked", "linked_30_40.csv")) %>% 
  select(serial1, serial2) %>% 
  rename(serial30 = serial1, serial40 = serial2)

#let's try to link the first three censuses
c_10_20_30 <- inner_join(linked_10_20, linked_20_30)

#last three
c_20_30_40 <- inner_join(linked_20_30, linked_30_40)

#let's shoot for all 4!
full_combo <- inner_join(c_10_20_30, linked_30_40)


