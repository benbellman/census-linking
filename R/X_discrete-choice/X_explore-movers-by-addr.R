library(rio)
library(here)
library(dplyr)
library(ggplot2)

# set years for time points
t1 <- 20
t2 <- t1 + 10

# load linked data
linked <- import(here("data", "linked", paste0("linked_", t1, "_", t2, ".csv"))) %>% 
  as_tibble() %>% 
  mutate(#ed1 = as.character(ed1),
         #ed2 = as.character(ed2),
         black = if_else(race_grp1 == "Black" | race_grp2 == "Black", 1, 0),
         white = if_else(race_grp1 == "White" & race_grp2 == "White", 1, 0),
         frnbrn = if_else(nativity1 == 5 | nativity2 == 5, 1, 0),
         race_cat = case_when(
           black == 1 ~ "Black",
           white == 1 & frnbrn == 0 ~ "White NB",
           white == 1 & frnbrn == 1 ~ "White Imm",
           TRUE ~ "Other"
         ))

# load in the distances between 1920 and 1930 
ed_dist <- import(here("data", "ed_distances_20_30.csv")) %>% 
  rename(ed2 = dest_ed)

# merge distances (in meters) to linked data
linked <- left_join(linked, ed_dist)

# these are the address, ed, race columns
addr <- select(linked, hn1, overall_match1, hn2, overall_match2, ed1, ed2, dist, race_cat) %>% 
  # add dummy vars for each address info match across census decades
  mutate(hn_same = if_else(hn1 == hn2, 1, 0),
         st_same = if_else(overall_match1 == overall_match2, 1, 0),
         both_same = if_else(hn_same == 1 & st_same == 1, 1, 0))

# tabulate the results
table(addr$hn_same)
table(addr$st_same)
table(addr$both_same)

# visualize ED distances based on identical 
ggplot(addr) +
  geom_boxplot(aes(both_same, dist))

# Ok, I'm convinced that comparing exact addresses is the best way to measure having moved

# lets see what proportion of each racial group had the same address
table(addr$both_same, addr$race_cat)
# 87% of black householders linked sample moved between 1920 and 1930
# 77% of white immigrant householders linked sample moved between 1920 and 1930
# 74% of white US-born householders linked sample moved between 1920 and 1930

# let's see how much of a problem missing data is
table(is.na(addr$both_same), addr$race_cat)
# 1% of black householders linked sample have missing address data
# 1.5% of white immigrant householders linked sample have missing address data
# 1.5% of white US-born householders linked sample have missing address data



