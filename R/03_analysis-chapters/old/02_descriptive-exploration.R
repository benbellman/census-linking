library(here)
library(rio)
library(dplyr)
library(purrr)
library(data.table)
library(ggplot2)
library(stringr)
library(ghibli)
library(ggalluvial)

#source(here("R", "functions", "load_linked_sample.R"))

#### Part 1: Overall Population Questions ####

# set up variables to keep
#ind_cols <- c("uniqueid", "pernum", "year", "serial", "relate", 
#              "ed", "hn", "overall_match", "street_precleaned",
#              "age", "sex", "race", "race_grp", "nativity", "marst", 
#              "nchild", "bpl", "sei", "occ1950", "ownershp")

# load full data, strip needed variables, and combine
#m_full <- list(
#  load_microdata(10, formatted = F) %>% select(ind_cols),
#  load_microdata(20, formatted = F) %>% select(ind_cols),
#  load_microdata(30, formatted = F) %>% select(ind_cols)
#) %>% bind_rows()



#### Part 2: Linked Sample Questions ####

# import linked data for analysis and convert vars to factors with levels
linked <- import(here("data", "analysis", "links-full-info.csv")) %>% 
  mutate(
    move_type = factor(move_type, levels = c("Same Address", "Same Neighborhood", "Left Neighborhood", "Missing Data")),
    home_type1 = factor(home_type1, levels = c("Owner", "Renter", "Not Head", "Missing Data"))
  )

# table looking at move types by year according to race
linked %>% 
  group_by(race_cat, year1) %>% 
  mutate(grp_yr_total = n()) %>% 
  group_by(race_cat, year1, move_type) %>% 
  mutate(Pct = round(n() / grp_yr_total * 100, 1)) %>% 
  select(race_cat, year1, move_type, Pct) %>% 
  unique() %>% 
  filter(move_type != "Missing Data" & race_cat != "Other") %>% 
  dcast(race_cat + year1 ~ move_type)

# same table, but as a stacked bar chart
linked %>% 
  group_by(race_cat, year1) %>% 
  mutate(grp_yr_total = n()) %>% 
  group_by(race_cat, year1, move_type) %>% 
  mutate(Pct = round(n() / grp_yr_total * 100, 1)) %>% 
  select(race_cat, year1, move_type, Pct) %>% 
  unique() %>% 
  filter(race_cat != "Other") %>% 
  ggplot() +
  geom_col(aes(x = year1, y = Pct, fill = move_type)) +
  facet_grid(. ~ race_cat)



# table looking at move types by year according to race AND homeownership in time 1
linked %>% 
  group_by(race_cat, year1, home_type1) %>% 
  mutate(grp_yr_total = n()) %>% 
  group_by(race_cat, year1, home_type1, move_type) %>% 
  mutate(Pct = round(n() / grp_yr_total * 100, 1)) %>% 
  select(race_cat, year1, home_type1, move_type, Pct) %>% 
  unique() %>% 
  filter(move_type != "Missing Data" & home_type1 != "Missing Data" & race_cat != "Other") %>% 
  dcast(race_cat + year1 ~ home_type1 + move_type)


# same info, but as stacked bar graphs
linked %>% 
  group_by(race_cat, decade, home_type1) %>% 
  mutate(grp_yr_total = n()) %>% 
  group_by(race_cat, decade, home_type1, move_type) %>% 
  mutate(Pct = round(n() / grp_yr_total * 100, 1),
         y_pos = case_when(
           move_type == "Same Address" ~ 100 - (Pct * 0.5),
           move_type == "Same Neighborhood" & home_type1 == "Owner" & decade == "1910-20" ~ 50,
           move_type == "Same Neighborhood" & home_type1 == "Owner" & decade == "1920-30" ~ 45,
           move_type == "Same Neighborhood" & home_type1 == "Owner" & decade == "1930-40" ~ 40,
           move_type == "Same Neighborhood" & home_type1 == "Renter" ~ 70,
           move_type == "Same Neighborhood" & home_type1 == "Not Head" & decade != "1930-40" ~ 75,
           move_type == "Same Neighborhood" & home_type1 == "Not Head" & decade == "1930-40" ~ 68,
           move_type == "Left Neighborhood" ~ Pct * 0.6
         )) %>% 
  select(race_cat, decade, home_type1, move_type, Pct, y_pos) %>% 
  unique() %>% 
  filter(home_type1 != "Missing Data" & race_cat != "Other") %>% 
  ggplot() +
  geom_col(aes(x = decade, y = Pct, fill = move_type)) +
  geom_text(aes(x = decade, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("LaputaMedium", direction = -1)) +
  facet_grid(home_type1 ~ race_cat) +
  labs(x = "Decade", y = "Cumulative Percent", fill = "Type of Move") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "movetype_by_race_and_year_hometype.pdf"), height = 5, width = 7)


# look at distribution of home-type for linked samples across each year
linked %>% 
  filter(race_cat != "Other") %>% 
  group_by(decade, race_cat) %>% 
  mutate(n_grp = n()) %>% 
  group_by(decade, race_cat, home_type1) %>% 
  mutate(n_home = n(), 
         Pct = round(n_home / n_grp * 100, 1),
         y_pos = case_when(
           home_type1 == "Owner" ~ 100 - (Pct * 0.5),
           race_cat == "Black" & home_type1 == "Renter" ~ 62,
           race_cat == "Black" & home_type1 == "Not Head" ~ 20,
           race_cat == "White Imm" & home_type1 == "Renter" & decade == "1910-20" ~ 50,
           race_cat == "White Imm" & home_type1 == "Renter" & decade == "1920-30" ~ 38,
           race_cat == "White Imm" & home_type1 == "Renter" & decade == "1930-40" ~ 30,
           race_cat == "White Imm" & home_type1 == "Not Head" ~ 10,
           race_cat == "White NB" & home_type1 == "Renter" & decade == "1910-20" ~ 50,
           race_cat == "White NB" & home_type1 == "Renter" & decade == "1920-30" ~ 45,
           race_cat == "White NB" & home_type1 == "Renter" & decade == "1930-40" ~ 40,
           race_cat == "White NB" & home_type1 == "Not Head" ~ 14
         )) %>% 
  select(decade, year1, race_cat, home_type1, n_grp, n_home, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = year1, y = Pct, fill = home_type1)) +
  geom_text(aes(x = year1, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("YesterdayLight", direction = -1)) +
  facet_grid(. ~ race_cat) +
  labs(x = "Year", y = "Cumulative Percent", fill = "Housing Tenure") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggsave(here("figures", "new_chp2", "hometype_by_race_and_year.pdf"), height = 3, width = 6)


# look at distribution of move type for linked samples across each year
linked %>% 
  filter(race_cat != "Other") %>% 
  group_by(decade, race_cat) %>% 
  mutate(n_grp = n()) %>% 
  group_by(decade, race_cat, move_type) %>% 
  mutate(n_move = n(), 
         Pct = round(n_move / n_grp * 100, 1),
         y_pos = case_when(
           move_type == "Same Address" ~ 100 - (Pct * 0.5),
           move_type == "Left Neighborhood" ~ Pct * 0.5,
           move_type == "Same Neighborhood" & race_cat == "Black" ~ 70,
           move_type == "Same Neighborhood" & race_cat == "White Imm" & year1 == 1910 ~ 70,
           move_type == "Same Neighborhood" & race_cat == "White Imm" & year1 == 1920 ~ 65,
           move_type == "Same Neighborhood" & race_cat == "White Imm" & year1 == 1930 ~ 50,
           move_type == "Same Neighborhood" & race_cat == "White NB" & year1 == 1910 ~ 70,
           move_type == "Same Neighborhood" & race_cat == "White NB" & year1 == 1920 ~ 65,
           move_type == "Same Neighborhood" & race_cat == "White NB" & year1 == 1930 ~ 55
         )) %>% 
  select(decade, year1, race_cat, move_type, n_grp, n_move, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = decade, y = Pct, fill = move_type)) +
  geom_text(aes(x = decade, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("LaputaMedium", direction = -1)) +
  facet_grid(. ~ race_cat) +
  labs(x = "Decade", y = "Cumulative Percent", fill = "Move Type") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "movetype_by_race_and_year.pdf"), height = 3, width = 6)

# Look at distributions of SEI of eventual heads across homeownership and years (violin? denisty?)
# density style
sei_means <- filter(linked, race_cat != "Other" & home_type1 != "Missing Data") %>% 
  group_by(race_cat, year1, home_type1) %>% 
  summarise(sei = round(mean(sei1), 1)) %>%
  mutate(y_pos = case_when(
    home_type1 == "Owner" ~ 0.175,
    home_type1 == "Renter" ~ 0.125,
    home_type1 == "Not Head" ~ 0.075
  ))

linked %>% 
  filter(race_cat != "Other" & home_type1 != "Missing Data") %>% 
  ggplot() +
  geom_density(aes(x = sei1, fill = home_type1, col = home_type1), alpha = 0.3) +
  geom_vline(data = sei_means,
             aes(xintercept = sei, col = home_type1)) +
  geom_label(data = sei_means,
            aes(x = 70, y = y_pos, label = paste0("Mean = ", sei), fill = home_type1),
            size = 3, alpha = 0.5) +
  scale_fill_manual(values = ghibli_palette("YesterdayMedium", direction = -1)) +
  scale_color_manual(values = ghibli_palette("YesterdayMedium", direction = -1)) +
  facet_grid(year1 ~ race_cat) +
  labs(x = "SEI", y = "Density", col = "Housing\nTenure", fill = "Housing\nTenure") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggsave(here("figures", "new_chp2", "sei_by_race_and_year_and_hometype.pdf"), height = 5, width = 7)




# Do table(s) of home_type1 and custom1, examine by race and year
# color palette for ED categories
custom_pal <- c(ghibli_palette("KikiLight")[4], ghibli_palette("KikiLight")[3], "lightgrey")

linked %>% 
  filter(race_cat != "Other" & home_type1 != "Missing Data" & is.na(ed_custom1) == F) %>% 
  group_by(year1, race_cat, home_type1) %>% 
  mutate(n_home = n()) %>% 
  group_by(year1, race_cat, home_type1, ed_custom1) %>% 
  mutate(n_cust = n(), 
         Pct = round(n_cust / n_home * 100, 1),
         y_pos = case_when(
           ed_custom1 == "Black" ~ 100 - (Pct * 0.5),
           race_cat == "Black" & ed_custom1 == "U.S. White" ~ (Pct * 0.5),
           race_cat == "White Imm" & ed_custom1 == "Immigrant" ~ 70,
           race_cat == "White NB" & ed_custom1 == "Immigrant" ~ 85,
           race_cat == "White Imm" & ed_custom1 == "U.S. White" ~ 20,
           race_cat == "White NB" & ed_custom1 == "U.S. White" ~ 40
         )) %>% 
  select(year1, race_cat, home_type1, ed_custom1, n_home, n_cust, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = year1, y = Pct, fill = ed_custom1)) +
  geom_text(aes(x = year1, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = custom_pal) +
  facet_grid(home_type1 ~ race_cat) +
  labs(x = "Year", y = "Cumulative Percent", fill = "Neighborhood Type") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggsave(here("figures", "new_chp2", "edclass_by_year_and_race_and_hometype.pdf"), height = 5, width = 7)
  

# what were rates of move types based on origin ED class, by race and year?
linked %>% 
  filter(race_cat != "Other" & move_type != "Missing Data" & is.na(ed_custom1) == F) %>% 
  group_by(year1, race_cat, ed_custom1) %>% 
  mutate(n_cust = n()) %>% 
  group_by(year1, race_cat, ed_custom1, move_type) %>% 
  mutate(n_move = n(), 
         Pct = round(n_move / n_cust * 100, 1),
         y_pos = case_when(
           move_type == "Left Neighborhood" ~ Pct * 0.5,
           move_type == "Same Address" ~ 100 - (Pct * 0.5),
           race_cat == "Black" & ed_custom1 == "Black" ~ 65,
           race_cat == "Black" & ed_custom1 == "Immigrant" ~ 80,
           race_cat == "Black" & ed_custom1 == "U.S. White" ~ 75,
           race_cat == "White Imm" & year1 %in% c(1910, 1920) ~ 65,
           race_cat == "White Imm" & year1 == 1930 ~ 50,
           race_cat == "White NB" ~ 60
         )) %>% 
  select(decade, race_cat, ed_custom1, move_type, n_cust, n_move, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = decade, y = Pct, fill = move_type)) +
  geom_text(aes(x = decade, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("LaputaMedium", direction = -1)) +
  facet_grid(ed_custom1 ~ race_cat) +
  labs(x = "Decade", y = "Cumulative Percent", fill = "Type of Move") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "movetype_by_year_and_race_and_edclass1.pdf"), width = 6, height = 5)


# let's look at rates of move types for only whites (combined) based on both housing and neighborhood type
linked %>% 
  filter(race_cat %in% c("White NB", "White Imm") & home_type1 != "Missing Data" & move_type != "Missing Data" & is.na(ed_custom1) == F) %>% 
  group_by(year1, home_type1, ed_custom1) %>% 
  mutate(n_grp = n()) %>% 
  group_by(year1, home_type1, ed_custom1, move_type) %>% 
  mutate(n_move = n(), 
         Pct = round(n_move / n_grp * 100, 1),
         y_pos = case_when(
           move_type == "Left Neighborhood" ~ Pct * 0.5,
           move_type == "Same Address" ~ 100 - (Pct * 0.5),
           home_type1 == "Owner" & year1 %in% c(1910, 1920) ~ 45,
           home_type1 == "Owner" & year1 == 1930 ~ 35,
           home_type1 == "Renter" ~ 70,
           home_type1 == "Not Head" ~ 70
         )) %>% 
  select(decade, home_type1, ed_custom1, move_type, n_grp, n_move, Pct, y_pos) %>% 
  unique() %>% 
  ggplot() +
  geom_col(aes(x = decade, y = Pct, fill = move_type)) +
  geom_text(aes(x = decade, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
  scale_fill_manual(values = ghibli_palette("LaputaMedium", direction = -1)) +
  facet_grid(ed_custom1 ~ home_type1) +
  labs(x = "Decade", y = "Cumulative Percent", fill = "Type of Move") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "white_movetype_by_year_and_hometype_and_edclass1.pdf"), width = 6, height = 5)



# what were rates of move types based on destination ED class, by race and year?
#linked %>% 
#  filter(race_cat != "Other" & move_type != "Missing Data" & is.na(ed_custom2) == F) %>% 
#  group_by(year1, race_cat, ed_custom2) %>% 
#  mutate(n_cust = n()) %>% 
#  group_by(year1, race_cat, ed_custom2, move_type) %>% 
#  mutate(n_move = n(), 
#         Pct = round(n_move / n_cust * 100, 1),
#         y_pos = case_when(
#           move_type == "Left Neighborhood" ~ Pct * 0.5
#         )) %>% 
#  select(year1, race_cat, ed_custom2, move_type, n_cust, n_move, Pct, y_pos) %>% 
#  unique() %>% 
#  ggplot() +
#  geom_col(aes(x = year1, y = Pct, fill = move_type)) +
#  geom_text(aes(x = year1, y = y_pos, label = paste0(str_remove(as.character(Pct), "\\.[0-9]$"), "%")), size = 3) +
#  scale_fill_manual(values = ghibli_palette("YesterdayLight", direction = -1)) +
#  facet_grid(ed_custom2 ~ race_cat) +
#  labs(x = "Year", y = "Cumulative Percent", fill = "Type of Move") +
#  theme_minimal() +
#  ggsave(here("figures", "new_chp2", "movetype_by_year_and_race_and_edclass2.pdf"), width = 6, height = 5)


## time for alluvial diagrams!
# create necessary data structure
alluv <- linked %>% 
  filter(race_cat != "Other" & (is.na(ed_custom1) == F) & (is.na(ed_custom2) == F) & move_type != "Missing Data") %>% 
  group_by(move_type, ed_custom1, ed_custom2, race_cat, decade) %>% 
  summarize(flow_n = n()) %>% 
  group_by(move_type, race_cat, decade) %>% 
  mutate(flow_pct = flow_n / sum(flow_n) * 100)

# black
filter(alluv, race_cat == "Black" & move_type == "Left Neighborhood") %>% 
  group_by(decade) %>% 
  mutate(total = format(sum(flow_n), big.mark = ",")) %>% 
  ggplot(aes(y = flow_pct, axis1 = ed_custom1, axis2 = ed_custom2)) +
  geom_flow(aes(fill = ed_custom2), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", label.strata = TRUE, size = 2) +
  geom_text(aes(x = 1.5, y = 105, label = paste0("N = ", total)), size = 3) +
  scale_fill_manual(values = c(custom_pal[1:2], "grey")) +
  scale_x_discrete(limits = c("Origin", "Destination")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  facet_grid(. ~ decade) +
  labs(x = "",
       y = "Percent of household heads",
       fill = "Destination type") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggsave(here("figures", "new_chp2", "alluv_black_leftn.pdf"))

# immigrant
filter(alluv, race_cat == "White Imm" & move_type == "Left Neighborhood") %>% 
  group_by(decade) %>% 
  mutate(total = format(sum(flow_n), big.mark = ",")) %>% 
  ggplot(aes(y = flow_pct, axis1 = ed_custom1, axis2 = ed_custom2)) +
  geom_flow(aes(fill = ed_custom2), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", label.strata = TRUE, size = 2) +
  geom_text(aes(x = 1.5, y = 105, label = paste0("N = ", total)), size = 3) +
  scale_fill_manual(values = c(custom_pal[1:2], "grey")) +
  scale_x_discrete(limits = c("Origin", "Destination")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  facet_grid(. ~ decade) +
  labs(x = "",
       y = "Percent of householders",
       fill = "Destination type") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggsave(here("figures", "new_chp2", "alluv_wimm_leftn.pdf"))


# white nb
filter(alluv, race_cat == "White NB" & move_type == "Left Neighborhood") %>% 
  group_by(decade) %>% 
  mutate(total = format(sum(flow_n), big.mark = ",")) %>% 
  ggplot(aes(y = flow_pct, axis1 = ed_custom1, axis2 = ed_custom2)) +
  geom_flow(aes(fill = ed_custom2), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", label.strata = TRUE, size = 2) +
  geom_text(aes(x = 1.5, y = 105, label = paste0("N = ", total)), size = 3) +
  scale_fill_manual(values = c(custom_pal[1:2], "grey")) +
  scale_x_discrete(limits = c("Origin", "Destination")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  facet_grid(. ~ decade) +
  labs(x = "",
       y = "Percent of householders",
       fill = "Destination type") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggsave(here("figures", "new_chp2", "alluv_wnb_leftn.pdf"))


# plot all groups together
# white nb
filter(alluv, race_cat != "Other" & move_type == "Left Neighborhood") %>% 
  group_by(decade, race_cat) %>% 
  mutate(total = format(sum(flow_n), big.mark = ",")) %>% 
  ggplot(aes(y = flow_pct, axis1 = ed_custom1, axis2 = ed_custom2)) +
  geom_flow(aes(fill = ed_custom2), width = .4) +
  geom_stratum(width = .4) +
  geom_text(stat = "stratum", label.strata = TRUE, size = 4) +
  geom_text(aes(x = 1.5, y = 105, label = paste0("N = ", total)), size = 4) +
  scale_fill_manual(values = c(custom_pal[1:2], "grey")) +
  scale_x_discrete(limits = c("Origin", "Destination")) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  facet_grid(race_cat ~ decade) +
  labs(x = "",
       y = "Percent of householders",
       fill = "Destination type") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none") +
  ggsave(here("figures", "new_chp2", "alluv_all_leftn.pdf"), height = 11, width = 9)


### Let's look at changes in segregation over time
# results are suggesting that things may actually have been integrating at the ED level
# white out-flows are not as strong as I would have expected
# but any decreases may not be happening at street-level scales
source(here("R", "functions", "load_linked_sample.R"))
source(here("R", "functions", "segregation_indices.R"))

m10 <- load_microdata(10, formatted = F) %>% 
  filter(relate == 101) %>% 
  select(year, race, bpl, ed, overall_match, overall_match_bool, street_precleaned) %>% 
  mutate(
    race_cat = case_when(
      race == 100 & bpl < 15000 ~ "White NB",
      race == 100 & bpl >= 15000 ~ "White Imm",
      race %in% c(200, 210) ~ "Black",
      TRUE ~ "Other"
    ),
    street = if_else(overall_match_bool == T, overall_match, street_precleaned),
    ed_street = paste0(ed, " - ", street)
  )

m20 <- load_microdata(20, formatted = F) %>% 
  filter(relate == 101) %>% 
  select(year, race, bpl, ed, overall_match, overall_match_bool, street_precleaned) %>% 
  mutate(
    race_cat = case_when(
      race == 100 & bpl < 15000 ~ "White NB",
      race == 100 & bpl >= 15000 ~ "White Imm",
      race %in% c(200, 210) ~ "Black",
      TRUE ~ "Other"
    ),
    street = if_else(overall_match_bool == T, overall_match, street_precleaned),
    ed_street = paste0(ed, " - ", street)
  )

m30 <- load_microdata(30, formatted = F) %>% 
  filter(relate == 101) %>% 
  select(year, race, bpl, ed, overall_match, overall_match_bool, street_precleaned) %>% 
  mutate(
    race_cat = case_when(
      race == 100 & bpl < 15000 ~ "White NB",
      race == 100 & bpl >= 15000 ~ "White Imm",
      race %in% c(200, 210) ~ "Black",
      TRUE ~ "Other"
    ),
    street = if_else(overall_match_bool == T, overall_match, street_precleaned),
    ed_street = paste0(ed, " - ", street)
  )

m40 <- load_microdata(40, formatted = F) %>% 
  filter(relate == 101) %>% 
  select(year, race, bpl, ed, overall_match, overall_match_bool, street_precleaned) %>% 
  mutate(
    race_cat = case_when(
      race == 100 & bpl < 15000 ~ "White NB",
      race == 100 & bpl >= 15000 ~ "White Imm",
      race %in% c(200, 210) ~ "Black",
      TRUE ~ "Other"
    ),
    street = if_else(overall_match_bool == T, overall_match, street_precleaned),
    ed_street = paste0(ed, " - ", street)
  )

# write a function that computes all desired indices for data from a single year
calc_all_index <- function(rows){
  bind_rows(
    tibble(index = "D wb", unit = "ED", value = diss(rows, ed, race_cat, "White NB", "Black")),
    tibble(index = "D wb", unit = "ED-street", value = diss(rows, ed_street, race_cat, "White NB", "Black")),
    tibble(index = "D wi", unit = "ED", value = diss(rows, ed, race_cat, "White NB", "White Imm")),
    tibble(index = "D wi", unit = "ED-street", value = diss(rows, ed_street, race_cat, "White NB", "White Imm")),
    tibble(index = "D bi", unit = "ED", value = diss(rows, ed, race_cat, "Black", "White Imm")),
    tibble(index = "D bi", unit = "ED-street", value = diss(rows, ed_street, race_cat, "Black", "White Imm")),
    tibble(index = "p* ww", unit = "ED", value = pstar(rows, ed, race_cat, "White NB", "White NB")),
    tibble(index = "p* ww", unit = "ED-street", value = pstar(rows, ed_street, race_cat, "White NB", "White NB")),
    tibble(index = "p* ii", unit = "ED", value = pstar(rows, ed, race_cat, "White Imm", "White Imm")),
    tibble(index = "p* ii", unit = "ED-street", value = pstar(rows, ed_street, race_cat, "White Imm", "White Imm")),
    tibble(index = "p* bb", unit = "ED", value = pstar(rows, ed, race_cat, "Black", "Black")),
    tibble(index = "p* bb", unit = "ED-street", value = pstar(rows, ed_street, race_cat, "Black", "Black"))
  )
}

# group by year and apply function for all indices
indices <- list(
  calc_all_index(m10) %>% mutate(year = 1910),
  calc_all_index(m20) %>% mutate(year = 1920),
  calc_all_index(m30) %>% mutate(year = 1930),
  calc_all_index(m40) %>% mutate(year = 1940)
) %>% bind_rows()

# plot them
ggplot(indices) +
  geom_path(aes(x = year, y = value, col = index), size = 1.5) +
  facet_grid(. ~ unit) +
  labs(x = "Year", y = "Value", col = "Index") +
  #scale_color_manual(values = c(ghibli_palette("PonyoMedium")[5:7], 
  #                              ghibli_palette("LaputaMedium")[3:5])) +
  scale_color_manual(values = ghibli_palette("KikiMedium")[2:7]) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 25)) +
  ggsave(here("figures", "new_chp2", "segregation.pdf"))








