library(here)
library(sf)
library(dplyr)
library(rio)
library(ggplot2)
library(purrr)
library(ghibli)

source(here("R", "functions", "load_linked_sample.R"))
source(here("R", "functions", "aggregate_microdata.R"))

# load full philadelphia data and join ED classes

#ed10 <- st_read(here("data", "merged_eds", "class", "Phl1910.shp")) %>% mutate(ed = as.character(ED))
#ed20 <- st_read(here("data", "merged_eds", "class", "Phl1920.shp")) %>% mutate(ed = as.character(ED))
#ed30 <- st_read(here("data", "merged_eds", "class", "Phl1930.shp")) %>% mutate(ed = as.character(ED))
#ed40 <- st_read(here("data", "merged_eds", "class", "Phl1940.shp")) %>% mutate(ed = as.character(ED))

m10 <- load_microdata(10, formatted = F) %>% filter(sei > 0)
m20 <- load_microdata(20, formatted = F) %>% filter(sei > 0)
m30 <- load_microdata(30, formatted = F) %>% filter(sei > 0)
m40 <- load_microdata(40, formatted = F) %>% filter(sei > 0)

ed_classes <- import(here("data", "ed_data", "custom_classes.csv"))

m10 <- left_join(m10, ed_classes)
m20 <- left_join(m20, ed_classes)
m30 <- left_join(m30, ed_classes)
m40 <- left_join(m40, ed_classes)

# combine individual data and select important variables
list(m10, m20, m30, m40) %>% 
  map(filter, race_grp != "Other") %>% 
  map(mutate, race_cat = case_when(
    race_grp == "Black" ~ "Black", 
    race_grp == "White" & bpl < 15000 ~ "White NB", 
    race_grp == "White" & bpl >= 15000 ~ "White Imm"
  )) %>% 
  map(select, custom, race_cat, sei, occscore, occ1950, erscor50, year) %>% 
  bind_rows() %>% 
  filter(is.na(custom) == F) -> sei_data

# create density plots for each race group
filter(sei_data, race_cat == "Black") %>% 
  ggplot() +
  geom_density(aes(x = sei, col = factor(custom))) +
  geom_vline(
    data = summarise(group_by(filter(sei_data, race_cat == "Black"), custom, year), sei = mean(sei)),
    aes(xintercept = sei, col = custom),
    lty = 5, lwd = 1
  ) +
  #xlim(0, 30) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(. ~ year, nrow = 2) +
  theme_bw()


filter(sei_data, race_cat == "White Imm") %>% 
  ggplot() +
  geom_density(aes(x = sei, col = factor(custom))) +
  geom_vline(
    data = summarise(group_by(filter(sei_data, race_cat == "White Imm"), custom, year), sei = mean(sei)),
    aes(xintercept = sei, col = custom), 
    lty = 5
  ) +
  #xlim(0, 30) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(. ~ year, nrow = 2) +
  theme_bw()


filter(sei_data, race_cat == "White NB") %>% 
  ggplot() +
  geom_density(aes(x = sei, col = factor(year))) +
  geom_vline(
    data = summarise(group_by(filter(sei_data, race_cat == "White NB"), custom, year), sei = mean(sei)),
    aes(xintercept = sei, col = factor(year)), 
    lty = 5
  ) +
  #xlim(0, 30) +
  #scale_color_brewer(palette = "Blues") +
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  facet_wrap(. ~ custom, ncol = 1) +
  theme_bw()


# these density plots aren't really working, let's just focus on means
sei_data %>% 
  group_by(custom, race_cat, year) %>% 
  summarise(sei = mean(sei), occ = mean(occscore), er = mean(erscor50)) -> sei_means

sei_means %>% 
  ggplot() +
  geom_line(aes(x = year, y = sei, col = custom)) +
  facet_wrap(. ~ race_cat)

# Now let's take a look at the most common occupations for each group in each neighborhood class
sei_data %>% 
  group_by(custom, race_cat, year, occ1950) %>%
  summarise(N = n()) %>% 
  arrange(custom, race_cat, year, desc(N)) %>% 
  top_n(3, N) -> top_occs

# actually, let's see what the top 10 overall occupational codes are in each year
sei_data %>% 
  group_by(year, occ1950) %>%
  summarise(N = n()) %>% 
  arrange(year, desc(N)) %>% 
  top_n(10, N) -> top_occs

#### do this analysis according to raw numbers, but also according to expected numbers
# Does one or two occuptions over-represent a racial group in a neighborhood based on its
# overall representation for that racial group in the city
# max(class_group_prop / group_prop)

sei_data %>% 
  group_by(race_cat, year) %>%
  mutate(race_n = n()) %>% 
  group_by(race_cat, year, occ1950) %>%
  mutate(race_occ_n = n(), race_occ_pct = race_occ_n / race_n) %>% 
  group_by(custom, race_cat, year) %>%
  mutate(nhood_race_n = n()) %>% 
  group_by(custom, race_cat, year, occ1950) %>%
  mutate(nhood_race_occ_n = n(), nhood_race_occ_pct = nhood_race_occ_n / nhood_race_n) %>%
  select(custom, race_cat, year, occ1950, race_occ_n, race_occ_pct, nhood_race_occ_pct) %>% 
  unique() %>% 
  mutate(nhood_ratio = nhood_race_occ_pct / race_occ_pct) -> race_occ_data






##### Describing Other Neighborhood Attributes

# load and aggregate data to ED level
ed_data <- c(10, 20, 30, 40) %>% 
  map(aggregate_microdata, ed) %>% 
  map(mutate, ed = as.character(ed)) %>% 
  bind_rows()
names(ed_data)[-(1:2)] <- paste0("ed_", names(ed_data)[-(1:2)])

# add neighborhood classes
ed_data$custom <- case_when(
  ed_data$ed_pct_black >= 25 ~ "Black",
  ed_data$ed_pct_black < 25 & ed_data$ed_pct_black >= 5 & ed_data$ed_pct_frnbrn >= 25 ~ "Mixed - Imm.",
  ed_data$ed_pct_black < 25 & ed_data$ed_pct_black >= 5 & ed_data$ed_pct_frnbrn < 25 ~ "Mixed - U.S.",
  ed_data$ed_pct_black < 5 & ed_data$ed_pct_frnbrn > 25 ~ "Immigrant",
  ed_data$ed_pct_black < 5 & ed_data$ed_pct_frnbrn < 25 ~ "U.S. White"
)
ed_data$custom <- factor(ed_data$custom, levels = c("Black", "Mixed - Imm.","Mixed - U.S.", "Immigrant", "U.S. White"))


# density plot of mean SEI of neighborhoods by class and year
filter(ed_data, is.na(custom) == F) %>% 
  ggplot() +
  geom_violin(aes(x = factor(year), y = ed_mean_sei, col = custom),
              draw_quantiles = c(0.5),
              fill = "#ebebeb") +
  scale_color_brewer(palette = "Set1") +
  #ylim(0, 30) +
  facet_wrap(. ~ custom, nrow = 1) +
  labs(x = "", y = "Mean SEI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30))


# density plot of mean age of neighborhoods by class and year
filter(ed_data, is.na(custom) == F) %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(year), y = ed_mean_age, col = custom),
              #draw_quantiles = c(0.5),
              fill = "#ebebeb") +
  scale_color_brewer(palette = "Set1") +
  ylim(20, 40) +
  facet_wrap(. ~ custom, nrow = 1) +
  labs(x = "", y = "Mean Age") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30))


# density plot of % under age 15 of neighborhoods by class and year
filter(ed_data, is.na(custom) == F) %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(year), y = ed_pct_under_15, col = custom),
              #draw_quantiles = c(0.5),
              fill = "#ebebeb") +
  scale_color_brewer(palette = "Set1") +
  #ylim(20, 40) +
  facet_wrap(. ~ custom, nrow = 1) +
  labs(x = "", y = "% age 15 and under") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30))


# density plot of % over age 64 of neighborhoods by class and year
filter(ed_data, is.na(custom) == F) %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(year), y = ed_pct_over_64, col = custom),
              #draw_quantiles = c(0.5),
              fill = "#ebebeb") +
  scale_color_brewer(palette = "Set1") +
  ylim(0, 20) +
  facet_wrap(. ~ custom, nrow = 1) +
  labs(x = "", y = "% over age 64") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30))



