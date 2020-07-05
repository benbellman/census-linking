library(tidyverse)
library(here)

source(here("R", "functions", "load_linked_sample.R"))

# load pre-preprocessed data
#full_data <- read_csv(here("data", "training_all_vars_v1.csv"))
training <- read.csv(here("data", "training_all_vars_v3.csv"), stringsAsFactors = F) %>% 
  as_tibble()

# load full philly files, and keep records of people in training households
hold <- list()
files <- c("micro/Phl20.csv", "micro/Phl30.csv", "micro/Phl40.csv")
for(a in 1:3){
  hold[[a]] <- here("data", files[a]) %>% 
    read.csv(stringsAsFactors = F) %>% 
    as_tibble() %>% 
    filter(serial %in% training$serial2)
  
  # drop un-harmonized variables
  hold[[a]]$histid <- NULL
  hold[[a]][,str_detect(names(hold[[a]]), "_")] <- NULL
}

full <- hold %>% 
  bind_rows() %>% 
  filter(relate == "Head/householder")

# attach match variable to full records
full <- training %>% 
  select(serial2, match) %>% 
  rename(serial = serial2) %>% 
  group_by(serial) %>% 
  mutate(match = max(match)) %>% 
  ungroup() %>% 
  unique() %>% 
  full_join(full)

# calculate new variable for race*sex
full <- full %>%
  mutate(race = str_replace(race, "Black/Negro", "Black"),
         race_sex = paste(race, sex, sep = "_"))

# recode the age variable
full$age <- recode(full$age, 
                    `90 (90+ in 1980 and 1990)` = "90",
                    `100 (100+ in 1960-1970)` = "100",
                    `112 (112+ in the 1980 internal data)` = "112",
                    `Less than 1 year old` = "0") %>% as.numeric()

# load random 30,000 sample from 1930 for comparison
samp_ids <- here("data", "linking_comparisons", "comparison_sample.csv") %>% 
  import() %>% 
  .$uniqueid2

samp <- load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), list(~ paste0(., "2"))) %>% 
  filter(uniqueid2 %in% samp_ids) %>% 
  mutate(method = "Original\nSample")

# calculate new variable for race
samp <- samp %>%
  mutate(
    race = case_when(
      race2 < 200 ~ "White",
      race2 == 200 ~ "Black",
      TRUE ~ "Other"
    )
  )






#######
# tabulate match status with race
table(full$race)
table(full$race) / 10.11

table(full$match, full$race)

table(samp$race)
table(samp$race) / 300


# look at age across groups
full %>% 
  group_by(race, match) %>% 
  summarise(mean_age = mean(age))


ggplot(full) +
  stat_bin(aes(x = age, fill = factor(match)), 
           alpha = 0.5, geom = "bar", 
           position = "identity", binwidth = 5) +
  facet_grid(sex ~ race)


race <- select(full, race, serial) %>% 
  rename(serial2 = serial)

full_test <- left_join(full_test, race)


