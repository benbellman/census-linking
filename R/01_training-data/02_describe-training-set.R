library(tidyverse)
library(here)

# load pre-preprocessed data
#full_data <- read_csv(here("data", "training_all_vars_v1.csv"))
training <- read.csv(here("data", "training_all_vars_v1.csv"), stringsAsFactors = F) %>% 
  as_tibble()

# load full philly files, and keep records of people in training households
hold <- list()
files <- c("Phl20.csv", "Phl30.csv", "Phl40.csv")
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

# tabulate match status with demographic variables
table(full$match, full$race)
table(full$match, full$sex)
table(full$match, full$race_sex)

ggplot(full) +
  stat_bin(aes(x = age, fill = factor(match)), 
           alpha = 0.5, geom = "bar", 
           position = "identity", binwidth = 5) +
  facet_grid(sex ~ race)






