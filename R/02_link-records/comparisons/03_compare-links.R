library(rio)
library(here)
library(dplyr)
library(purrr)
library(ggplot2)

source(here("R", "functions", "load_linked_sample.R"))


#### comparing sample representation ####

f_05_175 <- here("data", "linking_comparisons", "crosswalks", "feigenbaum_crosswalk-0.5-1.75.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2")))) %>% 
  mutate(method = "Feigenbaum")

m_05_175 <- here("data", "linking_comparisons", "crosswalks", "mine_crosswalk-0.5-1.75.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2")))) %>% 
  mutate(method = "Bellman")

ferrie <- here("data", "linking_comparisons", "crosswalks", "ferrie_crosswalk.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2")))) %>% 
  mutate(method = "Ferrie")

samp_ids <- here("data", "linking_comparisons", "comparison_sample.csv") %>% 
  import() %>% 
  .$uniqueid2
  
samp <- load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2"))) %>% 
  filter(uniqueid2 %in% samp_ids) %>% 
  mutate(method = "Original Sample")

# combine attributes for plotting
plot_vars <- c("method", "sei2", "age2", "race_grp2", "bpl2", "erscor502", "edscor502", "marst2")

to_plot <- list(f_05_175, m_05_175, ferrie, samp) %>% 
  map(select, plot_vars) %>% 
  bind_rows() %>% 
  mutate(new_bpl = case_when(bpl2 == 4200 ~ "Pennsylvania",
                             bpl2 != 4200 & bpl2 < 15000 ~ "Rest of USA",
                             TRUE ~ "Other Country"),
         new_marst = case_when(marst2 == 1 ~ "Married",
                               marst2 == 2 ~ "Married, spouse absent",
                               TRUE ~ "Not Married"))


# Plot SEI distributions
ggplot(data = to_plot) +
  geom_density(aes(x = sei2, col = method)) +
  labs(x = "SEI", y = "Density", col = "Method") +
  ggsave(filename = here("figures", "compare_sei_linkage.pdf"), width = 8, height = 4, units = "in")

# Plot age distributions
ggplot(data = to_plot) +
  geom_density(aes(x = age2, col = method)) +
  labs(x = "Age", y = "Density", col = "Method") +
  ggsave(filename = here("figures", "compare_age_linkage.pdf"), width = 8, height = 4, units = "in")




ggplot(data = to_plot) +
  geom_density(aes(x = erscor502, col = method))

ggplot(data = to_plot) +
  geom_density(aes(x = edscor502, col = method))



## Should I try to make these stacked bars at some point?

# plot racial breakdown
to_plot %>% 
  group_by(method, race_grp2) %>% 
  summarise(n = n()) %>% 
  group_by(method) %>% 
  mutate(method_n = sum(n),
         pct = n / method_n * 100) %>% 
  bind_rows(tibble(method = c("Ferrie"),
                   race_grp2 = c("Other"),
                   n = 0,
                   method_n = 1036,
                   pct = 0)) %>% 
  mutate(race_grp2 = factor(race_grp2, levels = c("White", "Black", "Other")),
         label = paste0(round(pct, 1), "%\nN = ", n)) %>% 
  ggplot(aes(x = method, y = pct, group = race_grp2)) +
    geom_col(aes(fill = race_grp2), position = "dodge") +
    geom_text(aes(label = label, y = c(32, 25, 40, 32, 25, 40, 32, 40, 32, 25, 40, 25)), position = position_dodge(0.9), size = 3) +
    labs(x = "Linkage Method", y = "% of Sample", fill = "Race Category") +
    ggsave(filename = here("figures", "compare_race_linkage.pdf"), width = 8, height = 4, units = "in")



# plot birthplace breakdown
to_plot %>% 
  group_by(method, new_bpl) %>% 
  summarise(n = n()) %>% 
  group_by(method) %>% 
  mutate(method_n = sum(n),
         pct = n / method_n * 100) %>% 
  mutate(new_bpl = factor(new_bpl, levels = c("Pennsylvania", "Rest of USA", "Other Country")),
         label = paste0(round(pct, 1), "%\nN = ", n)) %>% 
  ggplot(aes(x = method, y = pct, group = new_bpl)) +
    geom_col(aes(fill = new_bpl), position = "dodge") +
    geom_text(aes(label = label, y = rep(c(20, 35, 27), 4)), position = position_dodge(0.9), size = 3) +
    labs(x = "Linkage Method", y = "% of Sample", fill = "Birthplace") +
    ggsave(filename = here("figures", "compare_birthplace_linkage.pdf"), width = 8, height = 4, units = "in")



# plot marriage status
to_plot %>% 
  group_by(method, new_marst) %>% 
  summarise(n = n()) %>% 
  group_by(method) %>% 
  mutate(method_n = sum(n),
         pct = n / method_n * 100) %>% 
  mutate(new_marst = factor(new_marst, levels = c("Married", "Married, spouse absent", "Not Married")),
         label = paste0(round(pct, 1), "%\nN = ", n)) %>% 
  ggplot(aes(x = method, y = pct, group = new_marst)) +
    geom_col(aes(fill = new_marst), position = "dodge") +
    geom_text(aes(label = label, y = rep(c(50, 37, 25), 4)), position = position_dodge(0.9), size = 3) +
    labs(x = "Linkage Method", y = "% of Sample", fill = "Marriage Status") +
    ggsave(filename = here("figures", "compare_marriage_linkage.pdf"), width = 8, height = 4, units = "in")
  




# let's look at representation in my sample across hyperparam thresholds
m_01_125 <- here("data", "linking_comparisons", "crosswalks", "mine_crosswalk-0.1-1.25.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2"))))

m_03_150 <- here("data", "linking_comparisons", "crosswalks", "mine_crosswalk-0.3-1.5.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2"))))

m_05_175 <- here("data", "linking_comparisons", "crosswalks", "mine_crosswalk-0.5-1.75.csv") %>% 
  import() %>% 
  inner_join(load_microdata(20, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "1")))) %>% 
  inner_join(load_microdata(30, formatted = F) %>% select_at(vars(-starts_with("us19")), funs(paste0(., "2"))))


ggplot() +
  geom_density(data = m_01_125, aes(x = sei2), col = "green") +
  geom_density(data = m_02_150, aes(x = sei2), col = "blue") +
  geom_density(data = m_03_175, aes(x = sei2), col = "red")

ggplot() +
  geom_density(data = m_01_125, aes(x = age2), col = "green") +
  geom_density(data = m_02_150, aes(x = age2), col = "blue") +
  geom_density(data = m_03_175, aes(x = age2), col = "red")




#### Analyze ML links, accuracy, and efficiency

# load training data to calculate accuracy metrics
source(here("R", "functions", "accuracy_metrics.R"))
source(here("R", "functions", "get_match.R"))

full_data <- import(here("data", "training_all_vars_v3.csv")) %>% 
  # add needed variables for linkage model
  mutate(mimatch = if_else(mi1 == mi2, 1, 0),
         mimatch = if_else(is.na(mimatch), 0, mimatch),
         ydiff_1 = if_else(ydiff == 1, 1, 0),
         ydiff_2 = if_else(ydiff == 2, 1, 0))

# save outcome as factor
full_data$match <- as.factor(full_data$match) %>% 
  recode_factor(`0` = "No",
                `1` = "Yes")

# save row names as case numbers for future merges
full_data$case <- row.names(full_data)

# set seed and choose random 60% for training data
set.seed(12345)
full_train <- sample_frac(full_data, 0.6, replace = F)

#full_train <- filter(full_data, case %in% test_index == F)
train_labs <- full_train$match

# set variables
vars_b <- c("exact", "exact_mult", "f_start", "l_start", "f_end", "l_end",
            "jw_frst", "jw_last", "fsoundex", "lsoundex", "mimatch", "ydiff_1", "ydiff_2",
            "n_match", "pct_match", "hh_size_diff", "avg_jw_frst", "hits")

vars_f <- c("exact", "exact_mult", "f_start", "l_start", "f_end", "l_end",
            "jw_frst", "jw_last", "fsoundex", "lsoundex", "mimatch", "ydiff_1", "ydiff_2", "hits")

# create training and test sets
train_b <- full_train[, vars_b]
train_f <- full_train[, vars_f]

test <- filter(full_data, case %in% full_train$case == F)

# set up options for model training (25 bootstrap draws)
# Train model using 25 bootstrap samples
fitControl <- trainControl(
  method = "boot",
  number = 25,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# run models on training data
model_b <- train(x = train_b, y = train_labs, method = "glm", family = binomial, trControl = fitControl)
model_f <- train(x = train_f, y = train_labs, method = "glm", family = binomial, trControl = fitControl)

# create table describing parameters
params <- expand.grid(b1 = c(0.1, 0.3, 0.5), b2 = c(1.25, 1.5, 1.75)) %>% 
  as_tibble() %>% 
  mutate(
    n_bell = 0,
    tpr_bell = 0,
    ppv_bell = 0,
    n_feig = 0,
    tpr_feig = 0,
    ppv_feig = 0
  )

# fill in cells of table for each hyper param combo
for(a in 1:9){
  # set hyper params
  b1 <- params$b1[a]
  b2 <- params$b2[a]
  
  # count matches from crosswalk
  params$n_feig[a] <- nrow(import(here("data", "linking_comparisons", "crosswalks", paste0("feigenbaum_crosswalk-", b1, "-", b2, ".csv"))))
  params$n_bell[a] <- nrow(import(here("data", "linking_comparisons", "crosswalks", paste0("mine_crosswalk-", b1, "-", b2, ".csv"))))
  
  # classify matches in leftover set of test data with hyper params
  feig <- define_matches(b1, b2, test, vars_f, model_f)
  feig$true_match <- test$match
  bell <- define_matches(b1, b2, test, vars_b, model_b)
  bell$true_match <- test$match
  
  # calculate TPR and PPV for each model
  params$tpr_feig[a] <- get_tpr(feig)
  params$ppv_feig[a] <- get_ppv(feig)
  params$tpr_bell[a] <- get_tpr(bell)
  params$ppv_bell[a] <- get_ppv(bell)
}

params %>% 
  export("~/Documents/Computer Backup/Dissertation/Chapter - Linkage/ml_results_mar1.csv", row.names = F)


#### Analyze overlap of links across methods ####

ferrie_ids <- unique(ferrie$uniqueid2)
bell_ids <- unique(m_05_175$uniqueid2)
feig_ids <- unique(f_05_175$uniqueid2)

## sort data by method they were linked with

# take original rows from sample that got linked in all methods
# does not include the links they were given, could differ across methods
in_all <- filter(samp,
                 uniqueid2 %in% ferrie_ids &
                   uniqueid2 %in% bell_ids &
                   uniqueid2 %in% feig_ids)

# create a "not in" operators
`%notin%` <- negate(`%in%`)

# get links only made by ferrie
only_ferrie <- filter(ferrie, 
                      uniqueid2 %notin% feig_ids &
                        uniqueid2 %notin% bell_ids)

# get links only made by bellman
only_bell <- filter(m_05_175, 
                        uniqueid2 %notin% ferrie_ids &
                          uniqueid2 %notin% feig_ids)

# get links only made by feigenbaum
only_feig <- filter(f_05_175, 
                        uniqueid2 %notin% ferrie_ids &
                          uniqueid2 %notin% bell_ids)

## plot density curves to compare distributions
# age
ggplot() +
  geom_density(data = only_ferrie, aes(x = age2), col = "red") +
  geom_density(data = only_bell, aes(x = age2), col = "blue") +
  geom_density(data = only_feig, aes(x = age2), col = "green")

# sei
ggplot() +
  geom_density(data = only_ferrie, aes(x = sei2), col = "red") +
  geom_density(data = only_bell, aes(x = sei2), col = "blue") +
  geom_density(data = only_feig, aes(x = sei2), col = "green")

# race
table(only_ferrie$race_grp2)
table(only_bell$race_grp2)
table(only_feig$race_grp2)


#### Analyze if any links were matched to different people in T1 by diff methods ####

c(here("data", "linking_comparisons", "crosswalks", "feigenbaum_crosswalk-0.5-1.75.csv"),
  here("data", "linking_comparisons", "crosswalks", "mine_crosswalk-0.5-1.75.csv"),
  here("data", "linking_comparisons", "crosswalks", "ferrie_crosswalk.csv")) %>% 
  map(import) %>% 
  bind_rows() %>% 
  unique() %>% 
  group_by(uniqueid2, uniqueid1) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) -> conflicting_ids
# thank goodness, no record got matched to different people by different methods!


#### Analyze change/stability in race of linked household heads ####
samples <- bind_rows(m_05_175, f_05_175, ferrie) %>% 
  mutate( 
    b1_b2 = if_else(race_grp1 == "Black" & race_grp2 == "Black", 1, 0),
    w1_w2 = if_else(race_grp1 == "White" & race_grp2 == "White", 1, 0),
    o1_o2 = if_else(race_grp1 == "Other" & race_grp2 == "Other", 1, 0),
    b1_w2 = if_else(race_grp1 == "Black" & race_grp2 == "White", 1, 0),
    w1_b2 = if_else(race_grp1 == "White" & race_grp2 == "Black", 1, 0),
    o_diff = if_else((race_grp1 == "Other" & race_grp2 != "Other") |
                       (race_grp1 != "Other" & race_grp2 == "Other"), 1, 0),
    race_chg = case_when(
      (b1_b2 == 1 | w1_w2 == 1 | o1_o2 == 1) ~ "Same",
      (b1_w2 == 1 | w1_b2 == 1) ~ "B/W conflict",
      o_diff == 1 ~ "Other conflict"
    )
  )

# simple tabulation of categories
samples %>% 
  group_by(method, race_chg) %>% 
  summarise(n = n())

# filter rows that actually have conflict in race category
conflicting <- samples %>% 
  filter(race_chg != "Same")

# split conflicting links that are in Bellman, Feigenbaum, or both
# first get ids of links made by both B and F
both_con_ids <- conflicting %>% 
  group_by(uniqueid2) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) %>% 
  pull(uniqueid2) 

# now filter the links into their own tables
both_con <- conflicting %>% 
  filter(uniqueid2 %in% both_con_ids) %>% 
  mutate(method = "Both") %>% 
  unique()

bell_con <- conflicting %>% 
  filter(method == "Bellman") %>% 
  filter(uniqueid2 %in% both_con_ids == F)

feig_con <- conflicting %>% 
  filter(method == "Feigenbaum") %>% 
  filter(uniqueid2 %in% both_con_ids == F)





