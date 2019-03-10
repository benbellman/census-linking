library(dplyr)
library(purrr)
library(tibble)
library(rio)
library(here)
library(caret)
library(lime)
library(tidyr)


#### Data set-up ####

# load pre-preprocessed training data
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

# set seed and choose random 10% of points to evaluate each model
set.seed(12345)
test_index <- sample(1:nrow(full_data), round(nrow(full_data)/10), replace = F)

# set variables
vars_b <- c("exact", "exact_mult", "f_start", "l_start", "f_end", "l_end",
            "jw_frst", "jw_last", "fsoundex", "lsoundex", "mimatch", "ydiff_1", "ydiff_2",
            "n_match", "pct_match", "hh_size_diff", "avg_jw_frst", "hits")

vars_f <- c("exact", "exact_mult", "f_start", "l_start", "f_end", "l_end",
            "jw_frst", "jw_last", "fsoundex", "lsoundex", "mimatch", "ydiff_1", "ydiff_2", "hits")

# create training and test sets
train_b <- full_data[-test_index, vars_b]
test_b <- full_data[test_index, vars_b]

train_f <- full_data[-test_index, vars_f]
test_f <- full_data[test_index, vars_f]

labs <- full_data[-test_index, ]$match

#### Excecute LIME analysis ####

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
model_b <- train(x = train_b, y = labs, method = "glm", family = binomial, trControl = fitControl)
#model_b <- train(x = train_b, y = labs, method = "rf")


# can't use parsnip until I update R version and re-install all packages
#model_b_pars <- logistic_reg(penalty = 0, mixture = 0) %>% 
#  set_engine("glm") %>% 
#  fit(match ~ ., data = train_b)


model_f <- train(x = train_f, y = labs, method = "glm", family = binomial, trControl = fitControl)
#model_f <- train(x = train_f, y = labs, method = "rf")

# create explainer objects
explainer_b <- lime(train_b, model_b)
explainer_f <- lime(train_f, model_f)

# explain predictions of test cases, split feature explainations up by observation
explanation_b <- explain(test_b, explainer_b, labels = c("Yes"), n_features = 10)
explanation_f <- explain(test_f, explainer_f, labels = c("Yes"), n_features = 10)

# combine into single table
explanation_b$Method <- "Bellman"
explanation_f$Method <- "Feigenbaum"
lime_obvs <- bind_rows(explanation_b, explanation_f)

# turn probability of match into its own column
lime_obvs$prob_match <- map_dbl(lime_obvs$prediction, ~ .x$Yes)

## Visualize the distribution of match probabilities

# plot probabilities less than 0.02
low_labels <- lime_obvs %>% 
  group_by(case, Method) %>% 
  summarise(prob_match = max(prob_match)) %>% 
  group_by(Method) %>% 
  mutate(count = n()) %>% 
  filter(prob_match <= 0.02) %>% 
  summarise(n = n(),
            count = max(count)) %>% 
  mutate(pct = round(n / count * 100, 1),
            label = paste0(Method, ": \n", pct, "% of test obs."),
            x = 0.0125,
            y = c(225, 160))

lime_obvs %>% 
  group_by(case, Method) %>% 
  summarise(prob_match = max(prob_match)) %>% 
  ggplot() +
    geom_density(aes(x = prob_match, col = Method)) +
    geom_text(data = low_labels, aes(label = label, x = x, y = y), size = 3) +
    xlim(0, 0.02) +
    xlab("Probability link is true") +
    ggsave(filename = here("figures", "compare_p_scores_low.pdf"), width = 4, height = 4, units = "in")

# plot probabilities greater than 0.9
high_labels <- lime_obvs %>% 
  group_by(case, Method) %>% 
  summarise(prob_match = max(prob_match)) %>% 
  group_by(Method) %>% 
  mutate(count = n()) %>% 
  filter(prob_match >= 0.9) %>% 
  summarise(n = n(),
            count = max(count)) %>% 
  mutate(pct = round(n / count * 100, 1),
         label = paste0(Method, ": \n", pct, "% of test obs."),
         x = 0.95,
         y = c(135, 85))

lime_obvs %>% 
  group_by(case, Method) %>% 
  summarise(prob_match = max(prob_match)) %>% 
  ggplot() +
    geom_density(aes(x = prob_match, col = Method)) +
    geom_text(data = high_labels, aes(label = label, x = x, y = y), size = 3) +
    xlim(0.9, 1) +
    ylim(0, 150) +
    xlab("Probability link is true") +
    ggsave(filename = here("figures", "compare_p_scores_high.pdf"), width = 4, height = 4, units = "in")



# box and whisker plots of test data probabilty scores based on method / ground truth of link
lime_obvs %>% 
  group_by(case, Method) %>% 
  summarise(prob_match = max(prob_match)) %>% 
  inner_join(full_data[test_index, c("case", "match")]) %>% 
  ggplot() +
    #geom_boxplot(aes(x = match, y = prob_match, col = match)) +
    geom_point(aes(x = match, y = prob_match, col = match), alpha = 0.5, position = "jitter") +
    facet_wrap(~ Method) + 
    guides(col=FALSE) +
    labs(x = "True match", y = "Estimated probability of match")


# First let's take a look at the overall importance of variables across my cases and 2 models
features <- lime_obvs %>%
  mutate(affirms = if_else(feature_weight > 0, 1, 0),
         contradicts = if_else(feature_weight < 0, 1, 0)) %>% 
  group_by(feature, Method) %>% 
  summarise(appears = n(),
            affirms = sum(affirms),
            contradicts = sum(contradicts),
            mean_abs_wt = mean(abs(feature_weight)))
  
  



# now, let's specifically look at the "problem" cases
# this is a function to analyze / plot a single case


# that is, cases that both models are getting very wrong
# Type 1 errors
t1_errors <- lime_obvs %>% 
  group_by(case, Method) %>% 
  summarise(prob_match = max(prob_match)) %>% 
  spread(Method, prob_match) %>% 
  inner_join(full_data[test_index, c("case", "match")]) %>% 
  filter(Feigenbaum > 0.25 & match == "No")


# specific type 1 errors (false positives)
# case 4275, med F, low B (F is wrong, I am right)
# case is a young unmarried woman that, according to manual match, left her childhood household. 
# Link made on exact name string match, but there are 10 total potential matches in block.
# correct link is case 4273
test_4275 <- filter(full_data, serial2 == full_data["4275", "serial2"])
filter(lime_obvs, case == "4275") %>% 
  plot_explanations() + facet_wrap( ~ Method)

# case 1427, med-high F, low-med B (F is wrong, I am kinda right)
test_1427 <- filter(full_data, serial2 == full_data["1427", "serial2"])
filter(lime_obvs, case == "1427") %>% 
  plot_explanations() + facet_wrap( ~ Method)

# case 4852, high F, high B (Both F and B are very wrong)
test_4852 <- filter(full_data, serial2 == full_data["4852", "serial2"])
filter(lime_obvs, case == "4852") %>% 
  plot_explanations() + facet_wrap( ~ Method)


# Type 2 errors
t2_errors <- lime_obvs %>% 
  group_by(case, Method) %>% 
  summarise(prob_match = max(prob_match)) %>% 
  spread(Method, prob_match) %>% 
  inner_join(full_data[test_index, c("case", "match")]) %>% 
  filter(Feigenbaum < 0.25 & match == "Yes")


# specific type 2 errors
# case 2853 or 5546, very low F, very low B (both are very wrong)

# case 5430, low F, very high B (F is wrong, I am right)

# case 4387, med-high F, low B (F is kinda right, I am wrong)




### DING DING DING WE HAVE A WINNER

# Case 4404
# Both models were generaly having "hits" as the most important variable
# This is intutitive why it's so importaat, but what if that is removed? (also hits2 is bogus, always creates weird results)
# In the Bellman framework, the probability of a match is essentially 1 with AND without "hits" var
# But in the Feigenbaum framework, That same case has prob of 0.58 with "hits," 0.36 without
# hh_size_diff and avg_jw_frst do a lot of legwork for this case, and in general

