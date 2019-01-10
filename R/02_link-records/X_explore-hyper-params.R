library(tidyverse)
library(rio)
library(here)
library(caret)
library(caTools)

source(here("R", "functions", "build_comparisons.R"))
source(here("R", "functions", "add_matching_vars.R"))
source(here("R", "functions", "prepare_city.R"))
source(here("R", "functions", "unabbv_names.R"))
source(here("R", "functions", "compare_hh_rosters.R"))
source(here("R", "functions", "all_row_combos.R"))
source(here("R", "functions", "relate_pt.R"))
source(here("R", "functions", "get_match.R"))

# load pre-preprocessed data
full_data <- import(here("data", "training_all_vars_v3.csv")) %>% 
  mutate(mimatch = if_else(mi1 == mi2, 1, 0),
         mimatch = if_else(is.na(mimatch), 0, mimatch)) %>% 
  as_tibble()

# New problems with Parsing
# there is a row that's fucking everything up because of a missing comma between the match column and a serial number
# And for some reason, there is a mistake that's putting a soundex code in a binary soundex match indicator

# fix strange parsing errors
#full_data[full_data$uniqueid1 == "16449491_2_1910", "pct_exact"] <- 33.33333333333333
#full_data[full_data$uniqueid1 == "16449491_2_1910", "pct_match_int"] <- 2222.2222222222217

# save outcome as factor
full_data$match <- as.factor(full_data$match) %>% 
  recode_factor(`0` = "No",
                `1` = "Yes")

# split data into training and testing sets
to_train <- full_data %>% 
  select(serial2) %>% 
  unique() %>% 
  sample_frac(0.5) %>% 
  .[["serial2"]]

train <- filter(full_data, serial2 %in% to_train)
test <- filter(full_data, serial2 %in% to_train == F)

# grab names of features in the models
vars_mine <- c("match", # variable to predict
          # name indicators
          #"exact", 
          "exact_all", 
          "f_start", "l_start", "f_end", "l_end",
          "jw_frst", "jw_last", 
          "fsoundex", "lsoundex",
          # indicators based on other potential matches
          "hits", "hits2",
          # indicators based on household comparisons
          "n_match", "pct_match", "hh_size_diff", "avg_jw_frst")

vars_feig <- c("match", # variable to predict
          # name indicators
          "exact", 
          "exact_all", "exact_mult",
          "f_start", "l_start", "f_end", "l_end",
          "jw_frst", "jw_last", 
          "fsoundex", "lsoundex", "mimatch",
          # indicators based on other potential matches
          "hits", "hits2", "ydiff")

# keep only match outcome and predictors
train_mine <- train[,vars_mine]
test_mine <- test[,vars_mine]
train_feig <- train[,vars_feig]
test_feig <- test[,vars_feig]

### Train a probit model with 10-fold cross-valiation
set.seed(123)

# Train model using 10-fold cross-validation using AUC
fitControl <- trainControl(
  method = "boot",
  number = 25,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train a model
glm_mine <- train(
  match ~ .,
  data = train_mine,
  method = "glm",
  family = binomial,
  trControl = fitControl
  ## Center and scale the predictors for the training
  ## set and all future samples.
  #preProc = c("center", "scale")
)

glm_feig <- train(
  match ~ .,
  data = train_feig,
  method = "glm",
  family = binomial,
  trControl = fitControl
  ## Center and scale the predictors for the training
  ## set and all future samples.
  #preProc = c("center", "scale")
)

print(glm_mine)
print(glm_feig)

#glm1

# look at specific objects within the model object
#names(glm1)
#str(glm1$finalModel)
#glm1$finalModel$coefficients

# Define a function to apply to block of rows based on focal record
# When applying to final links, need to make sure that records in t2 are never matched to the same record in t1
# keep only the highest match score, explore if I can make a reasonable guess without rematching
#get_match <- function(block, model, b1, b2){
#  # instructions for more than one possible match
#  if(nrow(block) > 1){
#    # predict probabilities of match
#    block$match_prob <- predict(model, block, type = "prob")$Yes
#    
#    # set the best and second-best match scores
#    best_prob <- sort(block$match_prob, decreasing = T)[1]
#    next_best_prob <- sort(block$match_prob, decreasing = T)[2]
#    
#    # define match
#    block %>% 
#      mutate(match = if_else(match_prob == best_prob &
#                               match_prob > b1 &
#                               best_prob / next_best_prob > b2, 1, 0))
#    # instructions if only one possible match
#  } else {
#    # predict probabilities of match
#    block$match_prob <- predict(glm1, block, type = "prob")$Yes
#    # define match
#    block %>% 
#      mutate(match = if_else(match_prob > b1, 1, 0))
#  }
#}
#
# function to define matches in full dataset
define_matches <- function(b1, b2, data, vars, model){
  data %>%
    split(.$serial2) %>% 
    map(select, vars[-1]) %>% 
    map(get_match, model, b1, b2) %>% 
    bind_rows()
}

# Do a grid search to find ideal values for b1 and b2, following Feigenbaum 2016
params <- all_row_combos(tibble(b1 = seq(0, 0.4, 0.01)),
                         tibble(b2 = seq(1, 2, 0.03125))) %>% as_tibble()

param_results <- map2(params[["b1"]], params[["b2"]], define_matches, test, vars_feig, glm1)

#reattach correct matches because I overwrote them clumsily
for(a in 1:length(param_results)){
  param_results[[a]]$true_match <- test$match
}

# calculate TPR and PPV

get_tpr <- function(test){
  all_pos <- nrow(filter(test, true_match == "Yes"))
  true_pos <- nrow(filter(test, auto_match == 1 & true_match == "Yes"))
  true_pos / all_pos
}

get_ppv <- function(test){
  matches <- nrow(filter(test, auto_match == 1))
  true_pos <- nrow(filter(test, auto_match == 1 & true_match == "Yes"))
  true_pos / matches
}

params$tpr <- unlist(map(param_results, get_tpr))
params$ppv <- unlist(map(param_results, get_ppv))

# create ggplot based on Feigenbaum paper

ggplot(params) +
  geom_tile(aes(x = b1, y = b2, fill = tpr), col = "white", size = 0.05) +
  labs(title = "True Positive Rate hyper-parameter grid")

ggplot(params) +
  geom_tile(aes(x = b1, y = b2, fill = ppv), col = "white", size = 0.05) +
  labs(title = "Positive Prediction Value hyper-parameter grid")

# use the simple sum (can be weighted) of two quality metrics
params$sum <- params$tpr + (1*params$ppv)

params[params$sum == max(params$sum),]
# first try generated 8 tiles, probably will drop as N in training data goes up

### Unweighted optimal parameters are:
# b1 = 0.2
# b2 = 1.57

### For Feigenbaum:
# b1 = 0.12
# b2 = 1.16


### calculating PPV and TPR for Dec methods draft
# set of nine parameter combos used
params <- all_row_combos(tibble(b1 = c(0.1, 0.2, 0.3)),
                         tibble(b2 = c(1.25, 1.5, 1.75))) %>% as_tibble()

param_results_mine <- map2(params[["b1"]], params[["b2"]], define_matches, test, vars_mine, glm_mine)
param_results_feig <- map2(params[["b1"]], params[["b2"]], define_matches, test, vars_feig, glm_feig)

#reattach correct matches because I overwrote them clumsily
for(a in 1:9){
  param_results_mine[[a]]$true_match <- test$match
  param_results_feig[[a]]$true_match <- test$match
}

params$tpr_mine <- unlist(map(param_results_mine, get_tpr))
params$ppv_mine <- unlist(map(param_results_mine, get_ppv))
params$tpr_feig <- unlist(map(param_results_feig, get_tpr))
params$ppv_feig <- unlist(map(param_results_feig, get_ppv))






##########
# read in a test data set for predicting real matches
demo <- import(here("data", "test_matches_10_20.csv")) %>% 
  as_tibble()

results <- define_matches(b1 = 0.2, b2 = 1.57, data = demo, model = glm1)




ggplot(results) +
  geom_histogram(aes(x = match_prob), binwidth = 0.01) +
  geom_vline(xintercept = 0.2, col = "red", size = 1)




##########
# read in the full 10-20 data set for predicting real matches
full1020 <- import(here("data", "potential_matches_10_20.csv")) %>% 
  as_tibble()

# retrain model on full training set, name as glm2
full_training <- full_data[vars]

set.seed(123)

# Train model with all data
glm2 <- train(
  match ~ .,
  data = full_training,
  method = "glm",
  family = binomial,
  trControl = fitControl
  ## Center and scale the predictors for the training
  ## set and all future samples.
  #preProc = c("center", "scale")
)





results <- define_matches(b1 = 0.2, b2 = 1.57, data = full1020, model = glm2)

ggplot(results) +
  geom_histogram(aes(x = match_prob), binwidth = 0.01) +
  geom_vline(xintercept = 0.2, col = "red", size = 1)






### Running Random Forest models
set.seed(123)

# Train model using 10-fold cross-validation using AUC
RF_fit <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

grid1 <- expand.grid(
  mtry = 1:18,
  splitrule = "gini",
  min.node.size = 20
)

rf1 <- train(
  match ~ .,
  data = full_data,
  method = "ranger",
  tuneGrid = grid1,
  trControl = RF_fit
)

rf1
plot(rf1)
