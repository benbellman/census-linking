library(tidyverse)
library(here)
library(caret)
library(caTools)

# load functions for calculating matching variables
source(here("R", "functions", "add_matching_vars.R"))
source(here("R", "functions", "compare_hh_rosters.R"))
source(here("R", "functions", "load_training_data.R"))
source(here("R", "functions", "unabbv_names.R"))

### combine all coded training data files

# first, load file containing household members of training cases
for_rosters <- read_csv(here("data", "for_rosters.csv")) 

# add newly coded training files
#full_data <- load_training_data(here("data", "Training"))
#write_csv(full_data, here("data", "training_all_vars_v1.csv"))

# load pre-preprocessed data
full_data <- read_csv(here("data", "training_all_vars_v1.csv"))

# fix random parsing errors
full_data[full_data$uniqueid1 == "16449491_2_1910", "pct_exact"] <- 33.33333333333333
full_data[full_data$uniqueid1 == "16449491_2_1910", "pct_match_int"] <- 2222.2222222222217

# save outcome as factor
full_data$match <- as.factor(full_data$match) %>% 
  recode_factor(`0` = "No",
                `1` = "Yes")

# grab names of features in the models
vars <- c("match", # variable to predict
          # name indicators
          "exact", "exact_all", 
          "f_start", "l_start", "f_end", "l_end",
          "jw_frst", "jw_last", 
          "fsoundex", "lsoundex",
          # indicators based on other potential matches
          "hits", "hits2", "exact_mult",
          # indicators based on household comparisons
          "not_head_t1", "mar_consistent", "n_fuzzy", "pct_fuzzy", "hh_size_diff")

# keep only match outcome and predictors
full_data <- full_data[,vars]



### Train a probit model with 10-fold cross-valiation
set.seed(123)

# Train model using 10-fold cross-validation using AUC
fitControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train a model
glm1 <- train(
  match ~ .,
  data = full_data,
  method = "glm",
  family = binomial,
  trControl = fitControl
  ## Center and scale the predictors for the training
  ## set and all future samples.
  #preProc = c("center", "scale")
)

glm1

# look at specific objects within the model object
names(glm1)
str(glm1$finalModel)
glm1$finalModel$coefficients



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
