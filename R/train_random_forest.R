library(tidyverse)
library(randomForest)
library(caret)
library(broom)
library(here)


training_set <- read_csv(here("data","Training","training_prelim_v1_1_full.csv"))

# select only the vars for prediction, plus matching
rf1 <- randomForest(formula = match ~ ., 
                    data = training_set[,c(3, 32:57)])
