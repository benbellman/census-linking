library(tidyverse)
library(here)
library(caret)
library(mlbench)
library(caTools)

# combine all coded training data files
files <- list.files(here("data", "Coded"), full.names = T)

test <- try(map(files, read_csv))


manual_set$match <- as.factor(manual_set$match)

# grab names of predictors in the model + match
vars <- names(training[,c(3, 32:43, 48:57)])

# keep only match outcome and predictors
#manual_set <- select(manual_set, 3, 32:57)

# create training/testing files from unique focal records
set.seed(101)
train_index <- sample(unique(manual_set$uniqueid40), 75)

training <- filter(manual_set, uniqueid40 %in% train_index)
testing  <- filter(manual_set, !(uniqueid40 %in% train_index))

# Specify Feigenbaum's training method
# 25 bootstrap samples
fitControl <- trainControl(method = "boot", number = 25)

# Train a model
set.seed(202)
Probit1 <- train(
  match ~ .,
  data = training[,vars],
  method = "glm",
  family = "",
  trControl = fitControl
  ## Center and scale the predictors for the training
  ## set and all future samples.
  #preProc = c("center", "scale")
)

summary(Probit1)
Logit1

# plot the results
trellis.par.set(caretTheme())
plot(Logit1) 
ggplot(Logit1)

#look inside model object
str(Logit1)
names(Logit1)

str(Logit1$finalModel)
str(Logit1$terms)

#Look at variables/coefficients





