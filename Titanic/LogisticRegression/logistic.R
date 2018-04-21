library(data.table)
library(ggplot2)
library(ISLR)
library(leaps)


setwd("C:/MyStuff/DataScience/Projects/Kaggle/Titanic/LogisticRegression/")
# In this script I'll fit a logistic regression model on the Titanic dataset. 
# First, I'm going to source the cleaning script that I wrote so I have the clean
# train and test sets in this environment
source("C:/MyStuff/DataScience/Projects/Kaggle/Titanic/Cleaning.R")

# Load in the submission file
test.responses <- fread("C:/MyStuff/DataScience/Projects/Kaggle/Titanic/gender_submission.csv")


# To fit the logistic regression model, I'll use the glm function 
log.fit <- glm(Survived ~ Pclass + Sex + SibSp + Parch + Cabin, data = train, family = binomial)

# Look at a summary of the fit
summary(log.fit)

# A few interesting things: 1) the Fare variable isn't at all significant, which 
# I thought might be the case after the EDA; this might be because of the correlation
# it has with the Pclass variable. 2) The Sex and Pclass variables are the most significant
# 3) The Cabin predictor is significant at the .05 level. 4) The SibSp predictor is also
# significant


# Let's create some predictions and see what happens
log.preds <- predict(log.fit, newdata = test, type = "response")
log.preds <- ifelse(log.preds > .5, 1, 0)


# Let's use cross validation to choose the probability we determine the Survived 
# value at
probs <- seq(.2, .8, by = .01)


err <- vector("numeric", length = length(probs))
for (i in 1:length(probs)) {
  fit <- glm(Survived ~ Pclass + Sex + SibSp + Parch + Cabin, data = train, family = binomial)
  log.preds <- predict(fit, newdata = test, type = "response")
  log.preds <- ifelse(log.preds > probs[i], 1, 0)
  err[i] <- mean(test.responses$Survived == log.preds)
}

# The optimal prob is 
probs[which.max(err)]

# Let's create some new predictions using this
log.preds <- ifelse(log.preds > .49, 1, 0)

# I'll use this as a first submission to get a baseline of the model
subm <- data.table("PassengerId" = test.responses[, PassengerId],
                   "Survived" = as.integer(log.preds))


fwrite(x = subm, "logis_sub_1.csv")


# Let's fit a model with Fare and see how it holds up

# To fit the logistic regression model, I'll use the glm function 
log.fit <- glm(Survived ~ Pclass_bin + Sex + SibSp + Cabin + fare.median + Parch_bin, data = train, family = binomial)

# Look at a summary of the fit
summary(log.fit)

# CV for prob cut point
probs <- seq(.2, .8, by = .01)


err <- vector("numeric", length = length(probs))
for (i in 1:length(probs)) {
  fit <- glm(Survived ~ Pclass_bin + Sex + SibSp + Parch_bin + Cabin + fare.median, data = train, family = binomial)
  log.preds <- predict(fit, newdata = test, type = "response")
  log.preds <- ifelse(log.preds > probs[i], 1, 0)
  err[i] <- mean(test.responses$Survived == log.preds)
}

# .59 is the optimal prob
probs[which.max(err)]


# Let's create some predictions and see what happens
log.preds <- predict(log.fit, newdata = test, type = "response")
log.preds <- ifelse(log.preds > .65, 1, 0)


# I'll use this as a first submission to get a baseline of the model
subm <- data.table("PassengerId" = test.responses[, PassengerId],
                   "Survived" = as.integer(log.preds))


fwrite(x = subm, "logis_sub_1.csv")

# These two models have been to good starting models to get an idea of the predictors
# and the response. However, we should contact a more formal model selection process.
# I'll conduct best subset selection using the regsubsets() function. I'll do this
# three times. Once using PClass_bin, once using Parch_bin, and once using both. 
# I'll use the binarized predictors in place of their regular counterparts. The 
# reason I don't want to use the binarized and regular predictors in the same model
# is because I think this would add a lot of correlation into the model. Once I  
# have the best model across the three groups using best subset selection, I'll 
# compare the three and see which is the best using test error
reg.pclass_bin <- regsubsets(Survived ~ Pclass_bin + Sex + SibSp + Parch + Cabin + fare.median, data = train)
reg.pclass_bin.summary <- summary(reg.pclass_bin)


# Now Let's create a data.table to visualize the results using the Cp, BIC, and Adjr2
pclass_bin.best.subset <- data.table("NumberOfPredictors" = 1:6,
                                     "AdjustedRSquared" = reg.pclass_bin.summary$adjr2,
                                     "CpStatistic" = reg.pclass_bin.summary$cp,
                                     "BIC" = reg.pclass_bin.summary$bic)

reg.parch_bin <- regsubsets(Survived ~ Pclass + Sex + SibSp + Parch_bin + Cabin + fare.median, data = train)
reg.parch_bin.summary <- summary(reg.parch_bin)


# Now Let's create a data.table to visualize the results using the Cp, BIC, and Adjr2
both_bin.best.subset <- data.table("NumberOfPredictors" = 1:7,
                                     "AdjustedRSquared" = reg.parch_bin.summary$adjr2,
                                     "CpStatistic" = reg.parch_bin.summary$cp,
                                     "BIC" = reg.parch_bin.summary$bic)


reg.both_bin <- regsubsets(Survived ~ Pclass_bin + Sex + SibSp + Parch_bin + Cabin + fare.median, data = train)
reg.both_bin.summary <- summary(reg.both_bin)


# Now Let's create a data.table to visualize the results using the Cp, BIC, and Adjr2
both_bin.subset <- data.table("NumberOfPredictors" = 1:6,
                                    "AdjustedRSquared" = reg.both_bin.summary$adjr2,
                                    "CpStatistic" = reg.both_bin.summary$cp,
                                    "BIC" = reg.both_bin.summary$bic)

reg.both <- regsubsets(Survived ~ Pclass + Sex + SibSp + Parch + Cabin + fare.median, data = train)
reg.both.summary <- summary(reg.both)


# Now Let's create a data.table to visualize the results using the Cp, BIC, and Adjr2
both.subset <- data.table("NumberOfPredictors" = 1:7,
                               "AdjustedRSquared" = reg.both.summary$adjr2,
                               "CpStatistic" = reg.both.summary$cp,
                               "BIC" = reg.both.summary$bic)



# It looks like using the CpStat and the BIC for the "both" group of models leads 
# to the full model being the best. For the Parch_bin model, there is unfortunatley
# a lot of disagreement between the metrics - none of them correspond to the same model.
# I'll use the full model in this case as well. Another set back for this one is 
# that there are actually predictors for each level of the Pclass variable, so 
# the Pclass2 predictor is only included in the 7 predictor model, and it leads 
# to a higher CpStat. I'll use the full because it balances the CpStat and 
# the BIC metrics. For the Pclass_bin model there is also some disagreement. I'll use
# the 5 predictor model for this, as it has the minimized CpStat and the second
# lowest BIC. For the model with both of the predictors in their regular forms, 
# I'll choose the 5 predictor model


# Now let's use CV to find the probability that should be our cut point for the response

# CV for prob cut point
probs <- seq(.2, .8, by = .01)


err <- vector("numeric", length = length(probs))
for (i in 1:length(probs)) {
  fit <- glm(Survived ~ Pclass_bin + Sex + SibSp + Cabin + fare.median, data = train, 
                               family = binomial)
  log.preds <- predict(fit, newdata = test, type = "response")
  log.preds <- ifelse(log.preds > probs[i], 1, 0)
  err[i] <- mean(test.responses$Survived == log.preds)
}

# .57 is the optimal prob
probs[which.max(err)]

# Pclass_bin model
pclass_bin.fit <- glm(Survived ~ Pclass_bin + Sex + SibSp + Cabin + fare.median, data = train, 
                      family = binomial)

# Predictions
pclass_bin.preds <- predict(pclass_bin.fit, newdata = test, type = "response")
pclass_bin.preds <- ifelse(pclass_bin.preds >= .57, 1, 0)


err <- vector("numeric", length = length(probs))
for (i in 1:length(probs)) {
  fit <- glm(Survived ~ Pclass + Sex + SibSp + Parch_bin + Cabin + fare.median, data = train,
             family = binomial)
  log.preds <- predict(fit, newdata = test, type = "response")
  log.preds <- ifelse(log.preds > probs[i], 1, 0)
  err[i] <- mean(test.responses$Survived == log.preds)
}

# .51 is the optimal prob
probs[which.max(err)]

parch_bin.preds <- predict(parch_bin.fit, newdata = test, type = "response")
parch_bin.preds <- ifelse(parch_bin.preds >= .51, 1, 0)



parch_bin.fit <- glm(Survived ~ Pclass + Sex + SibSp + Parch_bin + Cabin + fare.median, data = train,
                     family = binomial)

# both_bin model
err <- vector("numeric", length = length(probs))
for (i in 1:length(probs)) {
  fit <- glm(Survived ~ Pclass_bin + Sex + SibSp + Parch_bin + Cabin + fare.median, data = train,
             family = binomial)
  log.preds <- predict(fit, newdata = test, type = "response")
  log.preds <- ifelse(log.preds > probs[i], 1, 0)
  err[i] <- mean(test.responses$Survived == log.preds)
}

# .59 is the optimal prob
probs[which.max(err)]


both_bin.fit <- glm(Survived ~ Pclass_bin + Sex + SibSp + Parch_bin + Cabin + fare.median, data = train,
                family = binomial)

both_bin.preds <- predict(both_bin.fit, newdata = test, type = "response")
both_bin.preds <- ifelse(both_bin.preds >= .59, 1, 0)


# both_bin model
err <- vector("numeric", length = length(probs))
for (i in 1:length(probs)) {
  fit <- glm(Survived ~ Pclass + Sex + SibSp + Cabin + fare.median, data = train,
             family = binomial)
  log.preds <- predict(fit, newdata = test, type = "response")
  log.preds <- ifelse(log.preds > probs[i], 1, 0)
  err[i] <- mean(test.responses$Survived == log.preds)
}

# .56 is the optimal prob
probs[which.max(err)]


both.fit <- glm(Survived ~ Pclass + Sex + SibSp + Cabin + fare.median, data = train,
                family = binomial)

both.preds <- predict(both.fit, newdata = test, type = "response")
both.preds <- ifelse(both.preds >= .56, 1, 0)


mean(test.responses$Survived == pclass_bin.preds)
mean(test.responses$Survived == parch_bin.preds)
mean(test.responses$Survived == both_bin.preds)
mean(test.responses$Survived == both.preds)

# The best model is the both_bin model. I'll create on last submission using this

# I'll use this as a first submission to get a baseline of the model
subm <- data.table("PassengerId" = test.responses[, PassengerId],
                   "Survived" = as.integer(both_bin.preds))


fwrite(x = subm, "logis_sub_1.csv")




