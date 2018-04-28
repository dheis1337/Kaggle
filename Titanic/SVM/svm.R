library(data.table)
library(ggplot2)
library(ISLR)
library(leaps)
library(e1071)

source("C:/MyStuff/DataScience/Projects/Kaggle/Titanic/Cleaning.R")

setwd("C:/MyStuff/DataScience/Projects/Kaggle/Titanic/SVM")


# Split the train data set into a train and test set 
index <- sample(891, 600)

train <- dat[index]
test <- dat[-index]

# Inspect data
str(train)


# fit linear model with cost of 10 to get a baseline idea for model performance
svm.linear <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median, data = dat, kernel = "linear")

# predict values using linear model
preds.linear <- predict(svm.linear, newdata = sub)

# I'll use this as a first submission to get a baseline of the model
subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.linear)))


fwrite(x = subm, "svm_linear.csv")

# Use cross validation to determine the optimal cost variable
tuned.linear <- tune(svm, Survived ~ Pclass + Sex + Cabin + fare.median + age.median, data = dat, kernel = "linear", 
                     ranges = list(cost = c(.0001, .001, .01, .1, 1, 10, 100, 1000)))

summary(tuned.linear)

# It looks like there's actually agreement between costs ranging from .1 to 1000. 
# As such, I'll just create a model using each and submit to see how the results differ
svm.linear_point1 <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                         data = dat, kernel = "linear", cost = .1) 

# Create predictions
preds.linear <- predict(svm.linear_point1, newdata = sub)

subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.linear)))


fwrite(x = subm, "svm_linear.csv")

# I already made a model with cost = 1, so now I'll move on to 10
svm.linear10 <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                         data = dat, kernel = "linear", cost = 10) 

# Create predictions
preds.linear <- predict(svm.linear10, newdata = sub)

subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.linear)))


fwrite(x = subm, "svm_linear.csv")

# SVM with cost = 100
svm.linear100 <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                    data = dat, kernel = "linear", cost = 100) 

# Create predictions
preds.linear <- predict(svm.linear100, newdata = sub)

subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.linear)))


fwrite(x = subm, "svm_linear.csv")


# SVM with cost = 1000
svm.linear1000 <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                     data = dat, kernel = "linear", cost = 1000) 

# Create predictions
preds.linear <- predict(svm.linear1000, newdata = sub)

subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.linear)))


fwrite(x = subm, "svm_linear.csv")

# It looks like a linear svm (support vector classifier) isn't good enough to 
# accurately fit the data. Let's use a polynomial one
svm.poly <- tune(svm, Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                 data = dat, kernel = "polynomial", 
                 ranges = list(cost = c(.0001, .001, .01, .1, 1, 10, 100, 1000)))

# It looks like the cost of 100 is the best model
summary(svm.poly)



# SVM with cost = 100
svm.poly <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                      data = dat, kernel = "polynomial", cost = 100) 

# Create predictions
preds.poly <- predict(svm.poly, newdata = sub)

subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.poly)))


fwrite(x = subm, "svm_poly.csv")

# Now I'll create a radial svm just to see how that works.
svm.radial <-tune(svm, Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                  data = dat, kernel = "radial", 
                  ranges = list(cost = c(.0001, .001, .01, .1, 1, 10, 100, 1000)))

# It looks like the cost of 100 leads to the best model
summary(svm.radial)

# SVM with cost = 100
svm.radial <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                data = dat, kernel = "radial", cost = 100) 

# Create predictions
preds.radial <- predict(svm.radial, newdata = sub)

subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.radial)))


fwrite(x = subm, "svm_radial.csv")

# The radial svm worked the best. Let's see if we can use cross validation to 
# determine a level of gamma to improve our predictions
svm.radial <- svm.radial <-tune(svm, Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                                data = dat, kernel = "radial", cost = 100, 
                                ranges = list(gamma = c(.0001, .001, .01, .1, 1, 10, 100, 1000)))

# It looks like a gamma of .1 is best 
summary(svm.radial)


# SVM with cost = 100
svm.radial <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median,
                  data = dat, kernel = "radial", cost = 100, gamma = .1) 

# Create predictions
preds.radial <- predict(svm.radial, newdata = sub)

subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.radial)))


fwrite(x = subm, "svm_radial.csv")


# Now let's see what happends if we use a few more variables. I'll work with the 
# radial svm first since it performed best in the above instances. Let's first find
# a cost value
svm.radial <- svm.radial <-tune(svm, Survived ~ Pclass + Sex + Cabin + fare.median + age.median +
                                  Parch + SibSp,
                                data = dat, kernel = "radial", 
                                ranges = list(cost = c(.0001, .001, .01, .1, 1, 10, 100, 1000)))

# The optimal cost value is 1
summary(svm.radial)


# Now let's do the same to find an optimal gamma
svm.radial <- svm.radial <-tune(svm, Survived ~ Pclass + Sex + Cabin + fare.median + age.median +
                                  Parch + SibSp,
                                data = dat, kernel = "radial", cost = 1,
                                ranges = list(gamma = c(.0001, .001, .01, .1, 1, 10, 100, 1000)))

# The optimal gamma value is .1
summary(svm.radial)


# Radial SVM gamma = .1, cost = 1
svm.radial <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median +
                    Parch + SibSp,
                  data = dat, kernel = "radial", cost = 1, gamma = .1) 

# Create predictions
preds.radial <- predict(svm.radial, newdata = sub)

subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.radial)))


fwrite(x = subm, "svm_radial.csv")

# Create class.weights based on proportion of Survived
class.weights <- table(dat[, Survived])

class.weights <- class.weights / 891

# Radial SVM gamma = .1, cost = 1 and class weights
svm.radial <- svm(Survived ~ Pclass + Sex + Cabin + fare.median + age.median +
                    Parch + SibSp,
                  data = dat, kernel = "radial", cost = 1, gamma = .1, 
                  class.weights = class.weights) 

# Create predictions
preds.radial <- predict(svm.radial, newdata = sub)

subm <- data.table("PassengerId" = sub[, PassengerId],
                   "Survived" = as.integer(as.character(preds.radial)))

# Now let's do the same to find an optimal gamma and cost 
svm.radial <- svm.radial <-tune(svm, Survived ~ Pclass + Sex + Cabin + fare.median + age.median +
                                  Parch + SibSp, class.weights = class.weights,
                                data = dat, kernel = "radial",
                                ranges = list(gamma = c(.0001, .001, .01, .1, 1, 10, 100, 1000),
                                              cost = c(.0001, .001, .01, .1, 1, 10, 100, 1000)))


                    