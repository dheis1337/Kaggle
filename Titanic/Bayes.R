library(naivebayes)
library(data.table)
library(ggplot2)
library(gmodels)


setwd("c:/mystuff/datascience/kaggle/titanic")

# Read in data
test <- fread("test.csv")
train <- fread("train.csv")
gender.sub <- fread("gender_submission.csv")

# Before I can do any EDA, I need to do some variable type conversion
surv <- train[, 2]
train <- train[, -2]
dat <- rbind(train, test)

train[, Pclass := factor(Pclass)]
train[, Sex := factor(Sex)]
test[, Pclass := factor(Pclass)]
test[, Sex := factor(Sex)]


# I'm going to run a Naive Bayes model on the data to predict survival. In order to 
# do so, I'm going to use the Sex, Pclass, Age, and Fare variables. Since the 
# Age and Fare variables are numeric, I need to bin them and turn them into factors. 
# From the EDA.R script, I determined that the binwidth should be 10, so I'm going to 
# reassign values to fall into a range of 10 years. 
dat[, Age1 := as.character("")]
dat[Age >= 0 & Age <= 9]$Age1 <- "0-9"
dat[Age >= 10 & Age <= 19]$Age1 <- "10-19"
dat[Age >= 20 & Age <= 29]$Age1 <- "20-29"
dat[Age >= 30 & Age <= 39]$Age1 <- "30-39"
dat[Age >= 40 & Age <= 49]$Age1 <- "40-49"
dat[Age >= 50 & Age <= 59]$Age1 <- "50-59"
dat[Age >= 60 & Age <= 69]$Age1 <- "60-69"
dat[Age >= 70 & Age <= 79]$Age1 <- "70-79"
dat[Age >= 80 & Age <= 89]$Age1 <- "80-89"
dat[Age >= 90 & Age <= 99]$Age1 <- "90-99"
dat[is.na(Age)]$Age1 <- NA
dat[, Age1 := factor(Age1)]

# From the EDA.R script, I determined that the a suitable bindwidth should be 25
dat[, Fare1 := as.character("")]
dat[Fare >= 0 & Fare <= 24]$Fare1 <- "0-24"
dat[Fare > 24 & Fare <= 49]$Fare1 <- "25-49"
dat[Fare > 49 & Fare <= 74]$Fare1 <- "50-74"
dat[Fare > 74  & Fare <= 99]$Fare1 <- "75-99"
dat[Fare > 99 & Fare <= 124]$Fare1 <- "100-124"
dat[Fare > 124 & Fare <= 149]$Fare1 <- "125-149"
dat[Fare > 149 & Fare <= 174]$Fare1 <- "150-174"
dat[Fare > 174 & Fare <= 199]$Fare1 <- "175-199"
dat[Fare > 199 & Fare <= 224]$Fare1 <- "200-224"
dat[Fare > 224 & Fare <= 249]$Fare1 <- "225-249"
dat[Fare > 249 & Fare <= 274]$Fare1 <- "250-274"
dat[Fare > 274 & Fare <= 299]$Fare1 <- "275-299"
dat[Fare > 300]$Fare1 <- ">300"
dat[, Fare1 := factor(Fare1)]

# Now I'm going to drop all the columns I'm not using
dat[, c("PassengerId", "Name", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Age",
          "Embarked") := NULL]

test <- dat[1:418]
train <- dat[419:1309]
train <- cbind(train, surv)
train[, Survived := factor(Survived)]

# Finally, I'm going to change the Survived variable from 0-1 encoding to decoded terms
set(test, i = which(test$Survived == 0), j = 1L, value = "Didn't Survive")
set(test, i = which(test$Survived == 1), j = 1L, value = "Survived")
test[, Survived := factor(Survived)]

test[, c("PassengerId", "Name", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Age",
          "Embarked") := NULL]


naive_bayes(x = train[, -1], y = train[, 1])

mod <- naive_bayes(formula = Survived ~ Pclass + Sex + Age1 + Fare1, data = train)

mod <- naiveBayes(formula = Survived ~ Pclass + Sex + Age1 + Fare1, data = train)

preds <- predict(mod, test)
preds <- as.character(preds)
preds <- as.numeric(preds)

sub <- data.frame(PassengerId = gender.sub$PassengerId, Survived = preds)


write.csv(x = sub, file = "NaiveBayes.csv", row.names = FALSE)
