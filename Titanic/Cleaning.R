library(data.table)

# This script will be used as the basic cleaning script for all my analyses 
# on the Titanic dataset. 


# Set directory
setwd("C:/MyStuff/DataScience/Projects/Kaggle/Titanic/")

# Read in data
train <- fread("train.csv", stringsAsFactors = FALSE)
test <- fread("test.csv", stringsAsFactors = FALSE)


# I want to change the Survived, and Pclass variables to factor now
train[, Survived := factor(Survived)]
train[, Pclass := factor(Pclass)]
train[, Sex := factor(Sex)]

# The Embarked variable can be dropped, as I found it unimportant in the EDA.R
# script
train[, Embarked := NULL]

# Now I will impute the missing Age values and create two new columns for imputations. 
# One will be a mean imputation and the other a median. 
ages <- train[, Age]
mean.ages <- mean(ages, na.rm = TRUE)
median.ages <- median(ages, na.rm = TRUE)


# Make age.median and age.mean column
train[, age.median := Age]
train[, age.mean := Age]

# Set NA values in these columns to respective statistic
train[is.na(age.mean), age.mean := mean.ages]
train[is.na(age.median), age.median := median.ages]

# Next I need to impute the one missing Fare value
fares <- train[, Fare]
mean.fares <- mean(fares, na.rm = TRUE)
median.fares <- median(fares, na.rm = TRUE)

# Make an age.mean and age.median column
train[, fare.mean := Fare]
train[, fare.median := Fare]

# Set the one NA value in these columns to their respective imputed values
train[is.na(fare.mean), fare.mean := mean.fares]
train[is.na(fare.median), fare.median := median.fares]


# Next I will impute the missing Cabin values, and I will say anyone who is missing
# a Cabin didn't have a Cabin. This is a huge topic of debate in this dataset, 
# and deeper analysis into this problem would require a much more thoughtful 
# imputation of these values. I'm just doing this for now.
train[which(grepl(".", Cabin)), Cabin := "Cabin"]
train[-which(grepl(".", Cabin)), Cabin := "No Cabin"]


#################### Clean the test dataset ####################################

# I want to change the Survived, and Pclass variables to factor now

test[, Pclass := factor(Pclass)]
test[, Sex := factor(Sex)]

# The Embarked variable can be dropped, as I found it unimportant in the EDA.R
# script
test[, Embarked := NULL]

# Now I will impute the missing Age values and create two new columns for imputations. 
# One will be a mean imputation and the other a median. 
ages <- test[, Age]
mean.ages <- mean(ages, na.rm = TRUE)
median.ages <- median(ages, na.rm = TRUE)


# Make age.median and age.mean column
test[, age.median := Age]
test[, age.mean := Age]

# Set NA values in these columns to respective statistic
test[is.na(age.mean), age.mean := mean.ages]
test[is.na(age.median), age.median := median.ages]

# Next I need to impute the one missing Fare value
fares <- test[, Fare]
mean.fares <- mean(fares, na.rm = TRUE)
median.fares <- median(fares, na.rm = TRUE)

# Make an age.mean and age.median column
test[, fare.mean := Fare]
test[, fare.median := Fare]

# Set the one NA value in these columns to their respective imputed values
test[is.na(fare.mean), fare.mean := mean.fares]
test[is.na(fare.median), fare.median := median.fares]

# Next I will impute the missing Cabin values, and I will say anyone who is missing
# a Cabin didn't have a Cabin. This is a huge topic of debate in this dataset, 
# and deeper analysis into this problem would require a much more thoughtful 
# imputation of these values. I'm just doing this for now.
test[which(grepl(".", Cabin)), Cabin := "Cabin"]
test[-which(grepl(".", Cabin)), Cabin := "No Cabin"]


