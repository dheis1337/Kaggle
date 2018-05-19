library(data.table)
library(MASS)


# This script will be used as the basic cleaning script for all my analyses 
# on the Titanic dataset. 


# Set directory
setwd("C:/MyStuff/DataScience/Projects/Kaggle/Titanic/")

# Read in data
dat <- fread("train.csv", stringsAsFactors = FALSE)
sub <- fread("test.csv", stringsAsFactors = FALSE)


# I want to change the Survived, and Pclass variables to factor now
dat[, Survived := factor(Survived)]
dat[, Pclass := factor(Pclass)]
dat[, Sex := factor(Sex)]

# The Embarked variable can be dropped, as I found it unimportant in the EDA.R
# script
dat[, Embarked := NULL]

# Now I will impute the missing Age values and create two new columns for imputations. 
# One will be a mean imputation and the other a median. 
ages <- dat[, Age]
mean.ages <- mean(ages, na.rm = TRUE)
median.ages <- median(ages, na.rm = TRUE)


# Make age.median and age.mean column
dat[, age.median := Age]
dat[, age.mean := Age]

# Set NA values in these columns to respective statistic
dat[is.na(age.mean), age.mean := mean.ages]
dat[is.na(age.median), age.median := median.ages]

# Make the observations with Fare 0 have a Fare of 1 for log transformation
set(dat, which(dat$Fare == 0), "Fare", 1)

# Next I need to impute the one missing Fare value
fares <- dat[, Fare]
mean.fares <- mean(fares, na.rm = TRUE)
median.fares <- median(fares, na.rm = TRUE)

# Make an age.mean and age.median column
dat[, fare.mean := Fare]
dat[, fare.median := Fare]

# Set the one NA value in these columns to their respective imputed values
dat[is.na(fare.mean), fare.mean := mean.fares]
dat[is.na(fare.median), fare.median := median.fares]


# Next I will impute the missing Cabin values, and I will say anyone who is missing
# a Cabin didn't have a Cabin. This is a huge topic of debate in this dataset, 
# and deeper analysis into this problem would require a much more thoughtful 
# imputation of these values. I'm just doing this for now.
dat[which(grepl(".", Cabin)), Cabin := "Cabin"]
dat[-which(grepl(".", Cabin)), Cabin := "No Cabin"]

dat[, Cabin := factor(Cabin)]

# log transform the Fare variable
dat[, fare_log := log(Fare)]
dat[, fare_log_10 := log(Fare, base = 10)]

# Create interaction terms
# log(Fare) and age
dat[, fare_age := fare_log * age.median]

#


#################### Clean the sub dataset ####################################

# I want to change the Survived, and Pclass variables to factor now

sub[, Pclass := factor(Pclass)]
sub[, Sex := factor(Sex)]

# The Embarked variable can be dropped, as I found it unimportant in the EDA.R
# script
sub[, Embarked := NULL]

# Now I will impute the missing Age values and create two new columns for imputations. 
# One will be a mean imputation and the other a median. 
ages <- sub[, Age]
mean.ages <- mean(ages, na.rm = TRUE)
median.ages <- median(ages, na.rm = TRUE)


# Make age.median and age.mean column
sub[, age.median := Age]
sub[, age.mean := Age]

# Set NA values in these columns to respective statistic
sub[is.na(age.mean), age.mean := mean.ages]
sub[is.na(age.median), age.median := median.ages]

# Make the observations with Fare 0 have a Fare of 1 for log transformation
set(sub, which(dat$Fare == 0), "Fare", 1)


# Next I need to impute the one missing Fare value
fares <- sub[, Fare]
mean.fares <- mean(fares, na.rm = TRUE)
median.fares <- median(fares, na.rm = TRUE)




# Make an age.mean and age.median column
sub[, fare.mean := Fare]
sub[, fare.median := Fare]

# Set the one NA value in these columns to their respective imputed values
sub[is.na(fare.mean), fare.mean := mean.fares]
sub[is.na(fare.median), fare.median := median.fares]

# Next I will impute the missing Cabin values, and I will say anyone who is missing
# a Cabin didn't have a Cabin. This is a huge topic of debate in this dataset, 
# and deeper analysis into this problem would require a much more thoughtful 
# imputation of these values. I'm just doing this for now.
sub[which(grepl(".", Cabin)), Cabin := "Cabin"]
sub[-which(grepl(".", Cabin)), Cabin := "No Cabin"]

sub[, Cabin := factor(Cabin)]


# log transform the Fare variable
sub[, fare_log := log(Fare)]
sub[, fare_log_10 := log(Fare, base = 10)]

# Create interaction terms
# log(Fare) and age
sub[, fare_age := fare_log * age.median]


# Let's create two new columns that will use the turn the 
# Parch variable into a binary variable
dat[, Parch_bin := ifelse(as.numeric(Parch) > 0, 1, 0)]
sub[, Parch_bin := ifelse(as.numeric(Parch) > 0, 1, 0)]


# Let's create a new predictor which makes the Pclass predictor binary based on 
# being in Pclass 1 or not
dat[, Pclass_bin := ifelse(as.numeric(Pclass == 1), 1, 0)]
sub[, Pclass_bin := ifelse(as.numeric(Pclass == 1), 1, 0)]


