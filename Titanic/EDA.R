library(data.table)
library(ggplot2)

# Set working directory
setwd("c:/mystuff/datascience/kaggle/titanic")

# Read in data
test <- read.csv("test.csv")
train <- read.csv("train.csv")
gender.sub <- read.csv("gender_submission.csv")

# First let's do some EDA
str(test)
summary(test)

# Now I want to look at a histogram of each numeric variable
# Plcass histogram
ggplot(test, aes(x = Pclass)) + 
  geom_histogram(bins = 3)

# Age
ggplot(test, aes(x = Age)) + 
  geom_histogram(bins = 20)

# Sibsp
ggplot(test, aes(x = SibSp)) + 
  geom_histogram(bins = 10)


