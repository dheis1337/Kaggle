library(data.table)
library(ggplot2)
library(ISLR)
library(MASS)

# This script is dedicated to modeling the Titanic dataset using logistic regression. 
# In this script, I will clean the data up a little, as well as conduct some 
# exploratory analysis before modeling the data. 

# Set directory
setwd("C:/MyStuff/DataScience/Projects/Kaggle/Titanic/")

# Read in data
train <- fread("train.csv", stringsAsFactors = FALSE)
test <- fread("test.csv", stringsAsFactors = FALSE)


# Look at basic structure of data 
str(train)

# I want to change the Survived, and Pclass variables to factor now
train[, Survived := factor(Survived)]
train[, Pclass := factor(Pclass)]


# It looks like there is some data missing from the Cabin variable. Let's take a look
train[, Cabin]

# After doing a little research on the internet (https://www.encyclopedia-titanica.org/cabins.html), 
# I found that cabin allocations is actually a bit of a mystery. There really wasn't
# a hard-and-fast record for who was in what cabin on-shore, i.e. the data came from
# a listing that was recovered from someone on the boat; this record was actually 
# incomplete to begin with. That said, I think the Cabin variable might be dicey
# to use. Before completely throwing it out of the analysis, I'm going to see if
# there is any kind of patterns related to who had a Cabin, who didn't and some 
# of the other variables, with respect to the Survive value of the passengers. If
# I can find some patterns, I'll consider using the Cabin variable in my analysis.
# Before I can do this, I need to convert all the Cabin values of "" to "No Cabin"
# and the ones with values to "Cabin".
train[which(grepl(".", Cabin)), Cabin := "Cabin"]
train[-which(grepl(".", Cabin)), Cabin := "No Cabin"]

# Now I can do some visualizations around the Cabin variable with respect to Survive
# variable. Let's start with a basic bar chart of the number of passengers that
# Survived based on their Cabin status
ggplot(train, aes(x= Cabin, fill = Survived)) +
  geom_bar(position = "fill")  

# From the visualization it looks like there is a firm difference between the Survived
# variable based on whether or not the passenger had a Cabin. Let's look at the difference
# in proprortions
nrow(train[which(Cabin == "Cabin" & Survived == 1)]) / nrow(train[which(Cabin == "Cabin")]) -
  nrow(train[which(Cabin == "No Cabin" & Survived == 1)]) / nrow(train[which(Cabin == "No Cabin")]) 
  
# It looks like there is about a 36% difference between the passengers with Cabin values
# and those with No Cabin values. This does make me think the Cabin variable will 
# be useful. That said, since there is a lot of confusion and doubt around the 
# data itself, I'm not going to try to impute the missing data in any special fashion 
# in my basic analysis. This might be an area forimproving them model if I get poor
# results after a few iterations. Next, I'll work on some more EDA for the other 
# variables. 
