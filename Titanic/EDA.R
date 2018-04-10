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
# variables. Let's start with Fare EDA

# Density plot
ggplot(train, aes(x = Fare)) +
  geom_density()

# Density plot by Pclass
ggplot(train, aes(x = Fare)) + 
  geom_density() +
  facet_wrap(~Pclass)

# Density plot where the color is mapped to the Survived variable 
ggplot(train, aes(x = Fare, color = Survived)) +
  geom_density()
  
# Similar plot as above, except breaking the training set into three data.tables
# based on Pclass
p.class1 <- train[Pclass == "1"]
p.class2 <- train[Pclass == "2"]
p.class3 <- train[Pclass == "3"]

# Density plot where the color is mapped to the Survived variable for p.class1
ggplot(p.class1, aes(x = Fare, color = Survived)) +
  geom_density()

# Density plot where the color is mapped to the Survived variable for p.class2
ggplot(p.class2, aes(x = Fare, color = Survived)) +
  geom_density()

# Density plot where the color is mapped to the Survived variable for p.class3
ggplot(p.class3, aes(x = Fare, color = Survived)) +
  geom_density()

# From these three plots it looks like there was a slight bias against people 
# who paid less in Fare, especially in Class 1 and 3

# Jittered scatterplot of Survived vs Fare.
ggplot(train, aes(x = Fare, y = Survived, color = Survived)) +
  geom_point(position = "jitter", alpha = .5)  

# Calculate the median of Fare based on response classes
surv.median <- median(train[Survived == "1"][, Fare])
no.surv.median <- median(train[Survived == "0"][, Fare])

# Same chart as above with both median lines plotted with different x limits 
# to help visualize the bulk of the data. 
ggplot(train, aes(x = Fare, y = Survived, color = Survived)) +
  geom_point(position = "jitter", alpha = .5) +
  geom_vline(xintercept = surv.median) +
  geom_vline(xintercept = no.surv.median, color = "blue") +
  xlim(c(0, 150))
  
# This plot revists the density plot of each Survived classes vs Fare, except 
# this plot has the no.surv.median vertical line plotted
ggplot(train, aes(x = Fare, color = Survived)) +
  geom_density() +
  geom_vline(xintercept = no.surv.median) +
  xlim(c(0, 150))

# Both of these plots definitely show a bias against low Fares. In essence, more of
# the data is located in the lower range of Fare values for passengers that didn't
# survive. Let's recreate both of these with the 3rd quantile to get another look 
# at this phenomenon

surv.third <- quantile(train[Survived == "1"][, Fare])[4]
no.surv.third <- quantile(train[Survived == "0"][, Fare])[4]


# This plot revists the density plot of each Survived classes vs Fare, except 
# this plot has the no.surv.median vertical line plotted
ggplot(train, aes(x = Fare, color = Survived)) +
  geom_density() +
  geom_vline(xintercept = no.surv.third, color = "blue") +
  geom_vline(xintercept = surv.third) +
  xlim(c(0, 150))

# Now let's look at the Sex variable
ggplot(train, aes(x = Sex)) +
  geom_bar() +
  facet_wrap(~Survived)
  
# From this we can see many many more men died than women and almost twice 
# as many women survived as men. This will definitely be an important predictor
# in our model.

# Next let's look at age. I'll start with some simple density plots
ggplot(train, aes(x = Age)) +
  geom_density()

# Next I'll overlay two densities based on response classes
ggplot(train, aes(x = Age, color = Survived)) +
  geom_density()

# Here there isn't as much of a different like we saw in the Fare density plots. 
# However, there is a lot of data missing for the Age values. I'm going to try 
# to impute some of these values and see what it does.
ages <- train[, Age]
mean.ages <- mean(ages, na.rm = TRUE)
median.ages <- median(ages, na.rm = TRUE)

# Make age.median and age.mean column
train[, age.median := Age]
train[, age.mean := Age]

# Set NA values in these columns to respective statistic
train[is.na(age.mean), age.mean := mean.ages]
train[is.na(age.median), age.median := median.ages]

# Now let's recreate the visualization and see what we come up with
# Next I'll overlay two densities based on response classes
ggplot(train, aes(x = age.mean, color = Survived)) +
  geom_density()

# Next I'll overlay two densities based on response classes
ggplot(train, aes(x = age.median, color = Survived)) +
  geom_density()

# Based on this there really isn't much of a difference in the between the two 
# resposnes based on Age. I'll still include it in my models, because it may 
# interact with some of the other variables differently and end up being significant. 
# I'll also do one more quick visualization where I facet by gender and see how
# this plot differs
ggplot(train, aes(x = age.median, color = Survived)) +
  geom_density() +
  facet_wrap(~Sex)

# Now I'll look at a jittered scatterplot like I did with the Fare variable
ggplot(train, aes(x = Age, y = Survived, color = Survived)) +
  geom_point(position = "jitter")

# Next I'll look at the relationship between SibSp and Survived
ggplot(train, aes(x = SibSp, fill = Survived)) +
  geom_bar(position = "fill") 

# I'll create the same plot with Parch
ggplot(train, aes(x = Parch, fill = Survived)) +
  geom_bar(position = "fill") 

# Density plots of Age with two overlayed densities for each response class, faceted
# by the SibSp variable
ggplot(train, aes(x = age.mean, color = Survived)) +
  geom_density() +
  facet_wrap(~SibSp)

# Finally I'll look at Pclass 
ggplot(train, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill")




# Bar chart of SibSp based on Pclass
ggplot(train, aes(x = Pclass)) 



