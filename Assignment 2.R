# Script Name: qbme Assignment 2
#   Purpose:
#   Author(s):
#   Date Created: 
#   
#   Notes:
#   
#
#
#-----------------------------------------------------------------------
# Working Directory 

getwd()
setwd("C:/Users/vanes/OneDrive/Desktop/QMBE_3730_Vanessa_Pham_2025/QMBE_3730_Vanessa_Pham_2025")

#-----------------------------------------------------------------------
#Libraries and Packages

install.packages("tidyverse")
install.packages("stargazer")
install.packages("caTools")
library(tidyverse)
library(stargazer)
install.packages("wooldridge")
library(wooldridge)
library(modelr)
library(broom)
library(caTools)


#-----------------------------------------------------------------------
#Load Data

data <- read.csv('admit.csv')
view(data)

#-----------------------------------------------------------------------
#Data Analysis

admit <- as.factor(data$admit)
view(admit)
summary(data)
str(data)

dim(data)
set.seed(1)

split <- sample.split(data$admit, SplitRatio = 0.7)
train_data_1 <- subset(data, split == TRUE)
test_data_1 <- subset(data, split == FALSE)
dim(train_data_1)
dim(test_data_1)

admit_eda <- glm(admit ~ gre + gpa + rank, data = train_data_1, family = "binomial")
summary(admit_eda)


exp(coef(admit_eda))

pred_admit <- predict(admit_eda, test_data_1, type = "response")
pred_admit_class <- ifelse(pred_admit > 0.5, 1, 0)
pred_admit_class <- as.factor(pred_admit_class)

print(pred_admit_class)

head(pred_admit)
head(pred_admit_class)
dataset<-do.call(rbind, Map(data.frame, pred_admit=pred_admit_class, admit=test_data_1$admit))
dim(dataset)

hist(data$gre, freq = FALSE)
lines(density(data$gre))
#The distribution is left-skewed 

#From what I understand, "0" is not admited and "1" is admited to graduate school. 
#After running the EDA regression holding all else equal, as Graduate Record Exam 
#scores increase by 1 unit, there is a increase 1.00230 odds of the individual being admited to graduate school 
#As GPA increase by 1 unit, there is a increase 2.17497 odds of the individual being admited to grauate school which doubles the odds
#As the rank of the undergraduate increases by 1 rank, there is a 0.57119 odds of the individual being admited to graduate school 
#As for the intercept, if all of the independant variables are 0, there is a 0.03176 odds of the individual being admited to gradute school 
#It is not balanced because 70% of people were not admitted and 30% of people were admitted


glance(admit_eda)

conf_matrix_1 <- table(Predicted = dataset$pred_admit, Actual = dataset$admit)
conf_matrix_1
admit_accuracy <- sum(diag(conf_matrix_1)) / sum(conf_matrix_1)
admit_accuracy #0.705 is the accuracy 

print(conf_matrix_1)

#We see that 253 observations which are not admited were correct identified and 98 observations which were predicted as admited was indentified incorrectly. 
#We see that 20 observation which were admited were correctly indenitifed and 29 observations that was marked as admited was inncorectly identified 

#I think the most important variable to predict admits are the rank of the undergraduate institution because it has the most significant out of all the variables. 
#it means that it is the most significant to the EDA and is highly correlated to admits. 

