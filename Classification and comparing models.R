# Script Name: Classification and comparing models
# 
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
library(tidyverse)
library(stargazer)

install.packages("wooldridge")
library(wooldridge)

install.packages("caret")
library(caret)

install.packages("randomForest")
library(randomForest)

install.packages("naniar")
library(naniar)

install.packages("gtExtras")
library(gtExtras)

install.packages("caTools")
library(caTools)

install.packages("dplyr")
library(dplyr)

library(ggplot2)

install.packages("pROC")
library(pROC)


#-----------------------------------------------------------------------
#Load Data

data <- read.csv('loan_default_data_set.csv')

#-----------------------------------------------------------------------
#Data Analysis

#1.EDA

Indicator <- as.factor(data_no_NA$Def_ind)
str(data)
View(data)
summary(data)
hist(data$Def_ind) 

#From seeing the structure of the data, I know that it has 20,000 observations and 21 variables. 
#We can use str and summary to find out more about the data set like the min, median, max, and mean in the summary
#The shape of the data is bernoulli

#2. Missing Values 
missing <- miss_var_summary(data) 

data_no_NA <- data %>%
  drop_na(pct_card_over_50_uti) %>%
  drop_na(rep_income)

#I first had to look at the missing values and I saw that only two of the variables had missing data which were 
#pct_card_over_50_uti and rep_income and they were both under 10% so I choose to drop the observations 
#I did that by using the drop_na funtion and will use the data_no_na for the rest of the assignments 

#3. Duplicates 

duplicated(data_no_NA)

data_no_NA[duplicated(data_no_NA)]

#It seems like there were no duplicates 

#4. For this dataset, I would not drop anything because we would only remove it if it was an accident and with this dataset, we can't tell 

#5. Plot Indicator of Default and credit card age
card_age <- data_no_NA$credit_card_age
Ind_Def <- data_no_NA$Def_ind

plot(Ind_Def ~ card_age , 
     xlab = "Credit Card Age", 
     ylab = "Indicator of Default")

#It seems like any cards before 50 and after 450 does not get defaulted however, all ages between can get a chance of being defaulted or not

#6. Education level 
edu_prop <- prop.table(table(data_no_NA$rep_education))
edu_prop

#The "others" degree was underrepresented. 

#7. Balance 
def_pro <- prop.table(table(data_no_NA$Def_ind))
def_pro

#It is not balanced. To help with this imbalanced, we can: over sample, random forest model, under sampling, both (over and under), or ROSE function

#8. Rep_income distribution 
hist(data_no_NA$rep_income)

#It seems symmetrical or approximately normal 

#9. 
view(data)

data_grp_region = data %>% group_by(rep_education)  %>%
  summarise(total_people = sum(Def_ind), 
            .groups = 'drop')

data_grp_region

#10, There is not much that stands out to me at this moment. We just know more information about the data and their distribution and relationship between each other 


#1. Seperate data into training and testing 
Ind_Def_1 <- as.factor(Ind_Def)

set.seed(2)

trainIndex <- createDataPartition(data_no_NA$Def_ind, p=0.8, list=FALSE)

View(train)

train <- data[trainIndex,]
test <- data[-trainIndex,]

train_drop <- train %>% na.omit()
test_drop <- test %>% na.omit()

knn_model <- train(Def_ind ~ ., data=train_drop, method='knn', tuneLength=5) # Fit KNN model

pred_knn <- predict(knn_model, test_drop)

summary(knn_model)

dt_model <- train(Def_ind ~ ., data=train_drop, method='rpart', tuneLength=5) # Fit Decision Tree Model

pred_dt <- predict(dt_model, test)

print(confusionMatrix(pred_dt, test$y_variable))


#The optimal model has the smalled RMSE which was k = 13. It's r-squared is 0.0349 which is not that high. However, if we look at the MAE, it is not the largest but still small. 
#As for the decision tree, they also use the RMSE to find the smallest value. The final used for the model was cp = 0.0137

#2. Matrix model 

pred_classes <- ifelse(pred_knn > 0.5, 1,0)

pred_classes <- as.factor(pred_classes)

pred_classs_1 <- as.factor(test_drop$Def_ind)

print(test_drop$Def_ind)

conf_matrix_2 <- confusionMatrix(pred_classes, pred_classs_1)

print(confusionMatrix(pred_classes, pred_classs_1))

print(pred_knn)

#We see that 4990 observations which are not defualted were correct identified and 560 observations which were predicted as Indicator of default was indentified incorrectly. 
#We see that 14 observation which were defaulted were correctly indenitifed and 24 observations that was marked as admited was inncorectly identified 

#3. ROC/AUC 
sim_roc <- roc(pred_classs_1, pred_knn, levels = c(0,1), direction = "<")

sim_roc

plot(sim_roc)

#4. 
#I think the most important feature to predict default status is RMSE becuase it chooses the optimal model by using the smallest value 

#https://www.datacamp.com/tutorial/k-nearest-neighbors-knn-classification-with-r-tutorial

 #5. 
#I think the decision tree has a better model because he has higher chance of defaulting. 


knn_model$results






