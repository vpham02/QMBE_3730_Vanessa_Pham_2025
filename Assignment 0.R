# Script Name: qbme Assignment 1
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

#-----------------------------------------------------------------------
#Load Data

data <- read.csv('netflix_titles.csv')
view(data)

#-----------------------------------------------------------------------
#Data Analysis

#I first looked at the summary to find how what kind of data set this is and 
#what kind of models we can run and how many rows and columns they have. 

summary(data)


#After running the rummary code, I found out that there are 12 columns and 
#almost all of them are charater or categorical except for the release year that
#is continuous I also found that there are 8807 rows which we can find as the length. 
#The only model that we are able to run is a classification because the independent 
#variables are both categorical and continous where the dependent variable is
#categorical. The columns are "show_id", "type", "title", "director", "cast", 
#"couontry", "date_added", "release_year", "rating", "duration", "listed_in",
#"description"

#In this section, I attempted to find the values of missing data in R but I am
#not sure why my coding is not working. I tried to make empty spaces as NA so 
#R can read them and count the missing values but it still is not working. 
#However, I went into excel and counted missing values which were 4307 in total.
#There are zero missing values under the column "show_id", 0 under "type", 0 
#under "title", 2634 under "director", 825 under "cast", 831 under "country"
#10 for "date_added", 0 for "release year", 4 for "rating", 3 for "duration"
#0 for "listed in", and finally 0 for discription 
data$country[data$country==" "] <- NA
data[!complete.cases(data),]

which(is.na(data$country))
sum(is.na(data))
is.na(data)

data_new <- na.omit(data)

#This is a bar plot of what kind of Netflix flims that are availble for the listed coutnries 
country_table <- table(data$country)
barplot(country_table, main = "Netflix Titles in Countries ", xlab = "Country Names")

#This is a bar plit of what type of Netflix Films, if they were either movies or TV shows 
type_table <- table(data$type)
barplot(type_table, main = "Netflix Film Type", xlab = "Film Type")

#This is a bar plot of what year the films not realeased 
year_table <- table(data$release_year)
barplot(year_table, main = "Netflix Film Release Year", xlab = "Year")
