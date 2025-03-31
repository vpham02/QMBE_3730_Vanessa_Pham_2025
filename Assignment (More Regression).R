# Script Name: qbme 3730 Assignment (More regression )
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

install.packages("ggplot2")
library(ggplot2)

install.packages("geomtextpath")
library(geomtextpath)


#-----------------------------------------------------------------------
#Load Data

data <- read.csv('wages.csv')
view(data)

data2 <- read.csv('AnnArbor.csv')
view(data2)

#-----------------------------------------------------------------------
#Data Analysis

#Wages 
age <- data$Age
wage <- data$Wage
age_sq <- data$Age.2
educ <- data$Educ

#1a. scatterplot to capture the relationship 
ggplot(data, aes(x = age, y = wage)) +
  geom_point() +
  geom_labelsmooth(aes(label = 'Linear Trend'), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 6, linewidth = 2, boxlinewidth = 0.6) +
  theme_bw() + guides(color = 'none') 

#1b.Linear Muliple regression 
reg1 <- lm(wage ~ age + educ, data = data)
summary(reg1)

#1c. Quadratic muliple regression 
reg2 <- lm(wage~ age + age_sq + educ, data = data)
summary(reg2)

ggplot(data, aes(x = age, y = wage)) +
  geom_point() +
  geom_labelsmooth(aes(label = 'Quadratic Trend'), fill = "white",
                   method = "lm", formula = y ~ poly(x,2),
                   size = 6, linewidth = 2, boxlinewidth = 0.6) +
  theme_bw() + guides(color = 'none') 

#1d. Predicted hourly wages 

#Age 30 
-22.721936 + 1.350002*(30) + -0.013322*(30)^2 + 1.253959*(16)
#25.85167

#Age 50 
-22.721936 + 1.350002*(50) + -0.013322*(50)^2 + 1.253959*(16)
#31.53651

#Age 70
-22.721936 + 1.350002*(70) + -0.013322*(70)^2 + 1.253959*(16)
#26.56375

#AnnArbor 
rent <- data2$Rent
beds <- data2$Beds
baths <- data2$Baths
sqft <- data2$Sqft

#2a. Plot Rent against each variable 

#Beds
ggplot(data2, aes(x = beds, y = rent)) +
  geom_point() +
  geom_labelsmooth(aes(label = 'Bed Trend'), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 6, linewidth = 2, boxlinewidth = 0.6) +
  theme_bw() + guides(color = 'none') 

#Baths 
ggplot(data2, aes(x = baths, y = rent)) +
  geom_point() +
  geom_labelsmooth(aes(label = ' Bath Trend'), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 6, linewidth = 2, boxlinewidth = 0.6) +
  theme_bw() + guides(color = 'none') 

#Sqft 
ggplot(data2, aes(x = sqft, y = rent)) +
  geom_point() +
  geom_labelsmooth(aes(label = ' Sqft Trend'), fill = "white",
                   method = "lm", formula = y ~ x,
                   size = 6, linewidth = 2, boxlinewidth = 0.6) +
  theme_bw() + guides(color = 'none') 

#sqft log 
ggplot(data2, aes(x = sqft, y = rent)) +
  geom_point() +
  geom_labelsmooth(aes(label = ' Sqft Trend Log'), fill = "white",
                   method = "lm", formula = y ~ log(x),
                   size = 6, linewidth = 2, boxlinewidth = 0.6) +
  theme_bw() + guides(color = 'none') 

#sqft quad 
ggplot(data2, aes(x = sqft, y = rent)) +
  geom_point() +
  geom_labelsmooth(aes(label = ' Sqft Trend Quad'), fill = "white",
                   method = "lm", formula = y ~ poly(x,2),
                   size = 6, linewidth = 2, boxlinewidth = 0.6) +
  theme_bw() + guides(color = 'none') 

#2b. Multiple Regression 
reg_rent <- lm(rent~ beds + baths + log(sqft), data = data2)
summary(reg_rent)

-3909.74 + 131.78*3 + 36.43*2 + 675.26*log(1600)
