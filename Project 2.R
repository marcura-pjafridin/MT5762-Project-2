setwd("C:/Users/Owner/Desktop/MT5762/Projects & Assignments/Project 2")
library(readxl)
library(dplyr)
library(tidyverse)
library(car)

#Load data
BabiesData <- readxl::read_excel("babies23.xlsx")

# Summary
summary(BabiesData)

# Checking the data types 
str(BabiesData)

# y variable
response_df <- BabiesData['wt...7']

# x variable
predictors_df <- BabiesData[, !names(BabiesData) %in% "wt...7"]

head(BabiesData)

# factor the categorical variables
cols <- c("parity", "race", "ed", "drace", "ded", "marital", "inc", "smoke", "time", "number")

BabiesData[,cols] <- data.frame(apply(BabiesData[cols], 2, as.factor))

# Create a full model 
FullModel <- lm(wt...7 ~ ., data = BabiesData)
formula(FullModel)
summary(FullModel)
anova(FullModel)

#Forward Selection
FirstMod <- lm(wt...7 ~ 1, data = BabiesData)

summary(FirstMod)

step(FirstMod, direction = "forward", scope = formula(FullModel))

# The final model is wt...7 ~ smoke + drace + ht + number + date + id + time + gestation
# AIC = 7023.91
ForMod <- lm(formula = wt...7 ~ smoke + drace + ht + number + date + id + time + gestation, data = BabiesData)

anova(ForMod)

#Backward Selection

step(FullModel, direction = "backward")

#The final model is wt...7 ~  id + date + race + ht + time + number
#AIC = 7027.35

BackMod <- lm(formula = wt...7 ~  id + date + race + ht + time + number, data = BabiesData)
anova(BackMod)

#Forward & Backward Selection 

step(FullModel, direction = "both")

#The final model is wt...7 ~ id + date + race + ht + time + number
#AIC = 7027.25
StepMod <- lm(wt...7 ~ id + date + race + ht + time + number, data = BabiesData)
anova(StepMod)

# Since the forward selection shows the lowest AIC, the model is selected. 

# Checking the normality of the model
qqnorm(resid(ForMod)) + qqline(resid(ForMod))
shapiro.test(resid(ForMod))

# Check the extreme residuals
bigResid <- which(abs(resid(ForMod))>5)
BabiesData[bigResid,]
hist(resid(ForMod))

ForResid <- resid(ForMod)
plot(fitted(ForMod), ForResid, ylab = 'residuals', xlab = 'Fitted values')

# p = 0.071311. The variance of the residuals are assumed to be constant (i.e. independent) over the values of the response (fitted values)

ncvTest(ForMod)

# Checking the autocorrelation of disturbances 

durbinWatsonTest(ForMod)

plot(ForMod, which = 1:2)

# Since there are NAs in the coefficient in the model, we remove some of the variables. 
# The updated model: wt...7 ~ smoke + drace + ht + date + id + gestation

UpdatedForMod <- lm(formula = wt...7 ~ smoke + drace + ht + date + id + gestation, data = BabiesData)

# Checking for multicollinearity 
vif(UpdatedForMod)
# All variables has GVF < 10
# The variables are all multicollinear