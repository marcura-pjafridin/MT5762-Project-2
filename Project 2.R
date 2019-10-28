library(readxl)
library(dplyr)
library(tidyverse)
library(car)

# Load data
BabiesData <- readxl::read_excel("babies23.xlsx")

# Summary
summary(BabiesData)

# Change outliers to NA
BabiesData <- replace(BabiesData, BabiesData == 99 | BabiesData == 999 | BabiesData == 98, NA)

# Remove NAs
BabiesData <- na.omit(BabiesData)

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

# Interactions between the variables 
interactionModel <- lm(wt...7 ~ gestation * age , data = BabiesData)
summary(interactionModel)

## The variables have interactions between each other. 

# Create a full model 
FullModel <- lm(wt...7 ~ ., data = BabiesData)
formula(FullModel)
summary(FullModel)
anova(FullModel)

#Forward Selection
FirstMod <- lm(wt...7 ~ 1, data = BabiesData)

summary(FirstMod)

step(FirstMod, direction = "forward", scope = formula(FullModel))

# The final model is wt...7 ~ gestation + ht + smoke + drace + parity + dwt
# AIC = 3294.29
ForMod <- lm(formula = wt...7 ~ gestation + ht + smoke + drace + parity + dwt, data = BabiesData)

anova(ForMod)

#Backward Selection

step(FullModel, direction = "backward")

#The final model is wt...7 ~  gestation + parity + ht + drace + dwt + time
#AIC = 3298.81

BackMod <- lm(formula = wt...7 ~  gestation + parity + ht + drace + dwt + time, data = BabiesData)
anova(BackMod)

#Forward & Backward Selection 

step(FullModel, direction = "both")

#The final model is wt...7 ~ gestation + parity + ht + drace + dwt + time
#AIC = 3298.81
StepMod <- lm(wt...7 ~ gestation + parity + ht + drace + dwt + time, data = BabiesData)
anova(StepMod)

# Since the forward selection shows the lowest AIC, the model is selected. 

# MODEL ASSUMPTIONS / DIAGNOSTICS

# Checking the normality of the model
qqnorm(resid(ForMod)) + qqline(resid(ForMod))
shapiro.test(resid(ForMod))

## the p-value = 0.4376, the distribution is normally distributed. 

# Check the extreme residuals
bigResid <- which(abs(resid(ForMod))>5)
BabiesData[bigResid,]
hist(resid(ForMod))

ForResid <- resid(ForMod)
plot(fitted(ForMod), ForResid, ylab = 'residuals', xlab = 'Fitted values')

# Checking the variance of the residuals 

ncvTest(ForMod)

## p = 0.36394. The variance of the residuals are assumed to be constant (i.e. independent) over the values of the response (fitted values)

# Checking the autocorrelation of disturbances 

durbinWatsonTest(ForMod)

## p = 0.274, hence the errors are not correlated. 

plot(ForMod, which = 1:2)

# Checking for multicollinearity 
vif(ForMod)

## All variables has GVF < 10
## The variables are all multicollinear

