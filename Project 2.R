library(readxl)
library(dplyr)
library(tidyverse)
library(car)

# Load data
BabiesData <- readxl::read_excel("babies23.xlsx")

# Summary
summary(BabiesData)

# Checking the data types 
str(BabiesData)

# Change outliers to NA
BabiesData <- replace(BabiesData, BabiesData == 99 | BabiesData == 999 | BabiesData == 98, NA)

# Remove NAs
BabiesData <- na.omit(BabiesData)

# factor the categorical variables
cols <- c("parity", "race", "ed", "drace", "ded", "marital", "inc", "smoke", "time", "number")

ModelData[,cols] <- data.frame(apply(ModelData[cols], 2, as.factor))

# Sample 80%

set.seed(123)
ModelData <- sample(nrow(BabiesData), nrow(BabiesData) * 0.8, replace = FALSE)

ModelData <- BabiesData[ModelData, ]

ModelData <- na.omit(ModelData)
summary(ModelData)

# y variable
response_df <- ModelData['wt...7']

# x variable
predictors_df <- ModelData[, !names(ModelData) %in% "wt...7"]

head(ModelData)

# Interactions between the variables 
interactionModel <- lm(wt...7 ~ smoke * gestation, data = ModelData)
summary(interactionModel)

## Gestation and smoke have interactions between each other. 

# Create a full model 
FullModel <- lm(wt...7 ~ ., data = ModelData)
formula(FullModel)
summary(FullModel)
anova(FullModel)

#Forward Selection
FirstMod <- lm(wt...7 ~ 1, data = ModelData)

summary(FirstMod)

step(FirstMod, direction = "forward", scope = formula(FullModel))

# The final model is wt...7 ~ gestation + ht + number + drace + dwt + id + parity + smoke
# AIC = 2620.68
ForMod <- lm(formula = wt...7 ~ gestation + ht + number + drace + dwt + id + parity + 
               smoke, data = ModelData)

anova(ForMod)

summary(ForMod)
## Adjusted R-squared = 0.2932, Multiple R-squared:  0.3182

#Backward Selection

step(FullModel, direction = "backward")

#The final model is wt...7 ~  id + gestation + age + ht + drace + dwt + time
#AIC = 2628.45

BackMod <- lm(formula = wt...7 ~  id + gestation + age + ht + drace + dwt + time, data = ModelData)

anova(BackMod)

summary(BackMod)
## Adjusted R-squared:  0.2933, Multiple R-squared:  0.3288

#Forward & Backward Selection 

step(FullModel, direction = "both")

#The final model is wt...7 ~ id + gestation + age + ht + drace + dwt + time
#AIC = 2628.45
StepMod <- lm(wt...7 ~  id + gestation + age + ht + drace + dwt + time, data = ModelData)
anova(StepMod)

summary(StepMod)
## Adjusted R-squared:  0.2933, Multiple R-squared:  0.3288
## Since the forward selection shows the lowest AIC, the model is selected. 

# MODEL ASSUMPTIONS / DIAGNOSTICS

# Checking the normality of the model
qqnorm(resid(ForMod)) + qqline(resid(ForMod))
shapiro.test(resid(ForMod))

## the p-value = 0.3998, the distribution is normally distributed. 

# Check the extreme residuals
bigResid <- which(abs(resid(ForMod))>5)
ModelData[bigResid,]
hist(resid(ForMod))

ForResid <- resid(ForMod)
plot(fitted(ForMod), ForResid, ylab = 'residuals', xlab = 'Fitted values')

# Checking the variance of the residuals 

lmtest::bptest(ForMod)
## p = 0.1532. The variance of the residuals are assumed to be constant (i.e. independent) over the values of the response (fitted values)

# Checking the autocorrelation of disturbances 

durbinWatsonTest(ForMod)

## p = 0.728, hence the errors are not correlated. 

plot(ForMod, which = 1:2)

# Checking for multicollinearity 
vif(ForMod)

## All variables has GVF < 10
## The variables are all multicollinear
