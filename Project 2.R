library(readxl)
library(dplyr)
library(tidyverse)
library(car)
library(bootstrap)

# ----------------------------------------- DATA CLEANING -------------------------------------------- ##
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

# Delete outliers certain columns
BabiesData <- subset(BabiesData, race < 10  & drace < 10 & smoke < 9 & ed < 9 
                     & ded < 9, select = id:number)

# Split data into train & validation set: 80% and 20%

set.seed(1000)
ModelData <- sample(nrow(BabiesData), nrow(BabiesData) * 0.8, replace = FALSE)

Validata <- BabiesData[-ModelData, ]

ModelData <- BabiesData[ModelData, ]

ModelData <- na.omit(ModelData)
summary(ModelData)

# factor the categorical variables
cols <- c("parity", "race", "ed", "drace", "ded", "marital", "inc", "smoke", "time", "number")

ModelData[,cols] <- data.frame(apply(ModelData[cols], 2, as.factor))

# ----------------------------------------- MODEL SELECTION -------------------------------------------- ##
# y variable
response_df <- ModelData['wt...7']

# x variable
predictors_df <- ModelData[, !names(ModelData) %in% "wt...7"]

head(ModelData)

# Create a full model 
FullModel <- lm(wt...7 ~ ., data = ModelData)
formula(FullModel)
summary(FullModel)
anova(FullModel)

#Forward Selection
FirstMod <- lm(wt...7 ~ 1, data = ModelData)

summary(FirstMod)

step(FirstMod, direction = "forward", scope = formula(FullModel))

# The final model is wt...7 ~ gestation + smoke + ht + drace + parity + dht + id
# AIC = 2570.89

ForMod <- lm(formula = wt...7 ~ gestation + smoke + ht + drace + parity + dht + id, data = ModelData)

anova(ForMod)

summary(ForMod)
## Adjusted R-squared = 0.3216, Multiple R-squared:  0.3591

#Backward Selection
step(FullModel, direction = "backward")

#The final model is wt...7 ~ id + gestation + parity + ht + drace + dht + time
#AIC = 2575.5

BackMod <- lm(formula = wt...7 ~ id + gestation + parity + ht + drace + dht + time, data = ModelData)

anova(BackMod)

summary(BackMod)
## Adjusted R-squared:  0.323, Multiple R-squared:  0.3691

#Forward & Backward Selection 
step(FullModel, direction = "both")

#The final model is wt...7 ~ id + gestation + parity + ht + drace + dht + time
#AIC = 2575.5
StepMod <- lm(wt...7 ~  id + gestation + parity + ht + drace + dht + time, data = ModelData)
anova(StepMod)

summary(StepMod)
## Adjusted R-squared:  0.323, Multiple R-squared:  0.3691
## Since the forward selection shows the lowest AIC, the model is selected. 

# ----------------------------------------- MODEL ASSUMPTIONS ------------------------------------------ ##

#FORWARD MODEL
# Checking the normality of the model
qqnorm(resid(ForMod))

qqline(resid(ForMod))

shapiro.test(resid(ForMod))

## the p-value = 0.5073, the distribution is normally distributed. 

# Check the extreme residuals
bigResid <- which(abs(resid(ForMod))>5)

ModelData[bigResid,]

hist(resid(ForMod))

ForResid <- resid(ForMod)

plot(fitted(ForMod), ForResid, ylab = 'residuals', xlab = 'Fitted values')

# Checking the variance of the residuals 
lmtest::bptest(ForMod)

ncvTest(ForMod)

## Breusch-Pagan Test p-value = 0.3148
## Variance Score Test p-value = 0.63857
## The variance of the residuals are assumed to be constant (i.e. independent)
## over the values of the response (fitted values)

# Checking the autocorrelation of disturbances 
durbinWatsonTest(ForMod)

## p = 0.59, hence the errors are not correlated. 

plot(ForMod, which = 1:2)

# Checking for multicollinearity 
vif(ForMod)

## All variables has GVF < 10
## The variables are all multicollinear

#STEP MODEL
# Checking the normality of the model
qqnorm(resid(StepMod))

qqline(resid(StepMod))

shapiro.test(resid(StepMod))

## the p-value = 0.554, the distribution is normally distributed. 

# Check the extreme residuals
bigResid2 <- which(abs(resid(StepMod))>5)

ModelData[bigResid2,]

hist(resid(StepMod))

ForResid2 <- resid(StepMod)

plot(fitted(StepMod), ForResid2, ylab = 'residuals', xlab = 'Fitted values')

# Checking the variance of the residuals 
lmtest::bptest(StepMod)

ncvTest(StepMod)

## Breusch-Pagan Test p-value = 0.537
## Variance Score Test p-value = 0.70392
## The variance of the residuals are assumed to be constant (i.e. independent)
## over the values of the response (fitted values)

# Checking the autocorrelation of disturbances 
durbinWatsonTest(StepMod)

## p = 0.646, hence the errors are not correlated. 

plot(StepMod, which = 1:2)

# Checking for multicollinearity 
vif(StepMod)

## All variables has GVF < 10
## The variables are all multicollinear

# Interactions between the variables 
interactionModel <- lm(wt...7 ~ gestation + smoke + ht + drace + parity + dht 
                       + id + gestation * smoke, data = ModelData)

summary(interactionModel)
## Gestation and smoke have interactions between each other

# ----------------------------------------- VALIDATION & MSE -------------------------------------------- ##

# validation datasets and Mean Square Error (MSE) and hold out a random 20% for this purpose
# set the random 20% validation dataset

# set a MSE matrix
MSE1 <- matrix(data = rep(0), nrow = 1, ncol = 2)

colnames(MSE1) <- c('formod', 'stepmod')

# choose two models: formod & stepmod
# FORWARD MODEL
response_formod <- Validata %>% select(wt...7)

predictors_formod <- Validata %>%
  select(gestation, drace, number, ht, id, date, dwt,
           dage)

pred_response_formod <- predict(ForMod, newdata = predictors_formod, se = T)

response_formod$pred <- pred_response_formod$fit

# calculate updateformodel's MSE
MSE1[1, 1] <- mean((response_formod$wt...7 - response_formod$pred)^2)

# STEP MODEL
response_stepmod <- Validata %>% select(wt...7)

predictors_stepmod <- Validata %>% 
  select( id, date, gestation, ht, drace, dage, dwt, number)

pred_response_stepmod <- predict(StepMod, newdata = predictors_stepmod, se = T)

response_stepmod$pred <- pred_response_stepmod$fit

# calculate stepmodel's MSE
MSE1[1, 2] <- mean((response_stepmod$wt...7 - response_stepmod$pred)^2)

# The result of the MSE shows that the MSE of the stepmod is smaller,
# which seems to mean that it fits better

# 5-fold cross validation
MSE <- matrix(0, nrow = 2, ncol = 2)

colnames(MSE) <- c('formod', 'stepmod')

rownames(MSE) <- c('5-fold MSE', 'validetion-dataset MSE')

MSE[2, ] <- MSE1[1, ]

k_fold <- function(fit, k = 5, x, y){
  set.seed(123)
  require(bootstrap)
  theta.fit <- function(x, y){lsfit(x, y)}
  theta.predict <- function(fit, x){cbind(1, x)%*%as.matrix(fit$coefficients)}
  
  results <- crossval(x, y, theta.fit, theta.predict, ngroup = k)
  #set up a matrix to put in the response
  response <- matrix(rep(0), nrow = nrow(x), ncol = 2)
  colnames(response) <- c('obser', 'pred')
  response[ , 1] <- fit$model[ , 1]
  #The cross-validated fit for each observation
  response[ , 2] <- results$cv.fit
  #calculate the MSE
  MSE <- mean((response[ , 1] - response[ , 2])^2)
  cat(k, "Fold Cross-Validated MSE = ", MSE, "\n")
  return(MSE)
}

# FORWARD MODEL
x_formod <- ForMod$model[ , 2:ncol(ForMod$model)]

y_formod <- ForMod$model[ , 1]

# calculate the 5-fold value of the updateformod
MSE[1, 1] <- k_fold(ForMod, k=5, x = x_formod, y = y_formod)  

# STEP MODEL
# spread the model data to fit the matrix calculation
x_stepmod <- StepMod$model[ , 2:ncol(StepMod$model)]

y_stepmod <- StepMod$model[ , 1]

# calculate the 5-fold value of the updateformod
MSE[1, 2] <- k_fold(StepMod, k=5, x = x_stepmod, y = y_stepmod)  

## The result show that the result of stepmod is a bit better
