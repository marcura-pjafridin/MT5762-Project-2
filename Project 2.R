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
summary(ForMod)
anova(ForMod)
plot(ForMod)
#Backward Selection

step(FullModel, direction = "backward")

#The final model is wt...7 ~  id + date + race + ht + time + number
#AIC = 7027.35

BackMod <- lm(formula = wt...7 ~  id + date + race + ht + time + number, data = BabiesData)
anova(BackMod)
summary(BackMod)
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
summary(UpdatedForMod)

# The second model is the updated stepmod removing 'number'
Updatedstepmod <- lm(formula = wt...7 ~ id + date + race + ht + time, data = BabiesData)
summary(Updatedstepmod)
# Checking for multicollinearity 
vif(UpdatedForMod)
# All variables has GVF < 10
# The variables are all multicollinear



##validation datasets and Mean Square Error (MSE) and hold out a random 20% for this purpose
#set the random 20% validation dataset
set.seed(1)
vali_data <- sample(nrow(BabiesData), nrow(BabiesData)*0.2, replace = F)
vali_babydata <- BabiesData[vali_data, ]

#set a MSE matrix
MSE1 <- matrix(data = rep(0), nrow = 1, ncol = 2)
colnames(MSE1) <- c('updateformod', 'updatestepmod')

#choose two models: updateformod & stepmod
#updateformod
response_updateformod <- vali_babydata %>% select(wt...7)
predictors_updateformod <- vali_babydata %>% select(smoke, drace, ht, date, id, gestation)
pred_response_updateformod <- predict(UpdatedForMod, newdata = predictors_updateformod, se = T)
response_updateformod$pred <- pred_response_updateformod$fit
#calculate updateformodel's MSE
MSE1[1, 1] <- mean((response_updateformod$wt...7 - response_updateformod$pred)^2)

#updatedstepmod
response_updatestepmod <- vali_babydata %>% select(wt...7)
predictors_updatestepmod <- vali_babydata %>% select(id, date, race, ht, time)
pred_response_updatestepmod <- predict(Updetedstepmod, newdata = predictors_updatestepmod, se = T)
response_updatestepmod$pred <- pred_response_updatestepmod$fit
#calculate stepmodel's MSE
MSE1[1, 2] <- mean((response_updatestepmod$wt...7 - response_updatestepmod$pred)^2)
#The result of the MSE shows that the MSE of the stepmod is smaller, which seems to mean that it fits better

#5-fold cross validation
MSE <- matrix(0, nrow = 2, ncol = 2)
colnames(MSE) <- c('updateformod', 'stepmod')
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

#updateformod
#spread the model data to fit the matrix calculation
x_formod <- UpdatedForMod$model[ , 2:ncol(UpdatedForMod$model)]
y_formod <- UpdatedForMod$model[ , 1]
x_formod <- spread(x_formod, key = smoke, value = smoke, sep = '')
x_formod <- spread(x_formod, key = drace, value = drace, sep = '')
x_formod <- x_formod %>% select(smoke1, smoke2, smoke3, smoke9, drace1, drace10, drace2, drace3, drace4, drace5, drace6, drace7, drace8,
                     drace9, drace99, ht, date, id, gestation)
x_formod[is.na(x_formod)] <- 0
for(i in 1:1236){
  if(x_formod$smoke2[i] == 2) x_formod$smoke2[i] <- 1
  if(x_formod$smoke3[i] == 3) x_formod$smoke3[i] <- 1
  if(x_formod$smoke9[i] == 9) x_formod$smoke9[i] <- 1
  if(x_formod$drace10[i] == 10) x_formod$drace10[i] <- 1
  if(x_formod$drace2[i] == 2) x_formod$drace2[i] <- 1
  if(x_formod$drace3[i] == 3) x_formod$drace3[i] <- 1
  if(x_formod$drace4[i] == 4) x_formod$drace4[i] <- 1
  if(x_formod$drace5[i] == 5) x_formod$drace5[i] <- 1
  if(x_formod$drace6[i] == 6) x_formod$drace6[i] <- 1
  if(x_formod$drace7[i] == 7) x_formod$drace7[i] <- 1
  if(x_formod$drace8[i] == 8) x_formod$drace8[i] <- 1
  if(x_formod$drace9[i] == 9) x_formod$drace9[i] <- 1
  if(x_formod$drace99[i] == 99) x_formod$drace99[i] <- 1
}
str(x_formod)
#transform the factor into numeric for calculating the matrix
cols_for <- c("smoke1", "smoke2", "smoke3", "smoke9", "drace1", "drace10", "drace2", "drace3",
          "drace4", "drace5", "drace6", "drace7", "drace8", "drace9", "drace99")
x_formod[,cols_for] <- data.frame(apply(x_formod[cols_for], 2, as.numeric))
str(x_formod)
x_formod <- as.matrix(x_formod)
y_formod <- as.matrix(y_formod)
#calculate the 5-fold value of the updateformod
MSE[1, 1] <- k_fold(UpdatedForMod, k=5, x = x_formod, y = y_formod)  


#stepmod
#spread the model data to fit the matrix calculation
x_stepmod <- Updatedstepmod$model[ , 2:ncol(Updatedstepmod$model)]
y_stepmod <- Updatedstepmod$model[ , 1]
x_stepmod <- spread(x_stepmod, key = race, value = race, sep = '')
x_stepmod <- spread(x_stepmod, key = time, value = time, sep = '')
x_stepmod <- x_stepmod %>% select(id, date, race1, race10, race2, race3,
                                  race4, race5, race6, race7, race8, race9, race99,
                                  ht, time1, time2, time3, time4, time5, time6,
                                  time7, time8, time9, time98, time99)
x_stepmod[is.na(x_stepmod)] <- 0
for(i in 1:1236){
  if(x_stepmod$race10[i] == 10) x_stepmod$race10[i] <- 1
  if(x_stepmod$race2[i] == 2) x_stepmod$race2[i] <- 1
  if(x_stepmod$race3[i] == 3) x_stepmod$race3[i] <- 1
  if(x_stepmod$race4[i] == 4) x_stepmod$race4[i] <- 1
  if(x_stepmod$race5[i] == 5) x_stepmod$race5[i] <- 1
  if(x_stepmod$race6[i] == 6) x_stepmod$race6[i] <- 1
  if(x_stepmod$race7[i] == 7) x_stepmod$race7[i] <- 1
  if(x_stepmod$race8[i] == 8) x_stepmod$race8[i] <- 1
  if(x_stepmod$race9[i] == 9) x_stepmod$race9[i] <- 1
  if(x_stepmod$race99[i] == 99) x_stepmod$race99[i] <- 1
  if(x_stepmod$time2[i] == 2) x_stepmod$time2[i] <- 1
  if(x_stepmod$time3[i] == 3) x_stepmod$time3[i] <- 1
  if(x_stepmod$time4[i] == 4) x_stepmod$time4[i] <- 1
  if(x_stepmod$time5[i] == 5) x_stepmod$time5[i] <- 1
  if(x_stepmod$time6[i] == 6) x_stepmod$time6[i] <- 1
  if(x_stepmod$time7[i] == 7) x_stepmod$time7[i] <- 1
  if(x_stepmod$time8[i] == 8) x_stepmod$time8[i] <- 1
  if(x_stepmod$time9[i] == 9) x_stepmod$time9[i] <- 1
  if(x_stepmod$time98[i] == 98) x_stepmod$time98[i] <- 1
  if(x_stepmod$time99[i] == 99) x_stepmod$time99[i] <- 1
}
str(x_stepmod)

#transform the factor into numeric for calculating the matrix
cols_stepmod <- c("race1", "race10", "race2", "race3", "race4", "race5", "race6", "race7",
          "race8", "race9", "race99", "time1", "time2", "time3", "time4", "time5",
          "time6", "time7", "time8", "time9", "time98", "time99")
x_stepmod[,cols_stepmod] <- data.frame(apply(x_stepmod[cols_stepmod], 2, as.numeric))
str(x_stepmod)
x_stepmod <- as.matrix(x_stepmod)
y_stepmod <- as.matrix(y_stepmod)
#calculate the 5-fold value of the updateformod
MSE[1, 2] <- k_fold(Updatedstepmod, k=5, x = x_stepmod, y = y_stepmod)  
#The result show that the updatedmodel is better because the value of 5-fold is much smaller