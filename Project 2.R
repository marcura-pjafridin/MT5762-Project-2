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
                     & ded < 9, select = pluralty:number)

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

# The final model is wt...7 ~ gestation + smoke + ht + drace + parity + dht
# AIC = 2572.19

ForMod <- lm(formula = wt...7 ~ gestation + smoke + ht + drace + parity + dht, data = ModelData)

anova(ForMod)

summary(ForMod)
## Adjusted R-squared = 0.3184, Multiple R-squared: 0.3546

#Backward Selection
step(FullModel, direction = "backward")

#The final model is wt...7 ~ gestation + parity + ht + drace + dht + time
#AIC = 2576.63

BackMod <- lm(formula = wt...7 ~ gestation + parity + ht + drace + dht + time, data = ModelData)

anova(BackMod)

summary(BackMod)
## Adjusted R-squared:  0.32, Multiple R-squared:  0.3649

#Forward & Backward Selection 
step(FullModel, direction = "both")

#The final model is wt...7 ~ gestation + parity + ht + drace + dht + time
#AIC = 2576.63
StepMod <- lm(wt...7 ~ gestation + parity + ht + drace + dht + time, data = ModelData)
anova(StepMod)

summary(StepMod)
## Adjusted R-squared:  0.32, Multiple R-squared:  0.3649
## Since the forward selection shows the lowest AIC, the model is selected. 

# ------------------------------------- CHECK ASSUMPTIONS --------------------------------------- ##
###### ForMod ######
# checking linear relationship
par(mfrow=c(2,3))
termplot(ForMod,se=T,partial.resid = T)
## no curvature, so the relationship is linear

# checking if residuals normally distributed
par(mfrow=c(1,1))
plot(ForMod,2)
shapiro.test(resid(ForMod))
## from the normal Q-Q plot, the residuals are normally distributed
## p-value = 0.5441, so the resiudals are normally distributed

# checking the variance of the residuals 
ncvTest(ForMod)
## p-value = 0.64777, so the variance of residuals is constant

# checking the independence of residuals
durbinWatsonTest(ForMod)
## p-value > 0.05, rediduals are independent

# checking collinearity 
vif(ForMod)
## 1 < GVIFs < 2, no strong colinearity

###### StepMod ######
# checking linear relationship
par(mfrow=c(2,3))
termplot(StepMod,se=T,partial.resid = T)
## no curvature, so the relationship is linear

# checking if residuals normally distributed
par(mfrow=c(1,1))
plot(StepMod,2)
shapiro.test(resid(StepMod))
## from the normal Q-Q plot, the residuals are normally distributed
## p-value = 0.5127, so the resiudals are normally distributed

# checking the variance of the residuals 
ncvTest(StepMod)
## p-value = 0.79342, so the variance of residuals is constant

# checking the independence of residuals 
durbinWatsonTest(StepMod)
## p-value > 0.05, rediduals are independent

# checking collinearity 
vif(StepMod)
## 1 < GVIFs < 2, no strong colinearity

# ------------------------------------ BOTH PASS THE CHECKING ------------------------------------ ##

# ----------------------------------------- INTERACTION -------------------------------------------- ##

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
  select(gestation, smoke, ht, drace, parity, dht)

pred_response_formod <- predict(ForMod, newdata = predictors_formod, se = T)

response_formod$pred <- pred_response_formod$fit

# calculate updateformodel's MSE
MSE1[1, 1] <- mean((response_formod$wt...7 - response_formod$pred)^2)

# STEP MODEL
response_stepmod <- Validata %>% select(wt...7)

predictors_stepmod <- Validata %>% 
  select(gestation, parity, ht, drace, dht, time)

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
  response[ , 1] <- y
  #The cross-validated fit for each observation
  response[ , 2] <- results$cv.fit
  #calculate the MSE
  MSE <- mean((response[ , 1] - response[ , 2])^2)
  cat(k, "Fold Cross-Validated MSE = ", MSE, "\n")
  return(MSE)
}

# FORWARD MODEL
x_formod <- BabiesData %>% select(gestation, smoke, ht, drace, parity, dht)
y_formod <- BabiesData %>% select(wt...7)
x_formod <- spread(x_formod, key = smoke, value = smoke, sep = '')
x_formod <- spread(x_formod, key = drace, value = drace, sep = '')
x_formod <- spread(x_formod, key = parity, value = parity, sep = '')
x_formod <- x_formod %>% select(gestation, smoke1, smoke2, smoke3, ht, drace1, drace2, drace3, 
                                drace4, drace5, drace6, drace7, drace8, drace9,
                                parity1, parity10, parity11, parity2, parity3, 
                                parity4, parity5, parity6, parity7, parity9, dht)
x_formod[is.na(x_formod)] <- 0
for(i in 1:471){
  if(x_formod$smoke2[i] == 2) x_formod$smoke2[i] <- 1
  if(x_formod$smoke3[i] == 3) x_formod$smoke3[i] <- 1
  if(x_formod$drace2[i] == 2) x_formod$drace2[i] <- 1
  if(x_formod$drace3[i] == 3) x_formod$drace3[i] <- 1
  if(x_formod$drace4[i] == 4) x_formod$drace4[i] <- 1
  if(x_formod$drace5[i] == 5) x_formod$drace5[i] <- 1
  if(x_formod$drace6[i] == 6) x_formod$drace6[i] <- 1
  if(x_formod$drace7[i] == 7) x_formod$drace7[i] <- 1
  if(x_formod$drace8[i] == 8) x_formod$drace8[i] <- 1
  if(x_formod$drace9[i] == 9) x_formod$drace9[i] <- 1
  if(x_formod$parity10[i] == 10) x_formod$parity10[i] <- 1
  if(x_formod$parity11[i] == 11) x_formod$parity11[i] <- 1
  if(x_formod$parity2[i] == 2) x_formod$parity2[i] <- 1
  if(x_formod$parity3[i] == 3) x_formod$parity3[i] <- 1
  if(x_formod$parity4[i] == 4) x_formod$parity4[i] <- 1
  if(x_formod$parity5[i] == 5) x_formod$parity5[i] <- 1
  if(x_formod$parity6[i] == 6) x_formod$parity6[i] <- 1
  if(x_formod$parity7[i] == 7) x_formod$parity7[i] <- 1
  if(x_formod$parity9[i] == 9) x_formod$parity9[i] <- 1
}
str(x_formod)
#transform into a matrix
x_formod <- as.matrix(x_formod)
y_formod <- as.matrix(y_formod)
#calculate the 5-fold value of the updateformod
MSE[1, 1] <- k_fold(ForMod, k=5, x = x_formod, y = y_formod)  

# STEP MODEL
# spread the model data to fit the matrix calculation
x_stepmod <- BabiesData %>% select(gestation, parity, ht, drace, dht, time)
y_stepmod <- BabiesData %>% select(wt...7)
x_stepmod <- spread(x_stepmod, key = parity, value = parity, sep = '')
x_stepmod <- spread(x_stepmod, key = drace, value = drace, sep = '')
x_stepmod <- spread(x_stepmod, key = time, value = time, sep = '')
x_stepmod <- x_stepmod %>% select(gestation, parity1, parity10, parity11, parity2,
                                  parity3, parity4, parity5, parity6, parity7, parity9, ht,
                                  drace1, drace2, drace3, drace4, drace5, drace6, drace7, 
                                  drace8, drace9, dht, time1, time2, time3, time4, 
                                  time5, time6, time7, time8, time9)
x_stepmod[is.na(x_stepmod)] <- 0

for(i in 1:471){
  if(x_stepmod$parity10[i] == 10) x_stepmod$parity10[i] <- 1
  if(x_stepmod$parity11[i] == 11) x_stepmod$parity11[i] <- 1
  if(x_stepmod$parity2[i] == 2) x_stepmod$parity2[i] <- 1
  if(x_stepmod$parity3[i] == 3) x_stepmod$parity3[i] <- 1
  if(x_stepmod$parity4[i] == 4) x_stepmod$parity4[i] <- 1
  if(x_stepmod$parity5[i] == 5) x_stepmod$parity5[i] <- 1
  if(x_stepmod$parity6[i] == 6) x_stepmod$parity6[i] <- 1
  if(x_stepmod$parity7[i] == 7) x_stepmod$parity7[i] <- 1
  if(x_stepmod$parity9[i] == 9) x_stepmod$parity9[i] <- 1
  if(x_stepmod$drace2[i] == 2) x_stepmod$drace2[i] <- 1
  if(x_stepmod$drace3[i] == 3) x_stepmod$drace3[i] <- 1
  if(x_stepmod$drace4[i] == 4) x_stepmod$drace4[i] <- 1
  if(x_stepmod$drace5[i] == 5) x_stepmod$drace5[i] <- 1
  if(x_stepmod$drace6[i] == 6) x_stepmod$drace6[i] <- 1
  if(x_stepmod$drace7[i] == 7) x_stepmod$drace7[i] <- 1
  if(x_stepmod$drace8[i] == 8) x_stepmod$drace8[i] <- 1
  if(x_stepmod$drace9[i] == 9) x_stepmod$drace9[i] <- 1
  if(x_stepmod$time2[i] == 2) x_stepmod$time2[i] <- 1
  if(x_stepmod$time3[i] == 3) x_stepmod$time3[i] <- 1
  if(x_stepmod$time4[i] == 4) x_stepmod$time4[i] <- 1
  if(x_stepmod$time5[i] == 5) x_stepmod$time5[i] <- 1
  if(x_stepmod$time6[i] == 6) x_stepmod$time6[i] <- 1
  if(x_stepmod$time7[i] == 7) x_stepmod$time7[i] <- 1
  if(x_stepmod$time8[i] == 8) x_stepmod$time8[i] <- 1
  if(x_stepmod$time9[i] == 9) x_stepmod$time9[i] <- 1
}
str(x_stepmod)

##transform into a matrix
x_stepmod <- as.matrix(x_stepmod)
y_stepmod <- as.matrix(y_stepmod)
#calculate the 5-fold value of the updateformod
MSE[1, 2] <- k_fold(StepMod, k=5, x = x_stepmod, y = y_stepmod)  
#The result show that the formod is better because the value of 5-fold is much smaller


