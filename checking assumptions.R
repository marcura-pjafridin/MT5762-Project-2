# ----------------------------------------- MODEL ASSUMPTIONS ------------------------------------------ ##

#FORWARD MODEL
# Checking the normality of the residuals
plot(ForMod)
## from the second plot, the residuals are normally distributed

qqnorm(resid(ForMod))

qqline(resid(ForMod))

shapiro.test(resid(ForMod))

## the p-value = 0.5073, the distribution is normally distributed. 

# Checking the variance of the residuals 

ncvTest(ForMod)

## the p-value = 0.63857
## the variance of the residuals is constant

# Checking the independence of residuals 
durbinWatsonTest(ForMod)

## the p-value = 0.594, so the residuals are independent

# Checking for multicollinearity 
vif(ForMod)

## All the GVIF < 10
## No strong multicolinearity

#STEP MODEL
# Checking the normality of the residuals
plot(StepMod)
## from the second plot, the residuals are normally distributed

qqnorm(resid(StepMod))

qqline(resid(StepMod))

shapiro.test(resid(StepMod))

## the p-value = 0.554, the distribution is normally distributed. 

# Checking the variance of the residuals 

ncvTest(StepMod)

## the Variance Score Test p-value = 0.70392
## the variance of the residuals is constant

# Checking the independence of residuals 
durbinWatsonTest(StepMod)

## the p-value = 0.67, so the residuals are independent

# Checking for multicollinearity 
vif(StepMod)

## All the GVIF < 10
## No strong multicolinearity

## Both ForMod and StepMod pass the assumption checking
