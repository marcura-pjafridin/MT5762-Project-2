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
