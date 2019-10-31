############### check assumptions #################

###### ForMod ######
# checking linear relationship
plot(ForMod,1)
## no curvature, so the relationship is linear

# checking if residuals normally distributed
plot(ForMod,2)
shapiro.test(resid(ForMod))
## from the normal Q-Q plot, the residuals are normally distributed
## p-value = 0.5073, so the resiudals are normally distributed

# checking the variance of the residuals 
ncvTest(ForMod)
## p-value = 0.63857, so the variance of residuals is constant

# checking the independence of residuals
durbinWatsonTest(ForMod)
## p-value = 0.59, rediduals are independent

# checking collinearity 
vif(ForMod)
## 1 < GVIFs < 2, no strong colinearity

###### StepMod ######
# checking linear relationship
plot(StepMod,1)
## no curvature, so the relationship is linear

# checking if residuals normally distributed
plot(StepMod,2)
shapiro.test(resid(StepMod))
## from the normal Q-Q plot, the residuals are normally distributed
## p-value = 0.554, so the resiudals are normally distributed

# checking the variance of the residuals 
ncvTest(StepMod)
## p-value = 0.70392, so the variance of residuals is constant

# checking the independence of residuals 
durbinWatsonTest(StepMod)
## p-value = 0.67, rediduals are independent

# checking collinearity 
vif(StepMod)
## 1 < GVIFs < 2, no strong colinearity


############### Both ForMod and StepMod pass the assumption checking #################