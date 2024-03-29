#bootstrapping
#Establish a function that returns the vector of regression coefficient 
bootstrapping <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}
#Then using the function to resample 1000 replications
library(boot)
set.seed(2345)
results <- boot(data = ModelData, statistic = bootstrapping,
                R = 1000, 
                formula = wt...7 ~ gestation + smoke + ht + drace + parity + dht + id)
print(results)


#Add an index parameter to the plot() and boot.ci() function
#To indicate which column of object to analyze
#index 2 is gestation, index 3 is smoke, index 4 is ht, index 5 is drace, 
#index 6 is parity, index 7 = dht, index 8 is id
#To polt the results for each index
#8 plots in total
for (i in 1:8) {
  plot(results, index = i)
}

plot(results, index = 1)
plot(results, index = 2)
plot(results, index = 3)
plot(results, index = 4)
plot(results, index = 5)
plot(results, index = 6)
plot(results, index = 7)
plot(results, index = 8)


#To generate the 95% confidence interval for “gestation + smoke + ht + drace + parity + dht + id”

boot.ci(results, type = "bca", index = 1)
boot.ci(results, type = "bca", index = 2)
boot.ci(results, type = "bca", index = 3)
boot.ci(results, type = "bca", index = 4)
boot.ci(results, type = "bca", index = 5)
boot.ci(results, type = "bca", index = 6)
boot.ci(results, type = "bca", index = 7)
boot.ci(results, type = "bca", index = 8)
