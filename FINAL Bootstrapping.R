set.seed(3445)
#create data
## The final model is wt...7 ~ gestation + smoke + ht + drace + parity + dht
y <- ModelData$wt...7
x1 <-ModelData$gestation
x2 <-ModelData$smoke
x3 <-ModelData$ht
x4 <-ModelData$drace
x5 <-ModelData$parity
x6 <-ModelData$dht

bootstrapdata1 <- data.frame(x1 , y)
bootstrapdata2 <- data.frame(x2 , y)
bootstrapdata3 <- data.frame(x3 , y)
bootstrapdata4 <- data.frame(x4 , y)
bootstrapdata5 <- data.frame(x5 , y)
bootstrapdata6 <- data.frame(x6 , y)

#gestation
#store the  regression coefficients of gestation
bootresults_gestation <- array(dim = c(1000, 2))
for (i in 1:1000) {
  #resample the data 1000 replication
  bootData_gestation <- bootstrapdata1[sample(1 : 100, 100, replace = T),]
  #fit the model
  bootLM_gestation <- lm( y~ x1, data = bootData_gestation)
  #store the coefficients
  bootresults_gestation[i, ] <- coef(bootLM_gestation)
}
#regression bootstrap coefficients
hist(bootresults_gestation[,1], main = "intercept distribution")
hist(bootresults_gestation[,2],  main = 'slope distribution')
#best guess of paramaters
c(mean(bootresults_gestation[,1]), mean(bootresults_gestation[,2]))
# the CIs for these
rbind(quantile(bootresults_gestation[,1], probs = c(0.025, 0.975)),
      quantile(bootresults_gestation[,2], probs = c(0.025, 0.975)))

#smoke
#store the  regression coefficients of smoke
bootresults_smoke <- array(dim = c(1000, 2))
for (i in 1:1000) {
  #resample the data 1000 replication
  bootData_smoke <- bootstrapdata2[sample(1 : 100, 100, replace = T),]
  #fit the model
  bootLM_smoke<- lm( y~ x2, data = bootData_smoke)
  #store the coefficients
  bootresults_smoke[i, ] <- coef(bootLM_smoke)
}
#regression bootstrap coefficients
hist(bootresults_smoke[,1], main = "intercept distribution")
hist(bootresults_smoke[,2],  main = 'slope distribution')
#best guess of paramaters
c(mean(bootresults_smoke[,1]), mean(bootresults_smoke[,2]))
# the CIs for these
rbind(quantile(bootresults_smoke[,1], probs = c(0.025, 0.975)),
      quantile(bootresults_smoke[,2], probs = c(0.025, 0.975)))


#ht
#store the  regression coefficients of ht
bootresults_ht <- array(dim = c(1000, 2))
for (i in 1:1000) {
  #resample the data 1000 replication
  bootData_ht <- bootstrapdata3[sample(1 : 100, 100, replace = T),]
  #fit the model
  bootLM_ht <- lm( y~ x3, data = bootData_ht)
  #store the coefficients
  bootresults_ht[i, ] <- coef(bootLM_ht)
}
#regression bootstrap coefficients
hist(bootresults_ht[,1], main = "intercept distribution")
hist(bootresults_ht[,2],  main = 'slope distribution')
c(mean(bootresults_ht[,1]), mean(bootresults_ht[,2]))
# the CIs for these
rbind(quantile(bootresults_ht[,1], probs = c(0.025, 0.975)),
      quantile(bootresults_ht[,2], probs = c(0.025, 0.975)))

#drace
#store the  regression coefficients of drace
bootresults_drace <- array(dim = c(1000, 2))
for (i in 1:1000) {
  #resample the data 1000 replication
  bootData_drace  <- bootstrapdata4[sample(1 : 100, 100, replace = T),]
  #fit the model
  bootLM_drace  <- lm( y~ x4, data = bootData_drace )
  #store the coefficients
  bootresults_drace [i, ] <- coef(bootLM_drace )
}
#regression bootstrap coefficients
hist(bootresults_drace [,1], main = "intercept distribution")
hist(bootresults_drace [,2],  main = 'slope distribution')
c(mean(bootresults_drace [,1]), mean(bootresults_drace [,2]))
# the CIs for these
rbind(quantile(bootresults_drace [,1], probs = c(0.025, 0.975)),
      quantile(bootresults_drace [,2], probs = c(0.025, 0.975)))


#
#parity
#store the  regression coefficients of parity
bootresults_parity <- array(dim = c(1000, 2))
for (i in 1:1000) {
  #resample the data 1000 replication
  bootData_parity  <- bootstrapdata5[sample(1 : 100, 100, replace = T),]
  #fit the model
  bootLM_parity  <- lm( y~ x5, data = bootData_parity )
  #store the coefficients
  bootresults_parity [i, ] <- coef(bootLM_parity )
}
#regression bootstrap coefficients
hist(bootresults_parity [,1], main = "intercept distribution")
hist(bootresults_parity [,2],  main = 'slope distribution')
c(mean(bootresults_parity [,1]), mean(bootresults_parity [,2]))
# the CIs for these
rbind(quantile(bootresults_parity [,1], probs = c(0.025, 0.975)),
      quantile(bootresults_parity [,2], probs = c(0.025, 0.975)))

#
#dht
#store the  regression coefficients of parity
bootresults_dht <- array(dim = c(1000, 2))
for (i in 1:1000) {
  #resample the data 1000 replication
  bootData_dht  <- bootstrapdata6[sample(1 : 100, 100, replace = T),]
  #fit the model
  bootLM_dht  <- lm( y~ x6, data = bootData_dht )
  #store the coefficients
  bootresults_dht [i, ] <- coef(bootLM_dht )
}
#regression bootstrap coefficients
hist(bootresults_dht [,1], main = "intercept distribution")
hist(bootresults_dht [,2],  main = 'slope distribution')
c(mean(bootresults_dht [,1]), mean(bootresults_dht [,2]))
# the CIs for these
rbind(quantile(bootresults_dht [,1], probs = c(0.025, 0.975)),
      quantile(bootresults_dht [,2], probs = c(0.025, 0.975)))