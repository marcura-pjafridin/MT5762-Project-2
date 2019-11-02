## PLOTS ## 
library(ggplot2)

gestationPlot <- ggplot(data = BabiesData, aes(x = gestation, y = wt...7), size = 2) + geom_point() + ggtitle("Birth Weight VS Gestation") + ylab("Birth Weight") + xlab("Gestation")
gestationPlot
mHeightPlot <- ggplot(data = BabiesData, aes(x = ht, y = wt...7), size = 2) + geom_point() + ggtitle("Birth Weight VS Mother's Height") + ylab("Birth Weight") + xlab("Mother's Height")
mHeightPlot
fHeightPlot <- ggplot(data = BabiesData, aes(x = dht, y = wt...7), size = 2) + geom_point() + ggtitle("Birth Weight VS Father's Height") + ylab("Birth Weight") + xlab("Father's Height")
fHeightPlot
SmokingPlot <- ggplot(data = BabiesData, aes(x = smoke, y = wt...7), size = 2, colour =smoke) +
                geom_point() + ggtitle("Birth Weight VS Smoke") + ylab("Birth Weight") + xlab("Smoke") + facet_wrap(smoke~.)
SmokingPlot

boxplot(BabiesData$pluralty, BabiesData$outcome, BabiesData$gestation, BabiesData$sex, BabiesData$parity, BabiesData$race, BabiesData$age, BabiesData$ed, 
        BabiesData$ht, BabiesData$wt...13, BabiesData$drace, BabiesData$dage, BabiesData$ded, BabiesData$dht, BabiesData$dwt, 
        BabiesData$marital, BabiesData$inc, BabiesData$smoke, BabiesData$time, BabiesData$number, names = c("pluralty", "outcome", "gestation", "sex", " parity", 
        "race", "age", "ed", "ht", "wt...13", "drace", "dage", "ded", "dht", "dwt",
        "marital", "inc", "smoke", "time", "number"), main = "Boxplots of Variables", las = 2, horizontal = FALSE, col= rainbow(19))

boxplot(BabiesData$wt...7 ~ BabiesData$smoke, col = terrain.colors(4), xlab = "Smoke", ylab = "Birth Weight", main = "Boxplot of Birth Weight VS Smoke")

boxplot(BabiesData$gestation, BabiesData$ht, BabiesData$dht, BabiesData$smoke, col = terrain.colors(4), names = c("gestation", "mother's height", "father's height", "smoke"))
