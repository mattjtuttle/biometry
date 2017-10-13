# Imports data as a dataframe
dataset <- read.csv(file = "Weekly_assignments/week08/InClassData.csv", header = TRUE)






model <- lm(formula = Nitrate.ppm ~ gap.area.m2, data = dataset)

plot(dataset$Nitrate.ppm ~ dataset$gap.area.m2)
abline(model)

plot(model)

hist(dataset$gap.area.m2)
hist(dataset$Nitrate.ppm)

library(fBasics)
normalTest(dataset$gap.area.m2, method = c("da"))
normalTest(dataset$Nitrate.ppm, method = c("da"))