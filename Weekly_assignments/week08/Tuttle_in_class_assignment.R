# Name: Matthew Tuttle
# Week08 - In class assignment

# Imports data as a dataframe
dataset <- read.csv(file = "Weekly_assignments/week08/InClassData.csv", header = TRUE)

# Tests the assumptions of the linear model
hist(dataset$gap.area.m2)
hist(dataset$Nitrate.ppm)

library(fBasics)
normalTest(dataset$gap.area.m2, method = c("da")) # Is right skewed
normalTest(dataset$Nitrate.ppm, method = c("da")) # Is normal

gap.area.root <- sqrt(dataset$gap.area.m2)
hist(gap.area.root)
normalTest(gap.area.root, method = c("da")) # Is normal

# Adds transformed data to dataframe
dataset$gap.area.root <- gap.area.root

# Creates a linear model
model <- lm(formula = Nitrate.ppm ~ gap.area.root, data = dataset)
summary(model) # Shows slope statistically different from zero

# Calculates 95% confidence interval around the slope (for fitted data)
pred.frame <- data.frame(gap.area.root = seq(from = 0, to = 100, length = 200))
pred.conf <- predict(model, interval = "confidence", newdata = pred.frame)
cbind(pred.frame, pred.conf)

# Calculates 95% confidence interval around predicted values
pred.pred <- predict(model, interval = "prediction", newdata = pred.frame)
cbind(pred.frame, pred.pred)

# Graphs data with linear model, confidence interval around the slope, and estimated range of predicted values
plot(dataset$Nitrate.ppm ~ dataset$gap.area.root, xlab = "Square root gap area (m)", ylab = "Nitrogen (ppm)")
abline(model)
matlines(pred.frame, pred.conf, col = c("black", "red", "red"))
matlines(pred.frame, pred.pred, col = c("black", "blue", "blue"))

#####
# Given the above model, we can conclude that the slope of the model is statistically different from zero (p = 0.0392). This means that we can predict that larger gap sizes lead to lower concentrations of nitrogen in the soil.

