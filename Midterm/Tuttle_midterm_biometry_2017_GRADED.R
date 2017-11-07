# Midterm
# The midterm is due Wednesday, October 25th at 11:15 am. Email the exam to eebbiometry@gmail.com. You need only send this single document (an R text document). Be sure that the code you provide can be run in R as is. Provide all the code used to carry out your analyses (e.g., tests for normality, plotting and tests used during exploratory data analysis, tests for collinearity, lack of fit (if required), diagnostics, etc.). Please annotate all your code so we can follow along step by step. Finally, remember that this is an exam; you are on the honor system to make this an INDIVIDUAL effort please.

#Name: Matthew Tuttle


##################################################################

# Grading

# In grading this exam I assumed that each part of a question is worth 1 point and that no partial credit is possible. Below is a breakdown of points earned by question:

# Question  Points_Earned

# 1A        0
# 1B        0
# 1C        0
# 1D        1
# 1E        1

# 2A        1
# 2B        1
# 2C        1
# 2D        1
# 2E        1

# 3         1

# 4A        1
# 4B        0
# 4C        1
# 4D        1
# 4E        1

# 5A        1
# 5B        1
# 5C        1
# 5D        1

# 6A        1
# 6B        1

# 7A        1
# 7B        1
# 7C        1


# Total     21/25 points

# Grade:    84.0%


##################################################################

#1. Below are field measurements of egg mass of treefrogs (We will assume a normal distribution).
egg.mass<-c(3.1, 4.6, 3.9, 3.4, 3.6, 3.7, 3.8, 4.6, 4.7, 4.3, 2.6, 6, 4.7, 3.4, 3.1, 4.9, 3.8, 5.3, 4.4, 5.9, 3.6, 5.1, 5.2, 3.6, 5.3, 2.9, 4.7, 4.7, 3.8, 6.5, 5.1, 6.3, 5.1, 2.7, 4.4, 4.3, 3.7, 5.5, 3.2, 5.6, 4, 2.3, 5.2, 4.4, 4.8, 3.6, 3.7, 4.2, 5.3, 5.3, 5.6, 4.1, 4.1, 5, 4.1, 5, 3.3, 5.9, 5.7)


#A.  Based upon our parameter estimates for a normal distribution, what is the equal tailed 95% interval for any sample drawn from this population (i.e., that is the expected range of 95% of our sample)?

expected.range <- quantile(egg.mass, c(0.025, 0.975))

  
#B. What is the equal tailed 95% interval for OUR sample?

mean.egg.mass <- mean(egg.mass)
sd.egg.mass <- sd(egg.mass)

upper.range <- mean.egg.mass-qnorm(0.025)*sd.egg.mass
lower.range <- mean.egg.mass+qnorm(0.025)*sd.egg.mass


#C. If we were to sample this population tens of thousands of times, what is the expected standard deviation of the means?

# The expected standard deviation of the means would be 1 since the data should be normally distributed.


#D. Calculate the 95% confidence interval

n.egg.mass <- length(egg.mass)
std.error <- sd.egg.mass/sqrt(n.egg.mass)
the.CI <- qt(0.975, df = n.egg.mass-1) * std.error

lower.95.CI <- mean.egg.mass - the.CI
upper.95.CI <- mean.egg.mass + the.CI


#E. Based on our parameter estimates, what is the probability that a sampled egg from the population has an egg mass equal to or less than 3.3

prob3.3 <- pnorm(3.3, mean = mean.egg.mass, sd = sd.egg.mass, lower.tail = TRUE)


##################################################################

#2.  You've sampled tunicates (sessile marine chordates) from m^2 quadrates in Oregon (the data is below).  Based on these data, answer the following questions.

tunicates<-c(13,9,7,4,5,9,5,4,5,6,8,8,6,9,9,5,8,5,6,7,14,9,3,13,9,5,8,4,5,3,6,9,3,2,10,6,8,8,6,9,12,4,19,7)

#A.  What is an appropriate distribution to use to model this data?

# An appropriate distribution for modeling this data is the Poisson distribution since the tunicate data is count data and therefore a discrete distribution should be used over a continuous one. Additionally, the Poisson distribution does not require a set number of trials and is bound from 0 to infinity, both of which fit well with this dataset.

# Test showing that this data fits a Poisson distribution well
library(vcd)
gf <- goodfit(tunicates, type = "poisson")
summary(gf)


#B.  What is/are the parameter estimate(s) for this distribution?

# The parameter estimate for this distribution is lambda, known as the rate parameter. The maximum likelihood estimate of this parameter is the mean of all observations.


#C.  What is the probability that your next quadrate will have 3 or fewer tunicates?

prob.3.fewer <- ppois(3, lambda = mean(tunicates), lower.tail = TRUE)


#D.  What is the probability that your next quadrate will have 9 or greater tunicates?

prob.9.greater <- ppois(8, lambda = mean(tunicates), lower.tail = FALSE)


#E.  Based on our parameter estimates, what is the probability that a quadrat has BETWEEN 3 and 6 tunicates?

prob.between <- dpois(4, lambda = mean(tunicates)) + dpois(5, lambda = mean(tunicates)) # Probability exclusive of the values 3 and 6, therefore only the densities at 4 and 5 are included


#######################################################

#3. Suppose 0.5% of all students seeking treatment at a school infirmary are eventually diagnosed as having mononucleosis.  Of those who do have mono, 90% complain of a sore throat.  But 30% of those who do not have mono also claim to have sore throats.  If a student comes to the infirmary and says he or she has a sore throat, what is the probability the student has mono?

prior <- 0.005 # 0.5% of students seeking treatment have mono
likelihood <- 0.9 # 90% with mono complain of sore throat
no.mono.sore <- 0.3 # 30% of students without mono that have sore throats
observed <- (likelihood * prior) + (no.mono.sore * (1 - prior)) # Probability of the data

prob.mono <- (likelihood * prior) / observed # Probability a student with a sore throat has mono


######################################################

#4.  Here are some data on fish mouth gape and gill raker length. Based on this data, answer the following questions.

gape.cm<-c(100.3, 97.2, 100.8, 105, 102, 101.1, 100, 105.6, 100.8, 102.5, 100.2, 105.5, 103.5, 98.6, 99.3, 98.9, 102.1, 97.7, 98.5, 99.4, 100.3, 100.1, 100.8, 101.2, 95.5, 99.3, 96.5, 97.8, 101.5, 100.4, 101.1, 95.6, 99.1, 95.5, 104.8, 102.4, 98, 101, 101.3, 103.4, 98.6, 98, 99.5, 96.7, 99.5, 100.4, 100.3, 96.2, 98.4)

gill.raker.length.mm<-c(250.7, 230, 257.6, 241.2, 247.7, 252, 251.5, 261.5, 242.7, 245.4, 265.9, 256.1, 238.3, 256.9, 236.2, 244.3, 247.2, 235.7, 253.8, 247.8, 242.4, 251.7, 251.3, 245.5, 237.5, 249.6, 228.8, 241.8, 253.5, 244.1, 242.3, 240.5, 251.6, 241, 253.4, 248.9, 232.5, 250.8, 252.4, 245.9, 238.8, 241.9, 225.1, 232.1, 239.9, 236.1, 253.8, 223.4, 233.2)

#A.  What is the pearson-product moment correlation for these data?

pearson.cor <- cor(gape.cm, gill.raker.length.mm, method = c("pearson"))


#B.  What is the 99% confidence interval around this correlation?

n.gape <- length(gape.cm)
z.pearson.cor <- 0.5 * log((1 + pearson.cor) / (1 - pearson.cor)) # Fisher's r-z transformation
z.se <- sqrt(1 / (n.gape - 3))

upper.z.CI <- z.pearson.cor + qt(0.995, n.gape - 2) * z.se
lower.z.CI <- z.pearson.cor + qt(0.005, n.gape - 2) * z.se

# Backtransformed 99% confidence interval
upper.99.CI <- (exp(2 * upper.z.CI) - 1) / (exp(2 * upper.z.CI) + 1)
lower.99.CI <- (exp(2 * lower.z.CI) - 1) / (exp(2 * lower.z.CI) + 1)


#C.  What units are correlation in?

# Correlation is dimensionless given that the data is z-transformed when calculating correlation.


#D.  What is the covariance between these two?

covariance <- cov(gape.cm, gill.raker.length.mm, method = c("pearson"))


#E.  What units are this covariance in?

# The units of this covariance are cm gape * mm gill raker length. This is because you multiply the units of your variables together when you are calculate the covariance. This value is therefore sensitive to the starting units you use when calculating the covariance. Correlation on the other hand is not, as it standardizes values via z-transformation. If we were to z-transform our data and then calculate the covariance, we would see that the covariance of our z-transformed data is equal to the correlation.


##################################################################

#5. Below are data on total leaf lesions due to viral infection and average nectar volume of flowers (microliters) of deadly nightshade (Atropa belladona). We are interested in knowing whether the number of leaf lesions can predict nectar volume.

#A. Make a pretty figure showing the data, your results, and the 95% confidence interval around the fitted values. Use these data to address the following questions.

nectar.ul <- c(132.6,125.66,133.99,129.78,124.59,131.93,131.49,131.95,130.88,129.6,126.09,132.59,133.5,130.5,133.35,130.46,131.84,136.92,129.24,130.81,127.19,127.05,125.32,127.71,127.44,130.94,125.5,125.3,132.04,132.65,129.04,126.43,122.96,126.15,125.35,127.01,133.2,136.29,135.59,130.46,125.07,132.54,130.3,124.04,131.43,128.21,132.81,131.45,132.25,130.21,125.4,129.43,126.75,131.9,130.34,132.96,127.61,130.39,130.19,129.27,133.51,127.37,131.83,128.48,132.39,131.06,136.65,130.95,134.02,130.87,130.6,130.72,128.86,129.84,128.69,130.5,126.97,129.58,129.43,131.76,128.49,135.49,126.94,130.11,127.82,132.24,122.45,131.49,130.26,131.61,127.09,133.37,128.22,127.17,126.86,128.83,131.12,131.71,130.56,130.21)

lesions <- c(51,17,25,28,29,22,33,50,38,17,38,15,5,29,44,22,46,28,29,32,17,20,42,46,19,27,33,16,44,45,16,49,40,30,21,25,28,43,25,27,36,31,8,19,33,34,30,33,45,34,19,22,34,23,36,19,33,18,34,28,57,7,30,13,36,37,32,31,31,35,45,29,42,47,30,27,13,41,21,36,8,39,22,20,42,33,30,34,23,42,22,27,35,25,31,16,22,30,37,23)


# Generates a linear model looking at volume of nectar as a function of lesions
model <- lm(nectar.ul ~ lesions)

# Calculates 95% confidence interval around fitted values
summary(lesions)
pred.frame <- data.frame(lesions = seq(from = 0, to = 60, length = 200))
pred.conf <- predict(model, interval = "confidence", newdata = pred.frame)
cbind(pred.frame, pred.conf)

# Makes a pretty figure
plot(x = lesions,
     y = nectar.ul,
     pch = 16,
     col="darkgrey",
     xlab="Number of lesions",
     ylab="Volume of nectar (uL)",
     las=1,
     main="Nectar volume increases as number of lesions increases",
     cex.lab=1.5,
     cex.axis=1.2
     )
abline(model)
matlines(pred.frame, pred.conf, lwd = 2, lty = c(1,2,2), col = c("black", "red", "red"))


#B. Use an anova to test the null hypothesis that the slope is different from zero.

anova.test <- anova(model)
prob.not.zero <- anova.test$`Pr(>F)`[1]

# Given that the probability of the slope being different from zero is greater than 0.05, we fail to reject our null hypothesis. Therefore, we accept that the slope is equal to zero, assuming that we are interested at looking at our data at the 95% confidence level with a strict p-value cut off of 0.05.

# p-value can also be calculated by hand
summary(model)
theta <- 0
slope <- model$coefficients[2]
slope.err <- 0.02793 # pulled value from model summary
t.slope <- (slope - theta) / slope.err
by.hand.prob.not.zero <- pt(q = t.slope, df = (length(nectar.ul) - 2), lower.tail = FALSE) * 2


#C. Theory predicts that nectar volume should INCREASE as the number of lesions increases. Test this hypothesis.

# One-tailed t-test used to calculate p-value in one direction to see if the slope is greater than zero.
prob.pos.relationship <- pt(q = t.slope, df = (length(nectar.ul) - 2), lower.tail = FALSE) 

# Given that the p-value is less than 0.05, this indicates that the slope is greater than zero. This means that as theory predicts, nectar volume increases as the number of lesions increases. However, given that we failed to reject our null hypothesis in part B, the appropriateness of using this one-tailed t-test to calculate this p-value is questionable.


#D.  Based on your parameter estimates, what is the 95% confidence interval around the fitted values of a plant with 62 lesions? 

lesions.95.CI <- predict(model, interval = "confidence", newdata = data.frame(lesions = 62))
lesions.lower.95.CI <- lesions.95.CI[2]
lesions.upper.95.CI <- lesions.95.CI[3]


##################################################################

#6. Below is data on a species of wild mustard. Leaf glucosinolate (ug/mg dry weight) and isothiocyanates (ug/mg dry weight) have been implicated in herbivore resistence. Leaf thickness (mm) is the average thickness of leaves based on the average of 10 randomly selected leaves. Using these data, answer the questions below.

leaf.glucosinolate<-c(121.061,151.448,106.522,97.407,110.052,158.652,111.531,109.037,102.374,122.181,144.418,147.539,137.071,124.052,104.414,114.593,108.744,103.743,95.769,76.728,115.18,109.493,88.466,108.89,103.884,113.558,118.682,156.071,129.332,139.775,97.221,146.174,82.809,114.034,107.907,122.952,115.362,94.464,114.42,88.883,102.469,115.418,120.71,123.972,137.832,108.621,90.55,133.821,113.255,104.742,125.003,104.045,124.76,91.614,147.902,112.632,125.443,99.278,116.993,106.76,101.314,100.753,114.221,98.549,92.063,130.005,111.562,98.613,138.934,94.704,110.393,129.742,108.153,63.278,95.036,140.126,129.686,84.854,112.408,130.374,123.597,115.504,92.829,121.988,126.17,126.373,107.206,113.996,102.958,106.608,118.755,96.874,99.928,134.368,95.333,127.166,125.02,105.476,129.876,129.043)

leaf.thickness<-c(1.0822, 0.4109, 0.7796, 0.5038, 0.5937, 1.1615, 0.9131, 0.5326, 0.9548, 0.7634, 0.3503, 0.0282, 0.5981, 0.8078, 0.8208, 0.728, 0.5962, 0.5556, 0.9134, 0.6792, 0.7639, 0.5394, 1.186, 1.098, 1.1487, 0.5253, 0.2114, 0.3222, 0.4034, 0.905, 0.7284, 0.5862, 1.1065, 1.3853, 0.4488, 0.9278, 0.6902, 0.0712, 0.9412, 0.5027, 0.4663, 1.1643, 0.0591, 0.4589, 0.9365, 0.7691, 1.6655, 0.4192, 0.4673, 0.9452, 0.7169, 0.346, 0.5997, 0.4616, 0.6681, 0.324, 0.4646, 1.192, 0.5435, 0.5451, 0.6609, 0.942, 0.926, 0.8393, 0.4795, 0.8782, 0.4605, 0.5265, 0.9938, 0.7201, 0.6742, 0.6268, 0.5456, 0.7823, 1.0237, 0.5825, 0.1897, 1.1867, 0.4108, 0.6239, 0.7908, 0.7012, 1.1844, 0.6381, 0.8714, 0.8657, 1.2253, 0.5606, 0.9779, 0.6655, 1.2221, 0.463, 0.5518, 0.9708, 1.0647, 0.4452, 0.3716, 1.1642, 1.2378, 0.6129)

isothiocyanates<-c(213.87,168.75,158.33,136.02,150.27,185.48,191.24,81.79,145.42,221.28,144.54,111.87,189.54,188.24,237.78,168.22,187.12,167.51,149.08,146.71,202.23,195.73,152.48,265.74,182.03,209.33,63.68,154.61,104.38,244.58,241.03,147.94,127.02,227.57,178.61,169,149.7,130.58,265.17,121.01,69.91,215.77,118.05,178.02,187.34,176.64,187.75,154.19,160.26,210.11,144.93,122.67,171.63,198.49,167.87,139.11,144.35,247.88,140.1,203.57,178.63,225.01,206.39,223.99,113.32,172.42,98.66,235.47,187.04,235.64,117.81,106.95,108.52,169.27,152.54,116.22,142.73,206.96,186.63,169.65,175.01,203.13,200.5,172.03,189.19,252.13,273.45,185.92,195.58,212.39,178.41,145.78,175.61,71.41,107.13,94.92,139.24,265.15,239.5,171.84)

LAR<-c(31.51,18.5,20.88,17.67,18.72,24.11,21.39,2.71,18.39,23.27,25.17,15.52,28.57,24.11,22.19,23.8,25.96,16.45,24.28,16.29,23.67,18.07,21.27,33.05,20.91,25.19,9.92,16.94,8.93,34.68,19.83,20.02,20.2,24.38,21.7,21.46,24.82,10.34,30.77,10.62,12.19,26.69,15.67,20.68,25.7,27.18,25.7,20.11,22.84,31.15,20.12,17.62,16.11,22.67,20.93,15.88,26.24,40.22,23.1,21.78,30.07,26.29,25.68,32.86,8.15,23.85,12.46,36.89,26.06,34.63,8.94,22.9,30.35,25.02,18.78,15.58,16.54,27.3,33.14,24.24,20.33,28.53,24.8,23.7,24.41,37.28,38.63,25.64,27.57,30.38,20.85,13.72,20.59,21.83,19.07,6.75,14.95,34.58,25.64,18.41)

#A. Given the data above of average leaf area removed (mm^2) by caterpillars (LAR), what is the best predictive model of leaf area removed based on the available data?

# Testing for normality
hist(leaf.glucosinolate)
hist(leaf.thickness)
hist(isothiocyanates)
hist(LAR)

library(fBasics)
normalTest(leaf.glucosinolate, method = "da") # Is normal
normalTest(leaf.thickness, method = "da") # Is normal
normalTest(isothiocyanates, method = "da") # Is normal
normalTest(LAR, method = "da") # Is normal

# Backward and forward stepwise model selection
model.full <- lm(LAR ~ leaf.glucosinolate + leaf.thickness + isothiocyanates)
model.back <- step(model.full, LAR ~ leaf.glucosinolate + leaf.thickness + isothiocyanates, direction = "backward")
model.for <- step(lm(LAR ~ 1), scope = list(upper = model.full, lower = ~ 1), direction = "forward")
summary(model.back)
summary(model.for) # Same as model.back

# The best predictive model based on the available data is a model of leaf area removed as a function of isothiocyanates and leaf thickness. Both the backward and forward model selections resulted in this model. Out of all of the possible models using this data, this model has the lowest AIC value (shown below) indicating that this is the best available model.

# AICs of all possible models
model1.1 <- lm(LAR ~ leaf.glucosinolate); AIC(model1.1)
model1.2 <- lm(LAR ~ leaf.thickness); AIC(model1.2)
model1.3 <- lm(LAR ~ isothiocyanates); AIC(model1.3)
model2.1 <- lm(LAR ~ leaf.glucosinolate + leaf.thickness); AIC(model2.1)
model2.2 <- lm(LAR ~ leaf.glucosinolate + isothiocyanates); AIC(model2.2)
model2.3 <- lm(LAR ~ leaf.thickness + isothiocyanates); AIC(model2.3) # Lowest AIC value
model3.1 <- lm(LAR ~ leaf.glucosinolate + leaf.thickness + isothiocyanates); AIC(model3.1)


#B. Based on your model, what is the most important predictor for leaf area removed (and on what do you base this)?

# The most important predictor for leaf area removed is isothiocyanates concentration. When selecting a model, isothiocyanates. The importance of isothiocyanates concentration is also obvious when you plot LAR as a function of isothiocyanates. There is a clear correlation between the two (see below), unlike LAR and either leaf.glucosinolate or leaf.thickness where the correlation is less obvious.

# Plots to look at correlations
plot(LAR ~ leaf.glucosinolate)
plot(LAR ~ leaf.thickness)
plot(LAR ~ isothiocyanates) # Clearest correlation able to be seen


#################################################

#7. Given the data y below, what is the probability AND log likelihood that the data are drawn from the following distributions?

y<-c(1.2,3.4,4.4,5.2)

#A. A normal distribution with a mean of 2 and standard deviation of 3.

dist.A.probs <- dnorm(y, mean = 2, sd = 3)
log.lik.dist.A <- sum(log(dist.A.probs)) # log likelihood drawn from this distribution
prob.dist.A <- exp(log.lik.dist.A) # probability drawn from this distribution


#B. A normal distribution with a mean of 3 and a standard deviation of 1.

dist.B.probs <- dnorm(y, mean = 3, sd = 1)
log.lik.dist.B <- sum(log(dist.B.probs)) # log likelihood drawn from this distribution
prob.dist.B <- exp(log.lik.dist.B) # probability drawn from this distribution


#C. What is the probability and log likelihood of the data based on the maximum likelihood estimate of the mean and standard deviation?

dist.MLE.probs <- dnorm(y, mean = mean(y), sd = sd(y))
log.lik.dist.MLE <- sum(log(dist.MLE.probs)) # log likelihood drawn from this distribution
prob.dist.MLE <- exp(log.lik.dist.MLE) # probability drawn from this distribution

