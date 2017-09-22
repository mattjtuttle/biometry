#In class and homework for the week.

# Name: Matthew Tuttle

# In an effort to keep up with rising starfruit demand, you begin taking measurements of yields of fruit from individual plants. Below is data of fruit mass in grams from each plant.
fruit.mass.g<-c(9.98,37.87,16.72,9.83,6.01,30.08,59.23,8.15,246.81,8.2,0.12,3.52,218.36,73.75,5.99,2.63,155.61,3.85,90.17,27.18,8.88,6.5,1.39,234.51,1.07,22.94,3,0.82,0.6,551.9,14.59,21.63,50.24,126.92,5.3,35.75,2.11,411.13,142.95,10.27)


#1. Use a histogram to look at the shape of your fruit mass data.

hist_init <- hist(fruit.mass.g)

#2. Log transform the data and use a histogram to look at the shape of your fruit mass data.

log_data <- log(fruit.mass.g + 1) # Added a constant of 1 since some values in dataset are less than 1
hist_log <- hist(log_data)

#3. Calculate the 95% CI and provide the answer in grams. (Hint: think about how your data is transformed!) 

the.mean <- mean(log_data)
std.dev <- sd(log_data)
n <- length(log_data)
std.error <- std.dev/sqrt(n)
the.CI <- qt(0.975, df = n-1) * std.error

lower.CI.log <- the.mean - the.CI
upper.CI.log <- the.mean + the.CI

lower.CI.g <- exp(lower.CI.log) - 1 # Backtransformed lower 95% confidence interval
upper.CI.g <- exp(upper.CI.log) - 1 # Backtransformed upper 95% confidence interval

#4 Wow! You found a super tree. It yields 754 g of starfruit. How exceptional is this tree? (What is the probability of finding that yield or greater based on the data?)

exceptionality <- pt(log(754 + 1), df = n-1, lower.tail = FALSE)
