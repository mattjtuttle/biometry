# Assume that there is a probability that mosquitos are infected with Rickettsia is 0.23
# Probability of 17 or fewer infected individuals?
prob_1 <- pbinom(17,75,0.23)

# Greater than 10 individuals?
prob_2 <- 1 - pbinom(10,75,0.23)
# First number needs to be ten as we are including the ten
# Always be careful when working with discrete distribution as this can really affect the probabilities that you get
# OR
prob_2b <- pbinom(10,75,0.23,lower.tail = FALSE)

# Exactly 23 individuals?
prob_3 <- dbinom(22,75,0.23)