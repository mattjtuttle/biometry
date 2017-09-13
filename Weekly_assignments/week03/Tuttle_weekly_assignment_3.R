#Return to eebbiometry@gmail.com by 11:15am, Friday September 15.
#   on the subject line put    YOURLASTNAME_EXERCISE
#Save the file as "Your_last_name_Weekly_Assignment_3.R"

#Name: Matthew Tuttle



#1. Given the following data of ticks found on dogs in summer (x), what is the maximum likelihood estimate of the parameter lambda?
x<-c(rep(0,20),rep(1,12),rep(2,8),rep(3,4),rep(4,2),5)
  
  max.like.estimate <- mean(x)

#2. Given the lambda above: what is the probability of finding 3 or more ticks on one dog?

  prob.3.plus <- ppois(2, max.like.estimate, lower.tail = FALSE)

#3. What is the most likely number of ticks found on a dog in this scenario?

  prob.x.ticks <- dpois(0:5, max.like.estimate)

  # Can be seen as equaling 1 tick by looking at max of plot
  plot(0:5, prob.x.ticks, type = "h", lwd = 4, main = "Probability mean distribution") 

  # Or can also be calculated
  most.like.num <- which(prob.x.ticks == max(prob.x.ticks)) - 1 # Subtract one since R starts vector indices at 1


#4. If you sample a normal distribution with a mean of 10 and a standard deviation of 2, what is the expected range of 50% of the population that is closest to the mean?

  qnorm(0.25, mean = 10, sd = 2);qnorm(0.75, mean = 10, sd = 2)


###You are an ornithologist that loves studying bird eggs. One day a colleague brings you a basket of eggs! Lucky you. Looking at the color and size, you narrow it down to two bird species that have, on average, different masses. Weighing the eggs, you collect the following data:

egg.data<-c(6.3,7.8,4.1,8.2,6.4,5.3,2.9,8.3)  


#5. Egg mass of species one can be modeled as a normal distribution with a mean of 5 grams and a standard deviation of 1.5 grams. The egg mass of species two can be modeled as a normal distribution with a mean of 7 grams and a standard deviation of 1 gram. Based on these data, what is the most likely species these eggs belong to?

  prob.species.1 <- mean(pnorm(egg.data, mean = 5, sd = 1.5))
  prob.species.2 <- mean(pnorm(egg.data, mean = 7, sd = 1))
  
# The eggs most likely belong to species 1 given that the sampled eggs have a higher average probability of being seen with the distribution of species 1 eggs as opposed to the distribution of species 2 eggs.
  