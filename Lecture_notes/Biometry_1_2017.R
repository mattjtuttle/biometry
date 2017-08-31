# Butterfly
# assume probability present in any given forest gap is 0.02
#absent, then, must be 1 - 0.02 = 0.98

1 - 0.02 #maths 

p <- 0.02 # p gets 0.02   This assigns the value 0.02 to p
q <- 1 - p

#prob is present in any given gap
print(p)

#in one gap and absent in the next
print(p*q)  #I'm going to stop using print now
p*q

#absent in two selected gaps
q*q

#present in 10 selected gaps
(p)^10


#What is the probability it occurs in any 10/349 gaps?

#How many different ways to get 10 gaps?
349*348*347*346*345*344*343*342*341*340
#How many unique ways of getting 10 gaps- divide the above by 10!
factorial(10)

#so,...
(349*348*347*346*345*344*343*342*341*340)/factorial(10)  #probability it occurs in any 10/349 gaps

choose(349,10)#binomial coefficient  

# Good for us.... but all we've figured out is the number of unique combinations
# Probability of butterfly in exactly 10 gaps:
# Prob in exactly 10  *  prob absent from exactly 339  *  unique combinations of 10 gaps

0.02^10 * 0.98^339 * choose(349,10)#small, but not nearly as small as ten specific gaps     (p)^10


#Many Bernoulli trials = Binomial Random Variable (thus, sampling from a binomial distribution)
#   Y~Binomial(n,p)
#   probability of obtaining Y successful outcomes in n independent Bernoulli trials where the probability of success for any event is p
#   if n=1, binomial random variable Y is equivalent to Bernoulli random variable
p <- 0.02
q <- 1-p

the.probs<-numeric()#create an empty numeric vector
obs<-0:349
		for (i in 1:length(obs)){  #starts a loop - each time i will increase by 1
			the.probs[i] <- p^obs[i] * q^(349-obs[i]) * choose(349,obs[i])  #calculate the probability it is in exactly obs towns
			}
	
	
	
plot(0:349,the.probs)	
plot(0:349,the.probs,xlim=c(0,19))
plot(0:349,the.probs,xlim=c(0,19),type="h",lwd=10,main="Probability Mass Function") #
sum(the.probs)#all the probabilities should sum to 1
# play with different values of p to see how it changes the shape of the binomial dist. 
