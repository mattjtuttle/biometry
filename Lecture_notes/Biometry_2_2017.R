p<-0.02
q<-1-p
the.probs<-numeric()#create an empty numeric vector
obs<-0:349
		for (i in 1:length(obs)){  #starts a loop - each time i will increase by 1
			the.probs[i] <- p^obs[i] * q^(349-obs[i]) * choose(349,obs[i])  #calculate the probability it is in exactly obs towns
			}
	
	
	
plot(0:349,the.probs)	
plot(0:349,the.probs,xlim=c(0,19))
plot(0:349,the.probs,xlim=c(0,19),type="h",lwd=10,main="Probability Mass Function",lend=2) #look it up on wiki

sum(the.probs)#all the probabilities should sum to 1



#say we want to know the probability of finding the butterfly in 5 or fewer gaps
sum(the.probs[1:6])#this is the sum of the probability of the butterfly in 0,1,2,3,4,or 5 gaps

# #or we could simply use the function dbinom() in R. It is a statisti al programming language, afterall.
# dbinom(0:5,349,0.02)
# sum(dbinom(0:5,349,0.02))

#we can annotate the output in the console window if we want (which we do for exercise purposes) using the cat() function
cat("\nThe probability is",sum(the.probs[1:6]))

#or we could assign sum(the.probs[1:6]) to an object
sum.probs<-sum(the.probs[1:6])
cat("\nThe probability is",sum.probs)

sum(the.probs[1:6])#this is the sum of the probability of finding the butterfly in 0,1,2,3,4,or 5 towns
pbinom(5,349,0.02) #same as above
pbinom(5,349,0.02,lower.tail=FALSE)#prob greater than 5



cum.probs<-cumsum(the.probs)

cum.probs
cum.probs[1:6]# the 1:6 counts from 1 to 6 which corresponds to 0, 1, 2, 3, 4, 5 patches occupied; 
#pbinom(0:5,349,0.02) #using r function to do the same thing

plot(0:349,cum.probs,xlim=c(0,19),type="h",lwd=10,main="Cumulative Distribution Function",lend=2)
abline(h=0.95,lty=2,col="red",lwd=5)#abline will draw a line through a plot, h=0.95 means draw a horizontal line at the y-axis value of 0.95. So we can think of this as there's a 95% chance that we will find between 0 and 11 patches occupied.

par(mfrow=c(1,2))#this sets up a plotting window with 1 row and 2 columns
plot(0:349,the.probs,xlim=c(0,19),type="h",lwd=10,main="Probability Mass Function",lend=2)
plot(0:349,cum.probs,xlim=c(0,19),type="h",lwd=10,main="Cumulative Distribution Function",lend=2)



#another way is to use the dbinom function
x<-0:349#this creates a vector of integers from 0 to 349
length(x)# function will tell you the length of a vector

dbinom(x,349,0.02)



plot(dbinom(x,349,0.02),xlim=c(0,20),main="Probability Mass Function",type="h",lwd=5,lend=2)
plot(pbinom(x,349,0.02),xlim=c(0,20),main="Cumulative Distribution Function",type="h",lwd=5,lend=2)

compare<-cbind(dbinom(x,349,0.02),the.probs)#this will join the two vectors together creating a matrix like object #note the rounding problems as you get close to zero (i.e., where they start to disagree) - r is using math, we were using arithmatic
colnames(compare)<-c("dbinom function","the.probs")
compare


x <- 0:75
plot(dbinom(x,75,0.23),xlim=c(0,40),main="Probability Mass Function",type="h",lwd=5)
plot(pbinom(x,75,0.23),xlim=c(0,40),main="Cumulative Distribution Function",type="h",lwd=5)

# Rickettsia is a genus of bacteria known to infect insects. Assume for a population of mosquitos the probability of infection is 0.23. 
# If you were to collect 75 mosquitos, what is the probability that you would have 17 or fewer infected individuals?
pbinom(17,75,0.23)#0.5371527
# If you were to collect 75 mosquitos, what is the probability that you would have greater than 10 infected individuals?
pbinom(10,75,0.23,lower.tail=FALSE)#0.9731568
# If you were to collect 75 mosquitos, what is the probability that you would have exactly 22 infected individuals?
dbinom(22,75,0.23)#0.04517953

##################

x<-0:100
plot(dbinom(x,100,0.6),xlim=c(0,100),main="Probability Mass Function",type="h",lwd=5)

# Let's think a little about the paramter p, and how we calculate the MLE
# Let's say we flip a coin 100 times and get 60 heads
# I'm going to write some code so we can look at the likelihood surface
pz<-seq(from=0.001,to=0.999,by=0.001) #possible values for p we'll explore

plot(pz,dbinom(60,100,pz),main="likelihood")#This is plotting the probability of getting 60 heads after 100 tosses across a large range of possible values for the p parameter.
plot(pz,dbinom(60,100,pz),type="l",main="likelihood") #type line makes it look nicer, and it's what we were trying to illustrate, since the number of points is arbitrary determined by the length of pz (of which we have full control over)
plot(pz,dbinom(60,100,pz,log=TRUE),type="l",main="likelihood") #type line makes 
#abline(v=mle.p,col="red",lwd=2)
plot(pz,dbinom(60,100,pz,log=TRUE),type="l",main="log likelihood")
points(0.6,max(dbinom(60,100,pz,log=TRUE)),pch=19)
arrows(0.4,0.08,0.6,0.081)
text(0.25,0.08,"MLE")

mle.p<-60/100
abline(v=mle.p,col="red",lwd=2)

abline(h=max(dbinom(60,100,pz,log=TRUE)),col="blue",lwd=2)#max() determines the maximum value of the vector returned by dbinom. abline(h=)is asking for a horizontal line. 

# we can also examine how the information content affects the 
#what if we only flipped the coin 10 times because... and we get 6 heads and 4 tails
pz<-c(seq(from=0.1,to=0.9,by=0.01))
plot(pz,dbinom(60,100,pz,log=T),type="l",main="likelihood")#)
lines(pz,dbinom(6,10,pz,log=T),col="red") #type line makes 
#We get better parameter estimates as we increase the amount of information/data.