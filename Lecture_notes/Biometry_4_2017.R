#intro thinking to continuous
?dunif
dunif(0.4,min=0,max=1)
dunif(0.6,min=0,max=1)

dunif(1,min=0,max=10)
dunif(0:10,min=0,max=10)
dunif(0:10,min=0,max=100)


# the function curve() is a good friend for thinking about distributions
curve(dunif(x,0,1),main="Uniform: Probability Density Function (PDF)")
curve(punif(x,0,1),main="Uniform: Cumulative Distribution Function (CDF)")
punif(0.5,min=0,max=1) #probability of getting a random variable <= 0.5 drawn from a uniform distribution bounded by 0 and 1



# Normal
#normal
par(mfrow=c(2,2))#this sets up a 2*2 plotting window
curve(dnorm(x,0,1),xlim=c(-5,5),main="mu=0, stdev=1")
curve(dnorm(x,2,1),xlim=c(-5,5),main="mu=2, stdev=1")
curve(dnorm(x,0,2),xlim=c(-5,5),main="mu=0, stdev=2")
curve(dnorm(x,0,0.5),xlim=c(-5,5),main="mu=0, stdev=0.5")



#some functions
y<-c(1,3,7,100,1000,1001,1002)
mean(y)
median(y)

y<-c(1,3,7,100,1000,1001)
mean(y)
median(y)





# can we predict the 50%, 95%, 99$ data intervals (note: these ARE NOT CONFIDENCE INTERVALS)
qnorm(0.25);qnorm(0.75)
qnorm(0.025);qnorm(0.975)
qnorm(0.005);qnorm(0.995)

curve(dnorm(x),xlim=c(-5,5))
abline(v=c(qnorm(0.25),qnorm(0.75)),lwd=3)
abline(v=c(qnorm(0.025),qnorm(0.975)),col="red",lwd=3)
abline(v=c(qnorm(0.005),qnorm(0.995)),col="blue",lwd=3)

set.seed(2)
dat<-rnorm(n=10000,mean=10,sd=2)
mean.dat<-mean(dat)
sd.dat<-sd(dat)

quantile(dat,c(0.25,0.75))#empirical
mean.dat-qnorm(0.25)*sd.dat
mean.dat+qnorm(0.25)*sd.dat

quantile(dat,c(0.025,0.975))#empirical
mean.dat-qnorm(0.025)*sd.dat #using qnorm will be more percise
mean.dat+qnorm(0.025)*sd.dat

quantile(dat,c(0.005,0.995))#empirical
mean.dat-qnorm(0.005)*sd.dat #this is the same as mean.dat+qnorm(0.995)*sd.dat
mean.dat+qnorm(0.005)*sd.dat

#sometimes easier transform our data so that they are in units of standard deviation (z transformation)
#z = (Yi-mean(Y))/sd
#the scale function will do this

inches<-c(4,7,6,4,5,3,2,4,5)
mean(inches)
sd(inches)

cm<-inches*2.54
mean(cm)
sd(cm)

zinches<-scale(inches)
zcm<-scale(cm)

mean(zinches)#pretty close to zero - rounding problems
mean(zcm)

sd(zinches)
sd(zcm)



#central limits theorem proof of concept
set.seed(2)
sampled.means<-NA#defines an 'empty' vector

#this loops 1000 times. Each time we sample 100 random variables
#drawn from a uniform distribution bounded by 0 and 100
for(i in 1:1000){
	y=runif(100,0,100)
	sampled.means[i]<-mean(y)
	}
#in the end, we have 1000 estimates of the mean
hist(sampled.means,breaks=25)
#Or distribution of estimates of the mean are normal, even though the population itself was not distributed normal.

