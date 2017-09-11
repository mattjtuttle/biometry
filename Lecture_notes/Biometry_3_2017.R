
######

#Poisson distribution - a distribution for (rare) events
#average number of male bears per ha is 0.3
#probability a sampled ha will have 4 male bears

p.four.bears <- ((0.3)^4/factorial(4))*exp(-0.3)
dpois(4,0.3)

p.zero.bears <- ((0.3)^0/factorial(0))*exp(-0.3)



p.X.bears <- dpois(x=0:20,lambda=0.3)
plot(0:20,p.X.bears,type="h",lwd=4,main="PMF")
cum.p.X.bears <- ppois(0:20,lambda=0.3)
plot(0:20,cum.p.X.bears,type="h",lwd=4,main="CDF")

# how lambda affects the shape
par(mfrow=c(2,2),mar=c(3,3,1,1))
plot(0:20,dpois(0:20,lambda=0.3),type="h",lwd=4,main="lambda=0.3")
plot(0:20,dpois(0:20,lambda=1.3),type="h",lwd=4,main="lambda=1.3")
plot(0:20,dpois(0:20,lambda=2.3),type="h",lwd=4,main="lambda=2.3")
plot(0:20,dpois(0:20,lambda=4.3),type="h",lwd=4,main="lambda=4.3")


#What is the probability that a 100 year flood will occur at least once over a 30 year period?

lambda<-1/100*30
ppois(0,0.3,lower.tail=FALSE)

#It is possible to approximate this solution with a binomial approach, using 30 as the number of trials, and p=0.01
pbinom(0,30,0.01,lower.tail=FALSE)

#GEnerally, when there is an inderminate number of trials, Poisson is the appropriate distribution.