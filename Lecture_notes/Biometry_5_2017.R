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
mean.dat-0.674*sd.dat
mean.dat+0.674*sd.dat

quantile(dat,c(0.025,0.975))#empirical
mean.dat-qnorm(0.025)*sd.dat #using qnorm will be more percise
mean.dat+qnorm(0.025)*sd.dat

quantile(dat,c(0.005,0.995))#empirical
mean.dat-qnorm(0.005)*sd.dat #this is the same as mean.dat+qnorm(0.995)*sd.dat
mean.dat+qnorm(0.005)*sd.dat





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




# SE
#y<-c(1,2,3,4,5)
y<-rnorm(1000,10,2)
se<-sd(y)/sqrt(length(y))#the square root of the standard deviation, sd(), divided by n, which is the length of y, length().  


# proof of concept simulation
# don't stress over the code, unless you are actively trying to learn how to write code (in that case, go ahead and stress over the code)
set.seed(4)
sampled.means<-NA#defines an 'empty' vector

#this loops 1000 times. Each time we sample 100 random variables drawn from a normal distribution with mean 0 and standard deviation of 1
for(i in 1:1000){
	y=rnorm(100)
	sampled.means[i]<-mean(y)
	}
#in the end, we have 1000 estimates of the mean

sd(sampled.means)

y=rnorm(100)
hist(y)
plot(density(y),ylim=c(0,5))
lines(density(sampled.means),col="red")
sd(sampled.means)# the standard deviation of our mean estimates
sd(y)/sqrt(length(y))# the standard error of a sample

#change it to 1000 sample size - note the SE gets smaller
#
# 95% CI




# t-distribution
curve(dnorm(x),ylim=c(0,0.5),xlim=c(-5,5),col="darkgreen",lty=2,lwd=4)
curve(dt(x,1),ylim=c(0,0.5),xlim=c(-5,5),add=TRUE)
curve(dt(x,5),ylim=c(0,0.5),xlim=c(-5,5),add=TRUE,col="blue")
curve(dt(x,10),ylim=c(0,0.5),xlim=c(-5,5),add=TRUE,col="red")
#curve(dnorm(x),ylim=c(0,0.5),xlim=c(-5,5),add=TRUE,col="darkgreen",lty=2,lwd=4)
curve(dt(x,100),ylim=c(0,0.5),xlim=c(-5,5),add=TRUE)

plot(qt(0.975,seq(from=1,to=50,by=0.1)),pch=19,type="l")


curve(dnorm(x),ylim=c(0,0.5),xlim=c(-15,15),lty=1,lwd=2)
abline(v=qnorm(0.025))
abline(v=qnorm(0.975))
curve(dt(x,1),ylim=c(0,0.5),xlim=c(-15,15),col="red",add=TRUE,lwd=2)
abline(v=qt(0.025,1),col="red")
abline(v=qt(0.975,1),col="red")
#those fat tails are fatter than you think!

# If normal, and infinite sample
# 50% of the population falls between mu +/- 0.674*sigma
# 95% of the population falls between mu +/- 1.96*sigma
# 99% of the population falls between mu +/- 2.576*sigma

qt(0.975,14)
qt(0.975,30)
qt(0.975,59)
qt(0.975,1000000000)
qnorm(0.975)




curve(dnorm(x),xlim=c(-5,5))
abline(v=qt(0.25,9999),col="red",lty=1)
abline(v=qt(0.75,9999),col="red",lty=1)
abline(v=qt(0.25,4),col="red",lty=2)
abline(v=qt(0.75,4),col="red",lty=2)

abline(v=qt(0.025,9999),col="blue",lty=1)
abline(v=qt(0.975,9999),col="blue",lty=1)
abline(v=qt(0.025,4),col="blue",lty=2)
abline(v=qt(0.975,4),col="blue",lty=2)


abline(v=qt(0.01,9999),col="orange",lty=1)
abline(v=qt(0.99,9999),col="orange",lty=1)
abline(v=qt(0.01,4),col="orange",lty=2)
abline(v=qt(0.99,4),col="orange",lty=2)


qt(0.025,14)#sample size 15 
qt(0.975,14)

################################### 
# 95% CI

set.seed(2)
# qt(0.975,1:50)
# qt(0.975,1:500)

y=rnorm(100)
the.mean<-mean(y)
the.stv<-sd(y)
n<-length(y)
std.error<-the.stv/sqrt(n)
theCI <- qt(0.975,df=n-1)*std.error  #qt(0.025,df=n-1) # same absolute value - t is centered around zero
(lower.CI<-the.mean-theCI)
quantile(sampled.means,0.025)
(upper.CI<-the.mean+theCI)
quantile(sampled.means,0.975)


curve(dnorm(x,the.mean,the.stv),ylim=c(0,0.5),xlim=c(-3,3))
abline(v=lower.CI,col="red")#95% CI
abline(v=upper.CI,col="red")
abline(v=the.mean+1.96*the.stv,lty=2,col="blue")#95% data
abline(v=the.mean-1.96*the.stv,lty=2,col="blue")


cat("\nMean (SE)=",the.mean,"(",the.stv/sqrt(n),")")
cat("\nMean (95%CI)=",the.mean,"(",lower.CI,",",upper.CI,")")


#####
#Coefficient of variation
y1<-rnorm(1000,10,2)#cats &
y2<-rnorm(1000,100,2)#dogs, oh my!
(sd(y1)) #similar sds
(sd(y2))
(CV1<-sd(y1)/mean(y1))
(CV2<-sd(y2)/mean(y2))

y1<-rnorm(1000,10,2)#cats &
y2<-rnorm(1000,100,20)#dogs, oh my!
(sd(y1)) #different sds
(sd(y2))
(CV1<-sd(y1)/mean(y1)) #similar CV
(CV2<-sd(y2)/mean(y2))




### moments
library("e1071")
?skewness

set.seed(3)
curve(dgamma(x,2,1),xlim=c(0,10))
x<-rgamma(10000,2,1)
plot(density(x,adjust=2))

# a<-rnorm(5000,0,2)
# b<-rnorm(1000,-2,4)
# c<-rnorm(3000,4,5)
# skewed<-c(a,b,c)
skewed.mean<-mean(x)
abline(v=skewed.mean,col="orange")
pnorm(skewed.mean,skewed.mean,sd(x))#predicted density if normal
pgamma(skewed.mean,2,1)#actual density to the mean
skewed.median<-quantile(x,0.5)
pgamma(skewed.median,2,1)#median actually does a better job...
abline(v=skewed.median,col="blue")
skewed.sd<-sd(x)

curve(dnorm(x,skewed.mean,skewed.sd),add=TRUE,col="red",lwd=2,lty=2)
skewness(x) #right, or positive skewed

curve(dgamma(x,1.1,1),xlim=c(0,10))
xm<-rgamma(10000,1.1,1)
skewness(xm)

set.seed(3)
qqplot(rnorm(length(x),mean(x),sd(x)),x);abline(0,1)


lx<--x
plot(density(lx,adjust=2))
skewed.mean<-mean(lx)
abline(v=skewed.mean,col="orange")
skewed.sd<-sd(lx)

curve(dnorm(x,skewed.mean,skewed.sd),add=TRUE,col="red",lwd=2,lty=2)
skewness(x) #right, or positive skewed

set.seed(3)
qqplot(rnorm(length(lx),mean(lx),sd(lx)),lx);abline(0,1)




?kurtosis

#leptokurtic

adj.mean1<-sort(rnorm(1000,0,0.1))
adj.mean2<-sort(rnorm(500,0,0.5))
adj.mean3<-sort(rnorm(300,0,0.8))
adj.mean4<-sort(rnorm(200,0,1))
adj.mean5<-sort(rnorm(100,0,1.2))
kurtos<-c(adj.mean1,adj.mean2,adj.mean3,adj.mean4,adj.mean5)


plot(density(kurtos,adj=2))
kurtos.mean<-mean(kurtos)
kurtos.sd<-sd(kurtos)
curve(dnorm(x,kurtos.mean,kurtos.sd),add=TRUE,col="red")
kurtosis(kurtos)

qqplot(rnorm(length(kurtos),mean(kurtos),sd(kurtos)),kurtos);abline(0,1)



#platykurtic

set.seed(2)
kurtos<-rbeta(1000,2,2)

plot(density(kurtos,adj=2),ylim=c(0,2))
kurtos.mean<-mean(kurtos)
kurtos.sd<-sd(kurtos)
curve(dnorm(x,kurtos.mean,kurtos.sd),add=TRUE,col="red")
kurtosis(kurtos)

set.seed(3)
qqplot(rnorm(length(kurtos),mean(kurtos),sd(kurtos)),kurtos);abline(0,1)


################################################################
#
#transformations
#power
set.seed(4)
y<-rnorm(50,10,3)
yy<-y^3

hist(yy)
hist(yy^(1/3),breaks=10)

#### makes sense for some things   e.g., area
set.seed(2)
length<-sort(rnorm(100,10,3))
width<-sort(rnorm(100,30,5))

area<-length*width
hist(area,breaks=20)
plot(length,area)
plot(length,sqrt(area))

little.area<-area[1:50]
large.area<-area[51:100]

boxplot(little.area,large.area)
sd(little.area)
sd(large.area)
sd(sqrt(little.area))
sd(sqrt(large.area))


#log
set.seed(3)
y<-rlnorm(1000,10,1)
hist(y,breaks=50)
hist(log(y+1),breaks=50)

ylog<-log(y+1)#transform
exp(ylog)-1#back transform
####Biostatistics
set.seed(7)


#recip
set.seed(4)
y<-rnorm(1000,10,3)
yy<-1/y

hist(yy,breaks=50)
hist(1/yy,breaks=50)


# arcsin - used to be commonly used for proportion or percentages
set.seed(1)
y<-rbeta(100,3,2)#beta distribution is contrained between 0 and 1
hist(y,breaks=20)

trans.y<-asin(sqrt(y))
hist(trans.y,breaks=15)

set.seed(1)
y<-rbeta(1000,1.1,2)
hist(y,breaks=20)
trans.y<-asin(sqrt(y))
hist(trans.y,breaks=15)

#rank
set.seed(4)
y<-runif(100,10,200)
hist(y,breaks=20)
y.trans<-rank(y)
hist(y.trans,breaks=20)

round.y<-round(y)
length(unique(round.y))
rank(round.y)

cbind(y[order(y)],round.y[order(y)],rank(round.y[order(y)]))

#standardization
y<-rnorm(100,10,4)
hist(y,breaks=20)

mean(y)
sd(y)

#centering
std.var.center<-y-mean(y)
hist(std.var.center,breaks=20)
mean(std.var.center)#new mean is zero - or really really close (computer floating point issue)
sd(std.var.center)#sd is the same as the uncentered

#center and scaling by standard deviation
std.var<-(y-mean(y))/sd(y)
hist(std.var,breaks=20)
mean(std.var)
sd(std.var)

#the function scale() will do this too
mean(scale(y,scale=FALSE))#this will center the data, but not scale by sd
sd(scale(y,scale=FALSE))

#the default setting of scale() will give the z-transformation
mean(scale(y))
sd(scale(y))



