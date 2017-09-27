
################
#Covariance

length.cm<-c(64,69,71,67,63,62,66,60,65,68)
weight.g<-c(130,148,180,175,121,127,141,118,120,159)

cbind(length.cm,weight.g)

plot(length.cm,weight.g,pch=19,las=1);points(mean(length.cm),mean(weight.g),pch=19,cex=2,col="red")

length.minus.mean<-length.cm-mean(length.cm)
weight.minus.mean<-weight.g-mean(weight.g)

cbind(length.cm,weight.g,length.minus.mean,weight.minus.mean)

product<-length.minus.mean*weight.minus.mean

cbind(length.cm,weight.g,length.minus.mean,weight.minus.mean,product)


sum.product<-sum(product)
n<-length(product)
Covariance <- sum.product/(n-1)
cov(length.cm,weight.g)

length.in<-length.cm/2.54
weight.lbs<-weight.g*0.00220462
plot(length.in,weight.lbs,pch=19,las=1);#plot(length.cm,weight.g,pch=19,las=1)
cov(length.in,weight.lbs)

length.ft<-length.in/12
cov(length.ft,weight.lbs)

#Put it in z units
#covariance of z-transformed data IS the correlation # note: this IS NOT the Fisher's r-z transformation
z.length<-(length.cm-mean(length.cm))/ sd(length.cm)
z.weight<-(weight.g-mean(length.cm))/sd(weight.g)

cov(z.length,z.weight)
cor(length.cm,weight.g)

cov(scale(length.cm),scale(weight.lbs))
cov(scale(length.in),scale(weight.lbs))







cor(weight.g,length.cm)

SEr <- sqrt((1-cor(weight.g,length.cm)^2 )/(10-2))
t.stat <- cor(weight.g,length.cm)/SEr
one.tailed.p<-pt(t.stat,df=8,lower.tail=FALSE)

t.stat.n <- (cor(weight.g,length.cm)-0.6)/SEr
one.tailed.pn<-pt(t.stat.n,df=8,lower.tail=FALSE)


one.tailed.p<-1-pt(t.stat,df=10-2);
curve(dt(x,df=10-2),xlim=c(-5,5),ylim=c(0,0.5))
abline(v=0,lty=2,col="red",lwd=3)
text(-3,0.4,"50%",cex=4)
text(3,0.4,"50%",cex=4)

curve(dt(x,df=10-2),xlim=c(-5,5),ylim=c(0,0.5))
abline(v=t.stat,lty=2,col="red",lwd=3)
text(0,0.45,paste(round(pt(t.stat,df=10-2)*100,digits=3),"%"),cex=2)
text(4.9,0.3,paste(round((1-pt(t.stat,df=10-2))*100,digits=3),"%"),cex=2,srt=270)


curve(pt(x,df=10-2),xlim=c(-4,5),ylim=c(0,1))
abline(v=0,lty=2,lwd=4,col="red")
text(-1,0.8,"50%",cex=3)
text(1,0.8,"50%",cex=3)



curve(pt(x,df=10-2),xlim=c(-2,5),ylim=c(0,1))
abline(v=t.stat,lty=3,lwd=4,col="red")
p.value<-2*one.tailed.p
abline(h=pt(t.stat,df=10-2),lty=2,lwd=3,col="blue")
arrows(3,0.6,4,pt(t.stat,df=10-2)-0.05)
arrows(3,0.5,4,0.1)
text(3,0.55,paste("p=",round(pt(t.stat,df=10-2),digits=4)),cex=2)

one.tailed.p<-pt(t.stat,df=10-2,lower.tail=FALSE)
two.tailed.p.value<-2*one.tailed.p

Correlation<-cor(weight.g,length.cm)
plot(length.cm,weight.g,pch=19,las=1)
text(68,135,paste("r=",round(Correlation,digits=5)),cex=1)
text(68,130,paste("p=",round(p.value,digits=4)),cex=1)

#simulate the null
set.seed(3)
corr<-NA
peez<-NA
teez<-NA
for(i in 1:10000){
x<-rnorm(50);y<-rnorm(50)
corr[i]<-cor(x,y)
peez[i]<-cor.test(x,y)$p.value
teez[i]<-cor.test(x,y)$statistic
}
hist(corr)#plot(density(corr))#
hist(peez)
quantile(peez,c(0.025,0.975))
hist(teez)
curve(dt(x,df=50-2),xlim=c(-5,5),ylim=c(0,0.5))
abline(v=qt(0.5,48))
abline(v=qt(0.45,48))
abline(v=qt(0.4,48))
abline(v=qt(0.3,48))
abline(v=qt(0.2,48))
abline(v=qt(0.1,48))
abline(v=qt(0.05,48))

###power
library(pwr)
pwr.r.test(n=10,r=cor(weight.g,length.cm),sig.level=0.05)
#power as function of sample size


power<-NA
for (n in 4:100){
	power[n]<-pwr.r.test(n=n,r=cor(weight.g,length.cm),sig.level=0.05)$power
	}
plot(4:100,power[4:100],las=1,type="l",xlab="n",ylab="power")
abline(h=0.80,col="red",lty=2,lwd=3)
abline(v=10,col="blue",lty=3,lwd=3)

pwr.r.test(r=cor(weight.g,length.cm),sig.level=0.05,power=0.8) # based on our observed difference (effect size), this tells us how many replicates are needed
points(7.69,0.8,cex=2)

#power as a function of effect size
power<-NA
effectsize<-seq(from=0.1,to=0.9,length=100)
#ds<-seq(from=d,to=1.3,length=100)
for (es in 1:100){
	power[es]<-pwr.r.test(n=10,r=effectsize[es],sig.level=0.05)$power
	}
plot(effectsize,power,las=1,type="l",xlab="effect size",ylab="power",main="n=10")
abline(h=0.8,col="red",lty=2)
cbind(effectsize,power)




pwr.r.test(n=10,sig.level=0.05,power=0.8)

#effect size and replicates
effectsize<-NA
power=0.80
for(n in 4:1000){
	effectsize[n]<-pwr.r.test(n=n,power=power,sig.level=0.05)$r
	}


plot(1:1000,effectsize,las=1,type="l",xlab="No. of observations",ylab="Effect size",main="Power=0.8")
cbind(1:1000,effectsize)



hist(peez)



set.seed(3)
corr<-NA
peez<-NA
teez<-NA
for(i in 1:8500){
x<-rnorm(50);y<-rnorm(50)
corr[i]<-cor(x,y)
peez[i]<-cor.test(x,y)$p.value
teez[i]<-cor.test(x,y)$statistic
}


#simulate where we know that 15% should be significant
library(MASS)
for(i in 8501:10000){
mu<-rep(0,2)
Sigma<-matrix(0.3,nrow=2,ncol=2)+diag(2)*0.7
rawvars<-mvrnorm(n=50,mu=mu,Sigma=Sigma)
corr[i]<-cor(rawvars[,1],rawvars[,2])
peez[i]<-cor.test(rawvars[,1],rawvars[,2])$p.value
teez[i]<-cor.test(rawvars[,1],rawvars[,2])$statistic
}

library(pwr)
pwr.r.test(n=50,r=0.3,sig.level=0.05)


sum(peez[8501:10000]<0.05)/1500

### 10000 test
### 1500 should have a "real" effect
########power is around 0.57, so we expect 0.57*1500 = 855 to be true positive
########                                   1500-855 =  645 to be false negative      

### 8500 should have no effect
########sig level is 0.05, so we expect 0.05*8500 = 425 to be false positive
########                                0.95*8500 = 8075 to be true negative

### total number of positives = 855+425 = 1280
### our false discovery rate is 425/1280 = 0.332, 33% of our results are wrong!!!

# FDR = sig.lev*(1-Prob(real)) / (Power*Prob(real) + sig.lev*(1-Prob(real)))

0.05*(1-0.15)/(0.57*0.15+0.05*(1-0.15))

quartz(width=5,height=5)
par(mar=c(0,0,0,0))
plot(0:1,0:1,type="n",ylab="",xlab="",yaxt="",xaxt="n")

polygon(x=c(0.05,0.25,0.25,0.05),y=c(.4,0.4,0.6,0.6));text(0.15,0.5,"10,000 tests")
polygon(x=c(0.3,0.6,0.6,0.3),y=c(0.65,0.65,0.85,0.85));text(0.45,0.75,"'Real effect' in 1500\n15%",cex=0.9)
polygon(x=c(0.3,0.6,0.6,0.3),y=c(0.15,0.15,0.35,0.35));text(0.45,0.25,"No effect in 8500\n85%")
polygon(x=c(0.7,0.95,0.95,0.7),y=c(0.8,0.8,0.95,0.95));text(0.82,0.87,"~57%\n true positive\n855")
polygon(x=c(0.7,0.95,0.95,0.7),y=c(0.55,0.55,0.7,0.7));text(0.82,0.63,"1500-855=\n645\n false negative")
polygon(x=c(0.7,0.95,0.95,0.7),y=c(0.45,0.45,0.3,0.3));text(0.82,0.37,"95%\n true negative\n 8075")
polygon(x=c(0.7,0.95,0.95,0.7),y=c(0.2,0.2,0.05,0.05));text(0.82,0.13,"5%\n false postive\n 425")

segments(0.25,0.6,0.3,0.65)
segments(0.25,0.4,0.3,0.35)
segments(0.6,0.8,0.7,0.9)
segments(0.6,0.7,0.7,0.6)

segments(0.6,0.3,0.7,0.4)
segments(0.6,0.2,0.7,0.1)

text(0.5,0.4,"Sig. level = 0.05",cex=1.2)
text(0.5,0.9,"Power ~ 0.57",cex=1.2)


########
## 95 CI for r
#########
length.cm<-c(64,69,71,67,63,62,66,60,65,68)
weight.g<-c(130,148,180,175,120,127,141,118,120,159)

r<-cor(length.cm,weight.g)#r=0.8
n<-10

#Fisher's r-z transformation
z<-0.5*log((1+r)/(1-r))
sigmaz<-sqrt(1/(n-3))
upperz<-z+sigmaz*1.96#
lowerz<-z-sigmaz*1.96#

upperz<-z+sigmaz*qnorm(0.975)#
lowerz<-z-sigmaz*qnorm(0.975)#


#translate back to r

R<-(exp(2*z)-1)/(exp(2*z)+1)
upper<-(exp(2*upperz)-1)/(exp(2*upperz)+1)
lower<-(exp(2*lowerz)-1)/(exp(2*lowerz)+1)

#calculate p-value
sr<-sqrt((1-R^2)/(n-2))
tstat<-R/sr
pval<-pt(tstat,n-2,lower.tail=FALSE)*2


cat("\n\n\nr=",r,"\nupper 95% CI",upper,"\nlower 95% CI",lower,"\n\nstandard error of r:",sr,"\n\nt =",tstat,"\ndf=",n-2,"\np=",pval,"\n\n")

cor(length.cm,weight.g)
cor.test(length.cm,weight.g)



####So with r and n, we can test hypotheses about r
# suppose n=20 and r=0.8
#95% CI of r
z<-0.5*log((1+0.8)/(1-0.8))
sigma.z <- sqrt(1/(20-3))

upper <- z + sigma.z*1.96
lower <- z - sigma.z*1.96

# translate back to r
upper.r<-(exp(2*upper )-1)/(exp(2*upper )+1)
lower.r<-(exp(2*lower)-1)/(exp(2*lower)+1)

# calculate t
se.r<-sqrt((1-0.8^2)/18)
t.r <- 0.8/ se.r
pt(t.r,18,lower.tail=FALSE)*2