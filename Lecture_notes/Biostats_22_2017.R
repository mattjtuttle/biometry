Present<-0.4
Absent<-1-Present

odd.absent<-Absent/Present #odds ratio


#so, if a pony has a 75% chance of winning a race, the odds are 3:1
#if a pony has 4:1 odds of winning, the probability it wins is 80%
winning<-0.75
losing<-1-winning
winning/losing

Present<-0.4
Absent<-1-Present
odd.absent<-Absent/Present #odds ratio - probability of being present realativ to the probability of being absent
log(odd.absent) #log odds ratio

odds<-NA
absent<-seq(from=0.01,to=0.99,length=1000)
for(i in 1:1000){
	odds[i]<-absent[i]/(1-absent[i])
}

plot(absent,odds)
abline(v=0.6,col="red",lwd=3)#prob absent
abline(h=1.5,col="red",lwd=3)#odds ratio
#zoom in
plot(absent,odds,xlim=c(0.5,0.7),ylim=c(1,2))
abline(v=0.6)#prob absent
abline(h=1.5)#odds ratio


plot(absent,odds,log="y")
abline(v=0.6)#prob absent
abline(h=1.5)#odds ratio

#log of the odds ratio is the logit link function


#logistic fun
b0<-0.0
b1<-0.1

xs<--100:100
ys<-(exp(b0+b1*xs))/(1+exp(b0+b1*xs))#Logistic
plot(xs,ys,type="l")
abline(v=0.0)
abline(h=0.5) # the "LD50" - where the prob is 0.5 of being present or absent

b0<- 0.5 # try different values for the intercept
b1<- -0.03 #try different values for the slope  0.03, negative, etc
xs<--100:100
ys<-(exp(b0+b1*xs))/(1+exp(b0+b1*xs))#Logistic
lines(xs,ys,col="red")





b0<--0.8
b1<-0.1

xs<--100:100
ys<-(exp(b0+b1*xs))/(1+exp(b0+b1*xs))
plot(xs,ys,type="l")
abline(v=-(b0/b1)) #  -(b0/b1)
abline(h=0.5)




#Logistic regression
lord<-c(0,0,0,0,0,0,1,0,1,0,1,1,1,1,0,1,1,1,1,1)
year<-1:20

plot(year,lord)
dumbmod<-lm(lord~year)
anova(dumbmod)
abline(dumbmod)

plot(year,lord)
mod<-glm(lord~year,family="binomial")
summary(mod)
exp(0.4037)#For every year increase, the odds of being dead is increased by 1.5 times

plot(year,-3.8205+0.4037*year)

abline(h=0.4037-3.8205)
abline(h=0.4037-3.8205+0.4037)
abline(h=0.4037-3.8205+0.4037+0.4037)
#again, for every increase in year, dead increased by 0.4037  -  these are in log units

#exponentiate 
plot(year,exp(-3.8205+0.4037*year))
abline(h=exp(0.4037-3.8205))#
abline(h=exp(0.4037-3.8205+0.4037))
abline(h=exp(0.4037-3.8205+0.4037+0.4037))
abline(h=exp(0.4037-3.8205+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037))
abline(h=exp(0.4037-3.8205+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037+0.4037))
#now the odds are in odd ratios

plot(year,lord)
names(mod)
points(year,mod$fitted.values,col="red")
lines(year,predict(mod,list(lord=year),type="response"))

dose.p(mod)#library(MASS)
abline(v=dose.p(mod))
abline(h=0.5)

#library(MASS)
dose.p(mod)
abline(v=dose.p(mod,p=0.95))
abline(h=0.95)










mod<-glm(lord~year,family="binomial")#full
summary(mod)
pchisq(27.526,19,lower.tail=FALSE)
pchisq(15.195,18,lower.tail=FALSE)#fitted values are not significantly different from the observed values
pchisq(40,18,lower.tail=FALSE)#Hypothetically, if our deviance was 40. This would indicate that our observed values deviate from our predicted values as a function of year (i.e., variance changes as a function of the predictor). That's not good.

mod1<-glm(lord~1,family="binomial")#reduced
summary(mod1)#compare AIC with full model
summary(mod)

-2*as.numeric(logLik(mod1))#compare to deviance output
-2*as.numeric(logLik(mod))

pchisq(mod$null.deviance-mod$deviance,1,lower.tail=FALSE)
anova(mod,mod1)

logLik(mod1)
logLik(mod)

logLik(mod1)-logLik(mod)
27.526-15.195#same difference as the difference in log likelihoods*2   #(logLik(mod1)-logLik(mod))*2
2*(logLik(mod1)-logLik(mod))

D <- -2*log(exp(as.numeric(logLik(mod1)))/exp(as.numeric(logLik(mod))))

D <- -2*as.numeric(logLik(mod1))+2*as.numeric(logLik(mod))# 2*(logLik(mod1)-logLik(mod))

pchisq(D,1,lower.tail=FALSE)

library(car)
Anova(mod)






dat<-read.csv("ClutchData.csv")

propSurv<-(dat$Clutch-dat$Mort)/dat$Clutch
plot(dat$Clutch[dat$Treat==1],propSurv[dat$Treat==1],xlab="ClutchSize",ylab="proportion survive")
modl<-lm(propSurv[dat$Treat==1]~dat$Clutch[dat$Treat==1])
anova(modl)
summary(modl)
abline(modl)

#GLM
y<-cbind(dat$Clutch-dat$Mort,dat$Mort)

mod<-glm(y~dat$Clutch,family="binomial")
summary(mod)


modfull<-glm(y~dat$Clutch*as.factor(dat$Treat),family="binomial")
summary(modfull)
pchisq(299.25,123,lower.tail=FALSE)#poor job of predicting - interaction term a problem


Clutcht<-dat$Clutch[dat$Treat==0]#NOT exposed to predators
modt<-glm(y[dat$Treat==0,]~Clutcht,family="binomial")
summary(modt)
hist(Clutcht)
hist(log(Clutcht))
modtl<-glm(y[dat$Treat==0,]~log(Clutcht),family="binomial")
summary(modtl)
pchisq(87.33,51,lower.tail=FALSE)

Clutchc<-dat$Clutch[dat$Treat==1]
modc<-glm(y[dat$Treat==1,]~Clutchc,family="binomial")
summary(modc)
hist(Clutchc)
hist(log(Clutchc))
modcl<-glm(y[dat$Treat==1,]~log(Clutchc),family="binomial")
summary(modcl)
pchisq(189.55,72,lower.tail=FALSE)

1-(189.55/229.16)#this is how much of the variation we're explaining - 
#actually it's the R^2






?zeroinfl
modzi<-glm.nb(y~dat$Clutch+as.factor(dat$Treat))



library(gam)
modgam<-gam(y[dat$Treat==1,]~s(log(Clutchc)),family="binomial")
summary(modgam)


##########
#
library(MASS)
exp(dose.p(modcl))


xv<-seq(0,30,length=length(dat$Clutch))
p<-(dat$Clutch-dat$Mort)/dat$Clutch
plot(log(dat$Clutch[dat$Treat==1]),p[dat$Treat==1],ylab="",type="n",xaxt="n",yaxt="n",xlab="",cex=2,las=1)
mtext("Clutch size",side=1,cex=1.25,line=2)
mtext("Probability of surviving",side=2,cex=1.25,line=2.25)
axis(side=2,las=1,label=FALSE)
mtext(c("1.0","0.8","0.6","0.4","0.2","0.0"),at=c(1,0.8,0.6,0.4,0.2,0.0),side=2,las=1,line=0.75,cex=1)
axis(side=1,at=log(c(1,2,5,10,20)),labels=FALSE)
mtext(c("1","2","5","10","20"),at=log(c(1,2,5,10,20)),side=1,line=0.5,cex=1)
points(log(dat$Clutch[dat$Treat==1]),p[dat$Treat==1])
lines(xv,predict(modtl,list(Clutcht=exp(xv)),type="response"))
points(log(dat$Clutch[dat$Treat==0]),p[dat$Treat==0],pch=19)
lines(xv,predict(modcl,list(Clutchc=exp(xv)),type="response"))


######################

######################

set.seed(6)
pz<-seq(from=0.1,to=0.9,length=100)
clutchsize<-rep(sort(rpois(100,10)),2)
treatment<-c(rep("control",100),rep("exclusion",100))
dat<-NA
count<-NA

for (i in 1:length(pz)){
	count[i]<-sum(rpois(2,3))
	dat[i]<-sum(rbinom(count[i],1,pz[i]))
}

for(i in 101:200){
	count[i]<-sum(rpois(2,3))
	dat[i]<-sum(rbinom(count[i],1,0.8))
}



newdat<-cbind(dat,count-dat);colnames(newdat)<-c("live","dead")

dat<-data.frame(newdat,clutchsize,treatment)
save(dat,file="clutchdata.R")

mod<-glm(newdat~clutchsize*treatment,family="binomial")
summary(mod)
Anova(mod)

clutchc<-clutchsize[1:100]
newdatc<-newdat[1:100,]
modc<-glm(newdatc~clutchc,family="binomial")
summary(modc)

pchisq(109.33,98,lower.tail=FALSE)
1-(109.33/234.55)#this is how much of the variation we're explaining - actually it's the R^2
Anova(modc)


clutcht<-clutchsize[101:200]
newdatt<-newdat[101:200,]
modt<-glm(newdatt~clutcht,family="binomial")
summary(modt)
pchisq(112.45,98,lower.tail=FALSE)
Anova(modt)

xv<-seq(0,20,length=100)
p<-(apply(newdat,1,sum)-newdat[,2])/apply(newdat,1,sum)
plot(clutchsize[1:100],p[1:100],ylab="",type="n",xaxt="n",yaxt="n",xlab="",cex=2,las=1,xlim=c(0,20))
mtext("Clutch size",side=1,cex=1.25,line=2)
mtext("Probability of surviving",side=2,cex=1.25,line=2.25)
axis(side=2,las=1,label=FALSE)
mtext(c("1.0","0.8","0.6","0.4","0.2","0.0"),at=c(1,0.8,0.6,0.4,0.2,0.0),side=2,las=1,line=0.75,cex=1)
axis(side=1)
#mtext(c("1","2","5","10","20"),at=c(1,2,5,10,20),side=1,line=0.5,cex=1)
points(clutchsize[1:100],p[1:100])
lines(xv,predict(modc,list(clutchc=xv),type="response"))
points(clutcht,p[101:200],pch=19)
lines(xv,predict(modt,list(clutcht=xv),type="response"))





############
set.seed(9)
dat<-rpois(50,lambda=2)
adist<-1:50


hist(lm(dat~adist)$residuals)


mod<-glm(dat~adist,family="poisson")
summary(mod)

plot(adist,dat)
predict(mod,type="response")


load("poisfuN.R")



head(dat)
plot(numberCrayfish~cover,data=dat)


mod<-glm(numberCrayfish~cover,family="poisson",data=dat)
summary(mod)
Anova(mod)
predict(mod,type="response")
plot(numberCrayfish~cover,data=dat)
points(dat[,3],predict(mod,type="response"),pch=19,col="red")

abline(h=exp(-5.333532+0.086166*40))
abline(h=exp(-5.333532+0.086166*50))

mod1<-glm(numberCrayfish~cover+as.factor(habitat),family="poisson",data=dat)
summary(mod1)
plot(numberCrayfish~cover,data=dat)
points(dat[,3],exp(-5.24712+0.07015*dat$cover+1.08386*0+ 0.36981*0),col="red", pch=20)
points(dat[,3],exp(-5.24712+0.07015*dat$cover+1.08386*1+ 0.36981*0),col="blue",pch=20)# 2.96% increase in # over H1
points(dat[,3],exp(-5.24712+0.07015*dat$cover+1.08386*0+ 0.36981*1),col="darkgreen",pch=20) 

exp(0.36981)
#1.44746% increase in # over benthic



exp(0.36981)



