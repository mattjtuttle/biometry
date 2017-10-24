# MULTIPLE REGRESSION
library(fBasics)

SO2<-c(65,26,69,61,94,10,18,9,10,28,31,26,29,31,16,10,13,12,17,56,36,29,14,10,24,110,28,17,8,30,9,47,35,29,14,56,14,11,46,11,23)
hist(SO2)
normalTest(SO2,"da")
SO2<-log(SO2)
normalTest(SO2,"da")

AveTemp<-c(49.7,51.5,54.6,50.4,50,61.6,59.4,66.2,68.9,51,59.3,57.8,51.1,55.2,45.7,70.3,61,56.7,51.9,49.1,54,57.3,68.4,75.5,61.5,50.6,52.3,49,56.6,55.6,68.3,55,49.9,43.5,54.5,55.9,51.5,56.8,47.6,47.1,54)
hist(AveTemp)
normalTest(AveTemp,"da")
normalTest(log(AveTemp),"da")
AveTemp<-log(AveTemp)

Num.factories<-c(1007,266,1692,347,343,337,275,641,721,137,96,197,379,35,569,213,91,453,454,412,80,434,136,207,368,3344,361,104,125,291,204,625,1064,699,381,775,181,46,44,391,462)
hist(Num.factories)
normalTest(Num.factories,"da")
normalTest(log(Num.factories),"da")
Num.factories<-log(Num.factories)

Population<-c(751,540,1950,520,179,624,448,844,1233,176,308,299,531,71,717,582,132,716,515,158,80,757,529,335,497,3369,746,201,277,593,361,905,1513,744,507,622,347,244,116,463,453)
hist(Population)
normalTest(Population,"da")
normalTest(log(Population),"da")
Population<-log(Population)

################model selection
#the more stuff you put in a model, the greater your r^2... but, that's not always a good thing, since the more parameters you put in a model, the greater your uncertainty will be around each parameter estimate (parameters 'eat up' degrees of freedom)

###adjusted r-squared # sometimes used as a tie breaker between "best" models
mod1.1<-lm(SO2~AveTemp);
summary(mod1.1)
anova(mod1.1)
1-(0.3611/var(SO2))#adjusted r squared
mod1.2<-lm(SO2~Num.factories);
summary(mod1.2)
mod1.3<-lm(SO2~Population);
summary(mod1.3)

mod2.1<-lm(SO2~AveTemp+Num.factories);
summary(mod2.1)
mod2.2<-lm(SO2~AveTemp+Population);
summary(mod2.2)
mod2.3<-lm(SO2~Num.factories+Population);
summary(mod2.3)
mod3.1<-lm(SO2~AveTemp+Num.factories+Population)
summary(mod3.1)
1-0.3201/var(SO2)#adjusted r squared


#garbage will increase r^2, but not adjusted r^2
#adjusted r^2 tells you the % of the variation explained by independent variables that actually matter
set.seed(1)
junk1<-rnorm(41)
junk2<-rnorm(41)
mod.dumb<-mod3.1<-lm(SO2~AveTemp+Num.factories+Population+junk1+junk2)
summary(mod.dumb)


model<-lm(SO2~AveTemp+Num.factories+Population)
logLik(model)


2*5-2*logLik(model)[1]
AIC(model)
AIC(model,k=log(41)) # this is the BIC


mod1.1<-lm(SO2~AveTemp);AIC(mod1.1)
mod1.2<-lm(SO2~Num.factories);AIC(mod1.2)
mod1.3<-lm(SO2~Population);AIC(mod1.3)

mod2.1<-lm(SO2~AveTemp+Num.factories);AIC(mod2.1)
mod2.2<-lm(SO2~AveTemp+Population);AIC(mod2.2)
mod2.3<-lm(SO2~Num.factories+Population);AIC(mod2.3)

mod3.1<-lm(SO2~AveTemp+Num.factories+Population);AIC(mod3.1)
#stepAIC(mod3.1)
stepAIC(model)#pretty much the same function as step

AIC(mod3.1)
extractAIC(mod3.1)
AIC(mod2.1)
extractAIC(mod2.1)
# different values because AIC considers the estimating the error as a parameter, extractAIC does not
#Note: this won't affect model selection
#-42.91013--41.71219 = 75.44283-76.64077

mod<-lm(SO2~AveTemp+Num.factories+Population)
step(mod)
summary(mod)

step(mod,SO2~AveTemp+Num.factories+Population,direction="backward")
step(mod,SO2~.,direction="backward")#does the same as above with less typing
# If we choose to use BIC because we only have 41 observations
step(mod,SO2~.,direction="backward",k=log(41))#BIC - note you get a different "best model" because BIC is penalizing you more for smaller sample size

step(lm(SO2~Num.factories),scope=list(upper=mod,lower=~1),direction="forward")
step(lm(SO2~1),scope=list(upper=mod,lower=~1),direction="forward")#start with just the intercept


set.seed(1)
junk1<-rnorm(41)
junk2<-rnorm(41)

sillymodel<-lm(SO2~AveTemp+Num.factories+Population+junk1+junk2)
#extractAIC(sillymodel)

step(sillymodel,direction="backward")

step(lm(SO2~1),scope=list(upper=sillymodel,lower=~1),direction="forward")





library(MASS)


#####  t thinking

dat<-c(3,5,4,7,5,6,8,9,6,4,5,2,5,6,9,6,4,5,7)

theta<-0

t.stat<-(mean(dat)-theta)/(sd(dat)/sqrt(length(dat)))

p.val<-(1-pt(q=t.stat,df=length(dat)-1))*2

pt(q=t.stat,df=length(dat)-1,lower.tail=FALSE)*2

t.test(dat,mu=0)

#could test it with theta of any value (e.g., 5)


########
#paired t test
control<-c(3,5,4,7,5,6,8,9,6,4,5,2,5,6,9,6,4,5,7)
treatment<-c(10,7,8,6,0,8,5,4,8,5,6,8,7,9,4,5,12,9,5)
cbind(control,treatment)
diff<-control-treatment


mean(diff)
sd(diff)/sqrt(length(diff))


t.test(control,treatment,paired=TRUE)
t.test(diff,mu=0)
###############


x1<-c(4,6,8,8,5,9,6,12,7,3)
x2<-c(9,8,11,14,9,10,6,13,12,8)

SSx1<-sum((x1-mean(x1))^2)
SSx2<-sum((x2-mean(x2))^2)
DF<-length(x1)+length(x2)-2

s.pooled<-sqrt((SSx1+SSx2)/DF)

t.stat<-(mean(x1)-mean(x2))/(s.pooled*sqrt((1/length(x1))+(1/length(x2))))
(pt(t.stat,18))*2 #we don't say lower.tail=TRUE here because t < 0

t.test(x1,x2,var.equal=TRUE)
t.test(x1,x2)#Welch, or Welch-Satterthwaite correct - adjusts degrees of freedom when variances are unequal


#Welch-Satterthwaite ###
x1sd<-sd(x1)
x2sd<-sd(x2)

numer<-((x1sd^2/length(x1))+(x2sd^2/length(x2)))^2
denom<-(1/(length(x1)-1))*(x1sd^2/length(x1))^2 + (1/(length(x2)-1))*(x2sd^2/length(x2))^2
numer/denom

######

set.seed(5)
x<-rnorm(50)
y<-rnorm(50,mean=2.5,sd=10)
t.test(x,y,var.equal=TRUE)
t.test(x,y)