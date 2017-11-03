##############################
#Ancova
# Response = Weight; Two diets (A & B), amount of diet consumed, and rearing temperature

Diet<-c('A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B')
DietConsumed<-c(3.6,5.3,4.1,2.9,3.2,1.9,5.6,5.7,4.6,2.8,2.5,2.4,2.6,2.8,2.7,3.5,4.6,4.2,3.6,3.9,4.2,4.1,2.6,3.1,3.9,5.6,5.8,6.8,1.6,5.6,3.5,2.4,2.3,5.5,4.7,3.5,4.2,2.1,2,3.6,5,2.1,2.8,2.6,2.7,2.9,3.8,4.2,3,2)
Temperature<-c(25,25.6,26.4,26.3,25.1,26.9,23.8,26.5,26.6,27.4,25.8,26.6,25.5,27.6,26.9,27.4,25.5,27.3,27.4,25.5,25,26.2,26.7,26.8,27.4,26.3,26.9,25.6,25.6,26,25.5,27.5,25.9,26.2,25.1,25.3,25.8,26.3,26,27,27.9,26.1,26.1,24.6,24.6,26,24.8,27,24.1,26.5)
Weight<-c(6.2,10.2,8.4,4.4,4.9,3.9,10.5,12.3,9.5,6.2,5.3,4.1,5,5.2,3,7.3,9.3,8.6,7.6,8.1,8.6,8.9,6.9,6.3,8.1,7.9,7.1,8.2,3.6,5.9,7,4.5,4.6,7.1,6.6,5.2,6.3,4.6,3.6,5.9,7.2,3.9,4.6,4.2,3.9,4.9,5.6,6.3,5.9,3.9)

dat<-data.frame(Diet,DietConsumed,Temperature,Weight)

library(car)
mod<-lm(Weight~Diet,data=dat)#simplest model, only looking at diet
Anova(mod)

mod<-lm(Weight~Diet+DietConsumed,data=dat)#add covariate diet consumed
#this is why it's called a "general" linear model - both continuous and nomial factors
Anova(mod)
plot(Weight~DietConsumed,data=dat)
mod
abline(a=2.469,b=1.295)
abline(a=2.469-1.607,b=1.295)#the intercept for DietB is 1.607 less than that for DietA
# note: here we are modelling it as having the same slope
#You would get a very similar answer if you ran the regression and then compared the diets based only on the residuals - but, you would need to keep track of your degrees of freedom.


mod.reg<-lm(Weight~DietConsumed)
mod.res<-lm(mod.reg$residuals~Diet)
mod.res
Anova(mod.res) #
mod#intercept different, but slope the same
Anova(mod) #note degrees of freedom...


# We can test this assumption by adding an interaction term
#significant interaction term
mod<-lm(Weight~Diet*DietConsumed,data=dat)#
#the above is a shortcut - the cumbersome way of doing the same thing..
mod<-lm(Weight~Diet+DietConsumed+Diet:DietConsumed,data=dat)

Anova(mod)# significant interaction term indicates that the slopes are not equal for dietA and dietB
# the significant interactions changes the interpretation of the coefficients
# With no interaction term, our slope for DietConsumed is the unique effect of diet consumed on weight
# The significant interaction means the effect of diet consumed is different for the different diets
#Now our slope is the unique effect of DietConsumed on Diet when diet = 0 (DietA: recall dummy coding)
#The slope describing the effect of dietconsumed on DietB is the slope for diet consumed - the interaction slope is added to the slope (for DietA) to get the slope for dietconsumed on DietB

# So, we run a different model for each diet
modA<-lm(Weight[Diet=="A"]~DietConsumed[Diet=="A"],data=dat)
modB<-lm(Weight[Diet=="B"]~DietConsumed[Diet=="B"],data=dat)
mod
2.1368-1.2609 #numbers from coefficients of full mod : compare to slope for modB
diff(c(-0.5747,2.3761))#compare to slope of DietB in full model - The difference between the intercepts is the effect of diet
#Hypothesis testing on each of the different models
Anova(modA)
Anova(modB)



plot(Weight~DietConsumed,data=dat)
abline(modA,col="red")
abline(modB,col="blue")

modR<-lm(Weight~DietConsumed,data=dat)# model without the covariate (diet)
abline(modR,lty=2,col="green")

#These are the parameters from lm(Weight~Diet+DietConsumed,data=dat)
abline(a=2.469,b=1.295)
abline(a=2.469-1.607,b=1.295)#the intercept for DietB is 1.607 less than that for DietA



#Investigating temperature
mod<-lm(Weight~Diet*DietConsumed*Temperature,data=dat)
Anova(mod)
step(mod)
# temperature - we might drop it to conserve degrees of freedom - with and without temperature are in the "family" of best models

#drop it
mod<-lm(Weight~Diet*DietConsumed,data=dat)
Anova(mod)
#or not (it's your model)
mod<-lm(Weight~Temperature+Diet*DietConsumed,data=dat)
Anova(mod)

mm<-model.matrix(mod)
plot(Weight~mm[,5])

################
#Other More complex ANOVAs
library(car)

treats<-as.factor(c(rep("A",10),rep("B",10),rep("C",10)))#
temperature<-as.factor(rep(c(rep("high",5),rep("low",5)),3))#
set.seed(10);mass<-rnorm(30,20,3)#;modinteraction<-lm(response~treats*temperature);anova(modinteraction)#this will give sig int
data.frame(treats,temperature,mass)


mod<-lm(mass~treats+temperature)
Anova(mod)
boxplot(mass~treats+temperature)


modinteraction<-lm(mass~treats+temperature+treats:temperature)

Anova(modinteraction) #significant interaction (note it looks like temperature isn't important if we interpret the p values associated with the main effects)



#Looking at the effect of treats at high and low temperature
interaction.plot(x.factor=temperature,trace.factor=treats,response=mass)
points(1,mean(mass[temperature=="high"]),col="red",pch=19,cex=3)#showing the means for treats2
points(2,mean(mass[temperature=="low"]),col="red",pch=19,cex=3)

lowmod<-lm(mass[temperature=="low"]~treats[temperature=="low"])
Anova(lowmod)
highmod<-lm(mass[temperature=="high"]~treats[temperature=="high"])
Anova(highmod)



#Looking at the effect of temperature across diets
interaction.plot(x.factor=treats,trace.factor=temperature,response=mass)
points(1,mean(mass[treats=="A"]),col="red",pch=19,cex=3)#showing the means for treats
points(2,mean(mass[treats=="B"]),col="red",pch=19,cex=3)
points(3,mean(mass[treats=="C"]),col="red",pch=19,cex=3)

Amod<-lm(mass[treats=="A"]~temperature[treats=="A"])
Anova(Amod)#temperature not important on dietA
boxplot(mass[treats=="A"]~temperature[treats=="A"])
Bmod<-lm(mass[treats=="B"]~temperature[treats=="B"])
Anova(Bmod)#temperature not important on dietB
boxplot(mass[treats=="B"]~temperature[treats=="B"])
Cmod<-lm(mass[treats=="C"]~temperature[treats=="C"])
Anova(Cmod)#temperature important on dietC
boxplot(mass[treats=="C"]~temperature[treats=="C"])


### Ssome scenarios
quartz(width=5,height=5)
par(mfrow=c(2,2),mar=c(2,1,1,1))
plot(0:1,0:1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
mtext(c("H","L"),at=c(0.1,0.7),side=1)
segments(0.1,0.9,0.7,0.1,lty=1)
segments(0.7,0.9,0.1,0.1,lty=2)
segments(0.1,0.5,0.7,0.5,lty=3)
text(0.8,0.8,"treats:NS")
text(0.8,0.7,"temp:NS")
text(0.8,0.6,"interaction:Sig")

plot(0:1,0:1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
mtext(c("H","L"),at=c(0.1,0.7),side=1)
segments(0.1,0.9,0.7,0.5,lty=1)
segments(0.1,0.1,0.7,0.5,lty=2)
segments(0.1,0.5,0.7,0.5,lty=3)
text(0.8,0.8,"treats:Sig")
text(0.8,0.7,"temp:NS")
text(0.8,0.6,"interaction:Sig")

plot(0:1,0:1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
mtext(c("H","L"),at=c(0.1,0.7),side=1)
segments(0.1,0.9,0.7,0.9,lty=1)
segments(0.1,0.5,0.7,0.5,lty=2)
segments(0.1,0.1,0.7,0.1,lty=3)
text(0.8,0.8,"treats:Sig")
text(0.8,0.7,"temp:NS")
text(0.8,0.6,"interaction:NS")

plot(0:1,0:1,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
mtext(c("H","L"),at=c(0.1,0.7),side=1)
segments(0.1,0.7,0.7,0.9,lty=1)
segments(0.1,0.4,0.7,0.6,lty=2)
segments(0.1,0.1,0.7,0.3,lty=3)
text(0.8,0.8,"treats:Sig")
text(0.8,0.7,"temp:Sig")
text(0.8,0.6,"interaction:NS")
