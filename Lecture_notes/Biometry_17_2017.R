A<-c(56,63,45,41,71,60,78,50,68,62)
B<-c(40,48,60,38,32,44,66,22,45,54)
C<-c(71,57,64,44,73,50,79,67,84,61)
t.test(A,B)
t.test(B,C)
t.test(A,C)
p.adjust(c(0.01735,0.002458,0.3139),"bonferroni")


treats<-c(rep("A",length(A)),rep("B",length(B)),rep("C",length(C)))
response<-c(A,B,C)
as.data.frame(cbind(treats,response))

boxplot(c(A,B,C)~treats,las=1)



meanA<-mean(A)
meanB<-mean(B)
meanC<-mean(C)
meanGrand<-mean(c(A,B,C))


#Variability between groups -or- SS among groups
SS<-10*(meanA-meanGrand)^2+10*(meanB-meanGrand)^2+10*(meanC-meanGrand)^2



ssA<-sum((A-meanA)^2)
ssB<-sum((B-meanB)^2)
ssC<-sum((C-meanC)^2)
#Variability within treatments -or- SS within groups
SSresid<-sum(ssA,ssB,ssC)

#Total variation
SStotal<-sum((c(A,B,C)-meanGrand)^2)



#Calculate MS as before
MSfactors<-SS/2
MSerror<-SSresid/27
Fval<-MSfactors/MSerror
pf(Fval,2,27,lower.tail=FALSE)


mod<-lm((c(A,B,C)~as.factor(treats)))
anova(mod)
mod
mean(c(A)) # note that this is the intercept
# that's because the computer is using dummy coding
summary(mod)# note the standard errors for the estimates are the same - because its assuming a common variance (assumption of homogeneity of variances)
#standard error for intercept -  sqrt(MSerror/10)
#standard error for intercept -  sqrt(MSerror/(10/2)) # splitting the number of observations by number of remaining factors, in this case 2
library(car)





#dummy coding
d1<-c(rep(1,10),rep(0,10),rep(0,10))
d2<-c(rep(0,10),rep(1,10),rep(0,10))
d3<-c(rep(0,10),rep(0,10),rep(1,10))

data.frame(cbind(treats,d1,d2,d3))
model.matrix(response~0+treats)#linear effects model dummy coding
#actually.....
mu<-rep(1,30)
cbind(mu,model.matrix(response~0+treats))#design matrix for linear effects model



model.matrix(lm(response~treats))#cell means model dummy coding


datwithdummy<-cbind(response,d1,d2,d3)
plot(rep(1,30),apply(datwithdummy[,1:2],1,prod),xlim=c(0,1),ylim=c(5,100))
points(rep(1,30),apply(datwithdummy[,c(1,3)],1,prod),col="red")
points(rep(1,30),apply(datwithdummy[,c(1,4)],1,prod),col="blue")
points(1,mean(response[d1==1]),pch=19,cex=2)
points(1,mean(response[d2==1]),pch=19,cex=2,col="red")
points(1,mean(response[d3==1]),pch=19,cex=2,col="blue")
#slopes for no intercept model - slope=group means
abline(a=0,b=mean(response[d1==1]),lty=2)
abline(a=0,b=mean(response[d2==1]),col="red",lty=2)
abline(a=0,b=mean(response[d3==1]),col="blue",lty=2)
#linear effects model
points(0,mean(response),pch=19,cex=3,col="purple")
segments(0,mean(response),1,mean(response[d1==1]))
segments(0,mean(response),1,mean(response[d2==1]),col="red")
segments(0,mean(response),1,mean(response[d3==1]),col="blue")

moddummyNI<-lm(response~0+d1+d2+d3)
anova(moddummyNI) #this is testing whether the group means = 0
moddummyNI # each of the Coefficents is the mean
mean(response[1:10])
mean(response[11:20])
mean(response[21:30])

moddummy<-lm(response~d2+d3)#this is what the computer is doing - d1 becomes the intercept
#Anova(moddummy)# 
summary(moddummy)
mm<-model.matrix(lm(response~treats))
mean(response[mm[,2]==0&mm[,3]==0])# intercept is mean of d1
mean(response[mm[,2]==1&mm[,3]==0])
mean(response[mm[,2]==0&mm[,3]==1])

plot(rep(0,10),response[mm[,2]==0&mm[,3]==0],ylim=c(20,90))
points(rep(1,10),response[mm[,2]==1&mm[,3]==0],col="red")
points(rep(1,10),response[mm[,2]==0&mm[,3]==1],col="blue")
points(0,mean(response[mm[,2]==0&mm[,3]==0]),pch=19,cex=2,col="black")
points(1,mean(response[mm[,2]==1&mm[,3]==0]),pch=19,cex=2,col="red")
points(1,mean(response[mm[,2]==0&mm[,3]==1]),pch=19,cex=2,col="blue")
segments(1,mean(response[mm[,2]==1&mm[,3]==0]),0,mean(response[mm[,2]==0&mm[,3]==0]),col="red",lwd=2)
segments(1,mean(response[mm[,2]==0&mm[,3]==1]),0,mean(response[mm[,2]==0&mm[,3]==0]),col="blue",lwd=2)

summary(moddummy)
mean(response[mm[,2]==1&mm[,3]==0])-mean(response[mm[,2]==0&mm[,3]==0]) #parameter estimate for d2
mean(response[mm[,2]==0&mm[,3]==1])-mean(response[mm[,2]==0&mm[,3]==0]) #parameter estimate for d3
abline(a=59.4,b=-14.5,col="red")
abline(a=59.4,b=5.6,col="blue")

#boxplot(response~treats,las=1)  
anova(moddummy)
MSval<-(1995.27+156.80)/2
F.value<-MSval/(4157.3/27)
pf(F.value,2,27,lower.tail=FALSE)
mod<-lm((response~treats))
anova(mod)

moddummy2<-lm(response~d1+d3)#here, d2 is the intercept
anova(moddummy2)# 
summary(moddummy2)
boxplot(response~treats,las=1,ylim=c(0,100)) #recall.  
MSval<-(132.0+2020.1)/2
F.value<-MSval/(153.97)
pf(F.value,2,27,lower.tail=FALSE)
anova(mod)

mod<-lm((response~treats))
mod
anova(mod)
#intercept is simply the mean of treatA



library(car)
anova(mod)
Anova(mod)

###########
#post hoc



treats<-as.factor(treats)
mod<-lm(response~treats)
aov.mod<-aov(mod)
TukeyHSD(aov.mod)

library(multcomp)
tukey.hsd<-glht(mod,linfct=mcp(treats="Tukey"))
contrMat(n=c(10,10,10),"Tukey")
summary(tukey.hsd)
plot(tukey.hsd)


#Dunnetts test - here A is the focal group to compare against the others
contrMat(c(10,10,10),"Dunnet")


dunnet.test<-glht(aov.mod,linfct=mcp(treats="Dunnett"))
dunnet.test
summary(dunnet.test)
plot(dunnet.test)

dunnet.test<-glht(aov.mod,linfct=mcp(treats=contrMat(c(10,10,10),"Dunnet",base=2)))#everything now compared against the second treatment
dunnet.test
summary(dunnet.test)
plot(dunnet.test)

#alternatively, we can re-order the factors, if we'd like..
treatsRO<-factor(treats,levels=c("B","C","A"))
modRO<-lm(response~treatsRO)
dunnets.test.RO<-glht(modRO,linfct=mcp(treatsRO="Dunnett"))
summary(dunnets.test.RO)

dunnet.test
summary(dunnet.test)
plot(dunnet.test)

#planned contrasts # 
k<-rbind("A-B"=c(1,-1,0))
planned.contrast<-glht(aov.mod,linfct=mcp(treats=k))
summary(planned.contrast)
plot(planned.contrast)



k<-rbind("B-C"=c(0,1,-1))
planned.contrast<-glht(aov.mod,linfct=mcp(treats=k))
summary(planned.contrast)

k<-rbind("A-BC"=c(1,-1,-1))
planned.contrast<-glht(aov.mod,linfct=mcp(treats=k))
summary(planned.contrast)
k<-rbind("A-BC"=c(1,-1/2,-1/2)) # r is smart enough that it figures out -1/2
planned.contrast<-glht(aov.mod,linfct=mcp(treats=k))
summary(planned.contrast)


k<-rbind("A-B"=c(1,-1,0),"B-C"=c(0,1,-1))
planned.contrast<-glht(aov.mod,linfct=mcp(treats=k))
summary(planned.contrast)

k<-rbind("A-B"=c(1,-1,0),"B-C"=c(0,1,-1),"A-BC"=c(-1,1,1))
planned.contrast<-glht(aov.mod,linfct=mcp(treats=k))
summary(planned.contrast)
plot(planned.contrast)



