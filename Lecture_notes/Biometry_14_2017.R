
###########

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

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    

##SO2=b0+b1temp+b2fact





b1p<-(cor(SO2,AveTemp) - (cor(SO2,Num.factories)*cor(AveTemp,Num.factories)))/(1-cor(AveTemp,Num.factories)^2) # I'm calling this b1p because it's b prime 1 because it's in units of standard deviations
b2p<-(cor(SO2,Num.factories) - (cor(SO2,AveTemp)*cor(AveTemp,Num.factories)))/(1-cor(AveTemp,Num.factories)^2)

# now we but the parameters back in conventional / observed units

b1<-b1p*sd(SO2)/sd(AveTemp)
b2<-b2p*sd(SO2)/sd(Num.factories)

b0<-mean(SO2)-b1*mean(AveTemp)-b2*mean(Num.factories)


model<-lm(SO2~AveTemp+Num.factories)
summary(model)

summary(model)
library(QuantPsyc)
lm.beta(model)

##proof of concept
# z transform data prior to lm()
z.SO2<-scale(SO2)
z.Avetemp<-scale(AveTemp)
z.Num.factories<-scale(Num.factories)

z.model<-lm(z.SO2~z.Avetemp+z.Num.factories)
summary(z.model)








model1<-lm(SO2~AveTemp+Num.factories)
model2<-lm(SO2~Num.factories+AveTemp)
anova(model1)
anova(model2)
anova(lm(SO2~AveTemp))
anova(lm(SO2~Num.factories))


library(car)
anova(model1)
anova(model2)
Anova(model1)
Anova(model2)

sum((SO2-mean(SO2))^2)
5.6458+1.2778+12.8053#SS from model1 using sequential SS
2.2561+4.6675+12.8053#SS from model2 using sequential SS
4.6675+1.2778+12.8053#=18.7506#SS using marginal not the total SS

#so, to calculate the partial r^2 if using marginal
#1.2778/19.7289    #number of factories
#4.6675/19.7289    #AveTemp

summary(model1)




### VIF
#library(HH)
library(car)
vif(model)

mod.temp<-lm(Num.factories~AveTemp)
summary(mod.temp)
#1/(1-0.02648)  #this is the VIF for AveTemp
mod.fact<-lm(AveTemp~Num.factories)
summary(mod.fact)
#1/(1-0.02648)  #this is the VIF for Num.factories


fullmodel<-lm(SO2~AveTemp+Num.factories+Population)
vif(fullmodel)
cor(Num.factories,Population)

model<-lm(SO2~AveTemp+Num.factories)
Anova(model)
plot(model)
#outlierTest(model)

model2<-lm(SO2~AveTemp+Population)

lm.beta(model)
lm.beta(model2)
lm.beta(fullmodel)# check out the magnitude and sign of population and num.factories when we don't worry about VIF, which we should


