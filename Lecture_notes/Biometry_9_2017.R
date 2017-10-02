#######
# Dependence relationship
library(MASS)
set.seed(53310)
mu<-rep(0,2)
Sigma<-matrix(0.6,nrow=2,ncol=2)+diag(2)*0.7
rawvars<-mvrnorm(n=10,mu=mu,Sigma=Sigma)
height.cm<-rawvars[,1]+18
nitrogen.ppm<-rawvars[,2]+8
plot(nitrogen.ppm,height.cm,pch=19,las=1)
anova(lm(height.cm~ nitrogen.ppm))
summary(lm(height.cm~ nitrogen.ppm))
plot(lm(height.cm~ nitrogen.ppm))


height.cm<-c(17.7,18.7,17.2,20.1,19.1,17.7,18.9,19.8,15.8,19.5)
nitrogen.ppm<-c(8.26,7.45,6.95,8.53,9.37,8.49,9.7,9.09,7.34,9.69)

cbind(height.cm,nitrogen.ppm)

plot(nitrogen.ppm,height.cm,pch=19,las=1)


height.minus.mean<-height.cm-mean(height.cm)
nitrogen.minus.mean<-nitrogen.ppm-mean(nitrogen.ppm)
cbind(height.cm, nitrogen.ppm,height.minus.mean,nitrogen.minus.mean)

product.x.y<-height.minus.mean*nitrogen.minus.mean
covariance<-sum(product.x.y)
variance<-sum(nitrogen.minus.mean^2)

slope<-covariance/variance

#or, simply
slope<-cov(height.cm,nitrogen.ppm)/var(nitrogen.ppm)

#intercept
# mean(y)=bo+b1(mean(x))
intercept<-mean(height.cm)-slope*mean(nitrogen.ppm)



model<-lm(height.cm~nitrogen.ppm)
# Call:
# lm(formula = height.cm ~ nitrogen.ppm)

# Coefficients:
# (Intercept)          nitrogen.ppm  
   # 53.30111      0.08391 
   # everytime nitrogen.ppm increases by 1 unit, height increases by 0.08391 cm

names(model)
model$residuals
mean(model$residuals)

plot(nitrogen.ppm,height.cm,pch=19,las=1)
abline(model)

########  
model<-lm(height.cm~nitrogen.ppm)

#SS regression
par(mar=c(4,4,1,1))
plot(nitrogen.ppm,height.cm,pch=19,las=1)
abline(h=mean(height.cm),lty=2,col="blue")
abline(model)


predicted.y<-intercept+slope*nitrogen.ppm
predicted.y
cbind(nitrogen.ppm,predicted.y)
model$fitted.values #fitted values are the predicted values
cbind(nitrogen.ppm,predicted.y,model$fitted.values)


points(nitrogen.ppm,predicted.y,col="red",pch=19)
for(i in 1:length(nitrogen.ppm)){
	arrows(nitrogen.ppm[i],predicted.y[i],nitrogen.ppm[i],mean(height.cm),lwd=1,code=3,length=0.1,angle=15)	
}


predicted.y.minus.mean<-((predicted.y)-mean(height.cm))

SSregression<-sum((predicted.y.minus.mean)^2)

#SS error

model<-lm(height.cm~nitrogen.ppm)
plot(height.cm~nitrogen.ppm)
abline(model)
abline(h=mean(height.cm),lty=2,col="blue")

points(nitrogen.ppm,predicted.y,col="red",pch=19)
for(i in 1:length(nitrogen.ppm)){
	arrows(nitrogen.ppm[i],predicted.y[i],nitrogen.ppm[i],height.cm[i],lwd=2,code=3,length=0.1,angle=15)	
}


SSerror<-sum((height.cm-predicted.y)^2)

##SS total
SStotal<-SSregression+SSerror
#  --or--

model<-lm(height.cm~ nitrogen.ppm)
plot(height.cm~ nitrogen.ppm)
abline(model)
abline(h=mean(height.cm),lty=2,col="blue")
for(i in 1:length(nitrogen.ppm)){
	arrows(nitrogen.ppm[i],mean(height.cm), nitrogen.ppm[i],height.cm[i],lwd=2,code=3,length=0.1,angle=15)	
}


Sstotal<-sum((height.cm-mean(height.cm))^2)

#SS total values calculated two different ways
SStotal # Calculated above at SSregression+SSerror
Sstotal # Calculated above as sum((height.cm-mean(height.cm))^2)


#R^2 as variation explained
SSregression/SStotal

#R^2 as the square of the correlation between predicted and observed values
observed<-height.cm
predicted<-model$fitted.values
plot(observed,predicted)
cor(observed,predicted)^2


#model<-lm(height.cm~nitrogen.ppm)
#summary(model)