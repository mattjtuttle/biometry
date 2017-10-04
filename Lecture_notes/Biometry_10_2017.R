
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
model
# Call:
# lm(formula = height.cm ~ nitrogen.ppm)

# Coefficients:
 # (Intercept)  nitrogen.ppm  
     # 10.6627        0.9176 
       # everytime nitrogen.ppm increases by 1 unit, height increases by 0.9176 cm

plot(nitrogen.ppm,height.cm,pch=19,las=1)
abline(a=10.6627,b=0.9176)

names(model)
model$coefficients
model$residuals

points(nitrogen.ppm[1],height.cm[1],col="red",cex=2)
predheight<-10.66265+ 0.9175621*nitrogen.ppm[1]
#or, better
predheight<-model$coefficients[1]+ model$coefficients[2]*nitrogen.ppm[1]
model$fitted.values
points(nitrogen.ppm[1],predheight,col="blue",cex=2)
height.cm[1]-predheight
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
plot(observed,predicted,xlim=c(14,22),ylim=c(14,22))
abline(a=0,b=1)
cor(observed,predicted)^2


#model<-lm(height.cm~nitrogen.ppm)
#summary(model)



######## anatomy of an anova table


MS.regresssion<-SSregression/1 # 1 df because number of parameters estimated (2, intercept and slope) - 1
MS.error<-SSerror/8 # 8 df because n - k -1 is sample size (10) - minus number of degrees of freedom in the model (k, the regression part), minus 1  10-1-1

MS.total <- SStotal/9 #the total mean square is the variance (total MS plays no direct role in ANOVA)
var(height.cm)

### slope = 0?
std.error.b1<-sqrt(MS.error/sum((nitrogen.ppm-mean(nitrogen.ppm))^2))


#calculate the t-value
theta<-0
t.stat<-(slope-theta)/std.error.b1
#test slope
(1-pt(t.stat,8))*2 #this gives the p-value associated with the null hypothesis that the slope is not different from zero
pt(t.stat,8,lower.tail=FALSE)*2

### intercept = 0?
std.error.b0<-sqrt(MS.error*((1/10)+(mean(nitrogen.ppm)^2/sum((nitrogen.ppm-mean(nitrogen.ppm))^2))))
t.stat<-(intercept-theta)/std.error.b0
pt(t.stat,8,lower.tail=FALSE)*2

#########
model<-lm(height.cm~ nitrogen.ppm)
summary(model)









#95% CI for estimate of slope
# 
slope + (qt(0.975,8)*std.error.b1)
slope - (qt(0.975,8)*std.error.b1)
confint(model,"nitrogen.ppm",level=0.95)
#95% CI for estimate of intercept
#
intercept+(qt(0.975,8)*std.error.b0)
intercept-(qt(0.975,8)*std.error.b0)
confint(model,"(Intercept)",level=0.95)
 
 plot(nitrogen.ppm,height.cm)
 abline(model)
 
 
 confint(model)
###




pred.frame<-data.frame(nitrogen.ppm=seq(from=0,to=20,length=50))#
pred.c<-predict(model,int="confidence",newdata=pred.frame)# c is confidence fitted data 
pred.c
cbind(pred.frame,pred.c)
model$fitted.values
#abline(h=113.72)
#abline(v=62)
plot(nitrogen.ppm,height.cm,pch=19,las=1)
abline(model)
matlines(pred.frame,pred.c,col=c("black","red","red"),lwd=3,lty=c(1,2,2))#confidence curves for fitted data








pred.p<-predict(model,int="prediction",newdata=pred.frame)#level=0.95 # p is prediction PREDICTED INDIVIDUALS

plot(nitrogen.ppm,height.cm,pch=19,las=1)
abline(model)
matlines(pred.frame,pred.p,col=c("black","red","red"),lwd=3,lty=c(1,3,3))#confidence curves for predicted individuals

plot(nitrogen.ppm,height.cm,pch=19,las=1,ylim=c(0,30),xlim=c(0,20))
#pred.wt<-pred.frame$length.cm
matlines(pred.frame,pred.c,col=c("black","red","red"),lwd=3,lty=c(1,2,2))#confidence curves for fitted data
matlines(pred.frame,pred.p,col=c("black","red","red"),lwd=3,lty=c(1,3,3))#confidence curves for predicted individuals



#Intervals get large and doesn't really make sense (zero nitrogen won't result in much in the way of growth)