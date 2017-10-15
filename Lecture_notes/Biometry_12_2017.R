	height.cm<-c(17.7,18.7,17.2,20.1,19.1,17.7,18.9,19.8,15.8,19.5)
	nitrogen.ppm<-c(8.26,7.45,6.95,8.53,9.37,8.49,9.7,9.09,7.34,9.69)
	
	model<-lm(height.cm~nitrogen.ppm)
	summary(model)
	anova(model)

model.r<-lm(height.cm~1);summary(model.r);mean(height.cm)# reduced model
anova(model.r)

anova(model,model.r)# we should keep height in the model
16.0450-8.5783  # the residual SS for both models   divide this by residualMS from full model
(16.0450-8.5783)/1.0723

#summary(model)
#pt(2.639,8,lower.tail=FALSE)*2  #8 degrees of freedom

slope<-cov(height.cm,nitrogen.ppm)/var(nitrogen.ppm)
intercept<-mean(height.cm)-(slope*mean(nitrogen.ppm))
predicted.y<-intercept+slope*nitrogen.ppm
SSerror<-sum((height.cm-predicted.y)^2)
MS.error<-SSerror/8
std.error.b1<-sqrt(MS.error/sum((nitrogen.ppm-mean(nitrogen.ppm))^2))
t.stat<-slope/std.error.b1

t.stat^2  #t^2 = F
pt(t.stat,8,lower.tail=FALSE)*2
pf(t.stat^2,1,8,lower.tail=FALSE)



#QC
#QC  #are different steps, most are graphical
#Lack of fit
#When you have multiple y's at a given x
  #important when we have multiple observations at the same value
    #cause this can drive how steep the slope is
  #gonna turn numbers into factors, so order of them doesn't matter, basically doing an ANOVA on this regression
  #Converting the predictor into a factor effectively ignores the continuous aspect of the predictor and treats each value as a categorical predictor.
  #can test for lack of fit by making a new model
height.cm<-c(17.7,18.7,17.2,20.1,19.1,17.7,18.9,19.8,15.8,19.5)
#nitrogen.ppm<-c(8.26,7.45,6.95,8.53,  ##9.37##  ,8.49,9.7,9.09,7.34,9.69)#change data to have two observations at 9.69
nitrogen.ppm<-c(8.26,7.45,6.95,8.53,9.69,8.49,9.7,9.09,7.34,9.69)
plot(height.cm~nitrogen.ppm)
model<-lm(height.cm~nitrogen.ppm)
as.factor(nitrogen.ppm) #as factor converts nitrogen.ppm into categories
as.numeric(as.factor(nitrogen.ppm))

modelLF<-lm(height.cm~as.factor(nitrogen.ppm))
anova(model)#
anova(modelLF)#not sig, probably not a problem

xs<-c(1,3,3,3,3,3,3,6,8,7,7)
ys<-c(4,8,6,7,5,4,6,12,10,9,10)
dumbmod<-lm(ys~xs)
plot(xs,ys);abline(dumbmod)
anova(dumbmod)
dumbmodLF<-lm(ys~as.factor(xs))
anova(dumbmodLF)


#QC plots
height.cm<-c(17.7,18.7,17.2,20.1,19.1,17.7,18.9,19.8,15.8,19.5)
nitrogen.ppm<-c(8.26,7.45,6.95,8.53,9.37,8.49,9.7,9.09,7.34,9.69)
model<-lm(height.cm~nitrogen.ppm)
plot(model)

#Residuals vs. Fitted - The residuals should be randomly distributed around the horizontal line representing the residual error of zero; there should not be a distinct trend in the distribution of points - numbered points should maybe be paid attention to, but not worried about too much - some points will always be numbered
  #symmetry is around the regression line
  #the red line is supposed to help you figure out if anything is a pattern of interest
  #you should have red line being straight across
  #the numbers on graph are the data points, will name 3 that are furthest away
  #want a nice symmetrical spread, uniform distribution, across the graph
  #don't want any pattern
  
  
#Normal Q-Q - Should be pretty close to the line
  #tells us theoeretical quantiles if our error term is normally distributed
    #want it to be normally distributed b/c have assumed this in this model
  #theoretical quantiles vs. standardized residuals
  #if it is normal, all the dots should fall right on the dotted line
  
  
#Scale and location - different way to look at residuals vs. fitted
 #want to have no pattern
 
 
#Residuals vs. Leverage - Because the regression must pass through the centroid, points that lie far from the centroid have greater leverage, and their leverage increases if there are fewer points nearby. As a result, leverage reflects both the distance from the centroid and the isolation of a point. The plot also contours values of Cook’s distance, which measures how much the regression would change if a point was deleted. Cook’s distance is increased by leverage and by large residuals: a point far from the centroid with a large residual can severely distort the regression. On this plot, you want to see that the red smoothed line stays close to the horizontal gray dashed line and that no points have a large Cook’s distance (i.e, >0.5).
    
height.cm<-c(17.7,18.7,17.2,20.1,19.1,17.7,18.9,19.8,15.8,39.5)#changed last value to 39.5
nitrogen.ppm<-c(8.26,7.45,6.95,8.53,9.37,8.49,9.7,9.09,7.34,9.69)
model<-lm(height.cm~nitrogen.ppm)
plot(model)

#TESTS FOR NORMALITY
#package fBasics - has function to test for normality

#3 MAIN TESTS FOR NORMALITY, 3RD IS THE BEST
  #1st - Shapiro Wilks Test
    #weaknesses - sensitive to small samples, with small samples will allow things to pass
        #with large samples, will flag the ones that don't work
        #doesn't work well if have identical values        
        
set.seed(6)
library(fBasics)
x<-rnorm(50)

hist(x,breaks=15)

normalTest(x,method=c("sw"))   #function in fBasics that tests Shapiro wilks and others
#null hypo of normality test is that data is not different from normal
#so according to the result of this shapiro wilks test, our data is not normal
#but we drew it from a normal distribution, so use head and look at it


#Kolmogorov-Smirnoff Test
  #most common one in the lit, but probs the worst
  #originally developed to compare 2 distributions, so is doing a QQ plot really
  #is taking our data and is challenging it against the theoretical expectations, and looking at how far our is from theretical
  #if is normal, all dots should be on line
normalTest(x,method=c("ks")) #how to test for normality with KS
  #here is passes the test, so acceept the null that our data is normally distributed, cause 2 sided is 0.57



#D'Agostine Pearson Omnibus Test
  #looking at both skewness and kurtosis (whether have a lot of data crammed towards mean or tail)
normalTest(x,method=c("da"))
  #get a p value of 0.1798, so accept null that data is dsitributed normally
  #also give a skewness and kurtosis numbers, so can help us decide how and if should transform data

#Error term must be normally distributed, can see if residual data is normally distributed


#care about normality with regression, cause taking the mean of each variable when calculating intercept and the covariance of this

#ERROR TERM IS THE MOST IMPORTANT THING THAT HAS TO BE NORMALLY DISTRIBUTED, MORE IMPORTANT THAN ACTUAL DATA
