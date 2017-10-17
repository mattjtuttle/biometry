data.object <- read.csv(file = "InClassData.csv")
data.object
Gap.area<-data.object$gap.area.m2
Gap.area
N<-data.object$Nitrate.ppm
N


#1.	Build an appropriate statistical model

#First, lets make sure our data are normal

hist(x = Gap.area)# not great. right skew
library(fBasics)
normalTest(x = Gap.area,method = "da") #Our data reject the null hypothesis that the data are drawn from a normal distribution.

#So we need to transform our gap area data. I will choose a square root transformation.

trans.gap.area<-sqrt(Gap.area)
hist(x = trans.gap.area) # It passes the eyeball test
normalTest(x = trans.gap.area,method = "da") # The transformed data fails to reject the null hypothesis that the data are sampled from a normal distribution, so we accept our data as normally distributed.


#What about N?

hist(x = N) #Eyeballs say yes
normalTest(x = N,method = "da") #Again, The data fails to reject the null hypothesis that the data are sampled from a normal distribution, so we accept our data as normally distributed.

#Now we can build our linear model:

model1 <- lm(N~trans.gap.area)
model1 # The model estimates the parameters we care about, an intercept and slope.




#2.	Test your assumptions

#is the model any good?
plot(model1)
#The first one, residual vs fitted shows no real pattern in the residuals. This is good. We want this to be random.
# Q-Q plot, looking to see if the residuals match a normal distribution. We want to see the points follow the line. We see #62 deviating from the line a bit. We will keep an eye on it when we get to leverage.
# plot 3, same as plot 1. I dont really worry about this one if the first one looked good.
#Leverage, this plot looks great. Our residuals are not close to the Cook's distance. All in all we are happy about our model's residuals.



#3.	Build a graph of the data including: Slope, Confidence interval around the slope, and Estimate range of predicted values
#Start with a plot of the data:

plot(x = trans.gap.area,y = N) #Looks bland. Here are some things I do to make it look nice:
plot(x = trans.gap.area,y = N,
     pch=16, #dot shape, 16 is a filled circle
     col="darkgrey", # We are going to be putting a lot of lines on this plot, making the data grey will help with the clutter.
     cex=1.3, #Scaling the size of the points
     xlab="Square root of gap area",
     ylab="Nitrate (ppm)",
     las=1, # Rotate the y axis labels
     xlim=c(0,55), #Give ourselves a little breathing room around the data
     ylim=c(35,65),
     main="Nitrate concentration decreases as gap size increases",
     cex.lab=1.5, #Make our axis labels larger.
     cex.axis=1.2  # Good for now. Lets add the lines requested in the question
     )


#Now lets predict the range of our 95% confidence around the slope
Just.some.xs<-data.frame(trans.gap.area=seq(from=-5,to=65,length=100)) # A vector of x values to fill out the predict function
predict.conf<-predict(model1,newdata=Just.some.xs,interval="confidence")# level=c(0.95) default.
matlines(Just.some.xs,predict.conf, #The data to plot
        lwd=3, #making thicker lines
          lty=c(1,2,2), #It is plotting the fittted values as a solid line (lty=1) and the upper and lower range as dashed lines (lty=2)
         col=c("black","red","red")) #Changing the color of the lines

#We also want to add the range where we expect 95% of future data!
predict.data<-predict(model1,newdata=Just.some.xs,interval="prediction")# level=c(0.95) default.
matlines(Just.some.xs,predict.data, #The data to plot
         lwd=3, #making thicker lines
         lty=c(1,3,3), #It is plotting the fittted values as a solid line (lty=1) and the upper and lower range as dotted lines (lty=2)
         col=c("black","orange","orange")) #Changing the color of the lines

#4.	Write a sentence or two addressing the research question given your model.

summary(model1)

##Given our model of the data, we reject the null hypothesis and suggest that gap area in a forest has a negative effect on soil nitrate concentration (b1= -0.09177, p= 0.0392)
