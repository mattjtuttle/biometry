load("splityield.R")

mod<-lm(yield~irrigation,data=yields)
summary(mod)
#let's remind ourselves what these parameters are about
#the intercept is 81.500 - where does this come from?
#it is the mean of the control

mean(yields[yields[,3]=="control",1])

mean(yields[yields[,3]=="irrigated",1])
#the parameter for the slope is for irrigationirrigated
mean(yields[yields[,3]=="irrigated",1])-mean(yields[yields[,3]=="control",1])
#thus, irrigated is 21.444 higher than control


#internally, this is what is happening in the model
plot(yield~irrigation,data=yields)
plot(rep(c(rep(0,9),rep(1,9)),4),yields[,1])
abline(mod)
abline(h=mean(yields[yields[,3]=="control",1]),lty=3)
abline(h=mean(yields[yields[,3]=="irrigated",1]),lty=3)

#Okay - now where all remembered up
# the lm() function picks which factor serves as the intercept alphabetically
#if you really don't like that, you can reorder the factors
reorder.irrigation<-factor(yields[,3],levels(yields[,3])[c(2,1)])
mod.reorder<-lm(yields[,1]~reorder.irrigation)
summary(mod.reorder)
#now irrigated is the intercept and the slope describes the control group difference
#But we digress...

summary(mod)#this is where we were
#We can perhaps make the intercept make more intuitive sense if we center our data around the grand mean
c.yields<-scale(yields[,1],scale=FALSE)#centered yeild
mod.c<-lm(c.yields~yields[,3])#our new model with centered data
summary(mod.c)#Note the parameter estimate and p-value for slope remains unchanged

mean(yields[yields[,3]=="control",1])-mean(yields[,1])#control is 10.722 lower than the grandmean
mean(yields[yields[,3]=="irrigated",1])-mean(yields[,1])#irrigated is 10.722 higher than the grand mean
diff(c((mean(yields[yields[,3]=="control",1])-mean(yields[,1])),(mean(yields[yields[,3]=="irrigated",1])-mean(yields[,1]))))#the difference between the two is the slope  diff(c(-10.722,10.722))
#By centering our data, our intercept is perhaps a bit more interpretable / meaningful

#Mixed Effects Models
#Fixed vs. Random effects  -  tricky because there are several definitions
#Gelman simply renamed these to get around the confusion
#	constant effects: identical for all groups in a population
#   varying effects: permitted to differ from group to group
#
#or we can simply think about it this way -
# fixed effects: all the possible values of a variable are fixed (treatment vs. control)
# random effects: the set of potential values can change (e.g., family(nested designs) or blocks)

#Let's look at the full yield data
yields
#the experiment was conducted in 4 different fields. 4 fields from a "population" of possible fields. Do it would be best to think about fields as being a random effect, with irrigation nested within each field. We're interested in the effect of irrigation (our fixed effect), but we need to account for variation among blocks
#we can see that there are replicates within each field for both irrigation treatments
#we will deal with this by adding a random effect for field
#this allows us to deal with the non-independence by assuming a different "baseline" of yield for each field
#it looks kinda like this
comb.field.irrigation<-paste(yields[,2],yields[,3],sep="")
boxplot(yields[,1]~comb.field.irrigation)

#We can model the differences among fields by assuming different Random Intercepts for each field. So, each field is getting it's own intercept.
#We can now see why this approach is called a "mixed model"
#Our previous playing we focused on "fixed effects" and dumped everything we could or didn't account for as being dumped in the error term
#Now we're trying to think a little bit about the error term, so that we can account for variation/idiosynchrosies among fields
#Esentially, we're giving structure to the error term

#Whereas before our mod was
# yields~irrigation + error
#now it is
# yields~irrigation + (1|field) + error
# the 1|field will be our way to telling R that we want to assume an intercept that is different for each field

library(lme4)
mod<-lmer(yield~irrigation,data=yields)#we get an error because we didn't include a random effect

mod<-lmer(yield~irrigation+(1|field),data=yields)#assume the intercept is different for each field
summary(mod)
anova(mod)
#Anova(lm(yield~irrigation+field,data=yields))
# Random effects:
 # Groups   Name        Variance Std.Dev.
 # field    (Intercept) 270.4    16.44   
 # Residual             228.1    15.10   
# Number of obs: 72, groups: field, 4

#If we look at column Std.Dev this gives us a measure of how much variability in yield is due to variation among fields
#Residual is still what we're unable to explain in our model

#The fixed effects part we can interpret the same as before
# Fixed effects:
                    # Estimate Std. Error t value
# (Intercept)           81.500      8.598   9.479 #81.5/8.598 is the t value
# irrigationirrigated   21.444      3.560   6.024 #21.444/3.560

#The intercept
head(yields)
mean(yields[yields[,3]=="control",1])

#The slope
mean(yields[yields[,3]=="irrigated",1])#102.9444-81.5

#Note that its the same as this
summary(lm(yield~irrigation,data=yields))
summary(mod)
#But, the Std.Error and t value are different because we've explicitly account for variation among fields

#Note that there are no p-values
#That's because there is no agreement on the "best way" to calculate it
#The most established / accepted approach is to use a likelihood ratio test
#Or we could simply look at the AIC

mod.null<-lmer(yield~(1|field),data=yields,REML=FALSE)#REML = FALSE so we can get a likelihood for the model
summary(mod.null)
mod.full<-lmer(yield~irrigation+(1|field),data=yields,REML=FALSE)
summary(mod.full)
#We can look at the AIC to pick the "best" model, or do a likelihood ratio test
anova(mod.null,mod.full)
#So we conclude that it's a good idea to keep irrigation in the model

#
mod<-lmer(yield~irrigation+(1|field),data=yields)
summary(mod)
coef(mod)
#just like we asked R to do, we've allowed intercepts to vary
#but - not that the slopes for each field remain the same
#this is what is called a random intercept model, because only the intercepts vary

plot(rep(c(rep(0,9),rep(1,9)),4),yields[,1])
abline(a=57.4,b=21.44)
abline(a=90.09,b=21.44)
abline(a=88.8,b=21.44)
abline(a=89.67,b=21.44)

lm(yield~irrigation,data=yields)#same slope


boxplot(yields[,1]~comb.field.irrigation)
#let's allow the slopes to vary too
mod2<-lmer(yield~irrigation+(1+irrigation|field),data=yields)
summary(mod2)#note that the correlation between the random effects is 1
#The means R settled on a solution where the two were combined
#Probably don't need them both, unless we care about the variation described at each level
coef(mod2)
cor(c(63.44188,88.05822,86.30634,88.19356),c(9.25337,25.87191,24.68921,25.96328))

plot(rep(c(rep(0,9),rep(1,9)),4),yields[,1])
abline(a=63.44,b=9.25)
abline(a=88.05,b=25.87)
abline(a=86.3,b=24.68)
abline(a=88.19,b=25.96)



mod2.null<-lmer(yield~irrigation+(1|field),data=yields,REML=FALSE)#probably appropriate model
mod2.Full<-lmer(yield~irrigation+(1+irrigation|field),data=yields,REML=FALSE)
anova(mod2.null,mod2.Full)
#the random intercept model is probably the appropriate model
#Bolker et al 2008 TREE  Better to try to reduce the model until all terms can be uniquely estimated
mod<-lmer(yield~irrigation+(1|field),data=yields)#probably appropriate model

summary(mod)
# Linear mixed model fit by REML ['lmerMod']
# Formula: yield ~ irrigation + (1 | field) 
   # Data: yields 

# REML criterion at convergence: 595.2296 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # field    (Intercept) 270.4    16.44   
 # Residual             228.1    15.10   
# Number of obs: 72, groups: field, 4

# % of variation of error term explained by our random effect i.e., field
270.4/(270.4+228.1)




yields

fatmod<-lmer(yield~irrigation*density+(1+density|field),data=yields)#different slopes and intercepts
summary(fatmod)

fatmodfull<-lmer(yield~irrigation*density+(1+density|field)+(1+irrigation|field),data=yields,REML=F)
fatmodred<-lmer(yield~irrigation*density+(1|field),data=yields,REML=F)

anova(fatmodfull,fatmodred)
#our reduced model is better

fatmodred2<-lmer(yield~irrigation+density+(1|field),data=yields,REML=F)
anova(fatmodred,fatmodred2)


#we could try different ones too
fatmodfull<-lmer(yield~irrigation*density+(1+density|field),data=yields,REML=F)
#our reduced is still better


#and we can further look at fertilizer
fatmodfull<-lmer(yield~irrigation*density*fertilizer+(1|field),data=yields,REML=F)
summary(fatmodfull)
anova(fatmodfull)
anova(fatmodred,fatmodfull)




fatmodfull<-lmer(yield~irrigation*density*fertilizer+(1|field),data=yields,REML=F)
fatmodfullnoINT<-lmer(yield~irrigation*density+fertilizer+(1|field),data=yields,REML=F)
anova(fatmodfull,fatmodfullnoINT)

