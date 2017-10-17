#Return to eebbiostatistics@gmail.com by 1:25pm, Friday September 30.
#   on the subject line put    YOURLASTNAME_EXERCISE

#Name:

# Does learning a second language change brain structure? Machelli et al. (2004) tested 22 native Italian speakers who had learned English as a second language. Proficiencies in reading, writing, and speech were assessed by a proficiency score. Gray matter density was measured in the left inferior parietal region of the brain using a neuroimaging technique, as mm^3 of gray matter per voxel. Read in the Brains.csv table:
Brains<-read.csv('Brains.csv')

#1.  Calculate the correlation between second language proficiency and gray matter density.
slp<-Brains$Second.language.proficiency
gmd<-Brains$Gray
cor(slp,gmd)

#2. Test the null hypothesis when theta = 0
t.stat<-(((cor(slp,gmd))-0)/(sqrt(((1-(cor(slp,gmd))^2)) /((length(slp)-2)))))
pvalue<-pt(q = t.stat,df = (length(slp)-2),lower.tail = FALSE)*2
pvalue

#or

cor.test(slp,gmd)

#3. What is the 83% confidence around your estimate of rho?
rho<-cor(slp,gmd)
n<-length(slp)
zrho<-0.5*log((1+rho)/(1-rho))
zse<-sqrt(1/(n-3))
uci.z<-zrho+qt(0.915,n-2)*zse
lci.z<-zrho+qt(0.085,n-2)*zse
uci<-(exp(2*uci.z)-1)/(exp(2*uci.z)+1)
lci<-(exp(2*lci.z)-1)/(exp(2*lci.z)+1)
uci
lci


#4. Build a general linear model of the data where proficency score predicts brain matter. Which factor is all of the error confined to?
Bmod<-lm(gmd~slp)
summary(Bmod)

##In a linear model, all of the error is estimated on the y axis

#5. Theory predicts that the slope should be 0.04. Test this statistical hypothesis.
theta<-0.04
slope<-0.03024
slope.err<-0.004749 #All of this information can be gathered from the summary of the model

t.slope<-(slope-theta)/slope.err 
#T value is less than zero so:

pvalue<-pt(q = t.slope,df = (length(slp)-2),lower.tail = TRUE)*2

#In a strict pearson framework, we fail to reject the null and accept that our data are drawn from a population with a slope of 0.04

#6. How much variation in gray matter density does your model explain?
# R^2 is 0.6696

