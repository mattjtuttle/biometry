#Return to eebbiometry@gmail.com by 11:59 pm, Wednesday December 6th.
# Save the finalhomework.R file as FirstName_LastName_HW11
#   on the subject line put    YOURLASTNAME_EXERCISE

#Name:




#1. We like ducks. We're interested in knowing whether those wind-sock things that people like to put in their yards reduce the number of ducks at a pond. We set up an experiment where 25 ponds had one of those things, and the other 25 did not. 

no.ducks<-c(0,1,1,0,2,2,0,1,0,1,1,0,1,0,0,2,3,0,0,0,1,0,1,0,0,1,0,1,4,0,0,0,3,3,1,2,3,1,2,0,5,1,0,0,4,2,4,1,1,3)
wind.socks<-c('yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no')


#1. We would like to know if the number of ducks is different between ponds with those wind-sock things and those without those wind-sock things. Build a model to address this hypothesis, and while you're at it - test this hypothesis. 





#2. What percent increase in ducks would we expect to see between the wind-sock ponds and the non-wind-sock ponds.  







#Temperature can affect diapause in insects. You examined 100 diapausing insects across a temperature gradient and record when they break diapause. In the diapause data below, a 0 indicates diapause and a 1 indicates an insect that has broken diapause. 

diapause<-c(0,1,0,0,1,1,0,1,0,0,0,0,1,0,0,1,1,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,1,0,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,1,1,0,1,1,1,1,1,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,0,1,1,1,0,0,1,0,1,0,1,1,1,1,1,1,1,1,1)
temperature<-c(10,10.17,10.34,10.52,10.69,10.86,11.03,11.2,11.37,11.55,11.72,11.89,12.06,12.23,12.4,12.58,12.75,12.92,13.09,13.26,13.43,13.61,13.78,13.95,14.12,14.29,14.46,14.64,14.81,14.98,15.15,15.32,15.49,15.67,15.84,16.01,16.18,16.35,16.53,16.7,16.87,17.04,17.21,17.38,17.56,17.73,17.9,18.07,18.24,18.41,18.59,18.76,18.93,19.1,19.27,19.44,19.62,19.79,19.96,20.13,20.3,20.47,20.65,20.82,20.99,21.16,21.33,21.51,21.68,21.85,22.02,22.19,22.36,22.54,22.71,22.88,23.05,23.22,23.39,23.57,23.74,23.91,24.08,24.25,24.42,24.6,24.77,24.94,25.11,25.28,25.45,25.63,25.8,25.97,26.14,26.31,26.48,26.66,26.83,27)



#3. Build a model to see if temperature predicts breaking diapause. 



#4. At what temperature would we expect 50% of the animals to be in diapause.




#Run this code and use the object "mod" to answer questions 5 & 6

library(vegan)
data(dune)
data(dune.env)
A1<-dune.env[,1]
mod<-rda(dune~A1,scale=TRUE)



# 5. How much variation in dune vegetation is explained by depth of the A1 soil profile?



# 6. What three species of plants vary most along this soil profile gradient?



#Bonus
#I can calculate a p-value based upon our observed F and degrees of freedom.
#pf(1.771,1,18,lower.tail=FALSE)
# [1] 0.1998715

#Why is the p-value different from the one returned from the anova() function?



