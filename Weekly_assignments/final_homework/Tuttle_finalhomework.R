#Return to eebbiometry@gmail.com by 11:59 pm, Wednesday December 6th.
# Save the finalhomework.R file as FirstName_LastName_HW11
#   on the subject line put    YOURLASTNAME_EXERCISE

#Name: Matthew Tuttle




#1. We like ducks. We're interested in knowing whether those wind-sock things that people like to put in their yards reduce the number of ducks at a pond. We set up an experiment where 25 ponds had one of those things, and the other 25 did not. 

no.ducks<-c(0,1,1,0,2,2,0,1,0,1,1,0,1,0,0,2,3,0,0,0,1,0,1,0,0,1,0,1,4,0,0,0,3,3,1,2,3,1,2,0,5,1,0,0,4,2,4,1,1,3)
wind.socks<-c('yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','yes','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no','no')

#1. We would like to know if the number of ducks is different between ponds with those wind-sock things and those without those wind-sock things. Build a model to address this hypothesis, and while you're at it - test this hypothesis. 

duck.mod <- glm(no.ducks ~ wind.socks, family = "poisson")
summary(duck.mod)

library(car)
Anova(duck.mod) # Statistical significance suggests that windsocks affects number of ducks at ponds


#2. What percent increase in ducks would we expect to see between the wind-sock ponds and the non-wind-sock ponds.  

summary(duck.mod)
exp(-0.9045) # Negative value means less ducks with windsocks present

# Ponds without windsocks are expected to see 0.4047442 more ducks than those with windsocks.




#Temperature can affect diapause in insects. You examined 100 diapausing insects across a temperature gradient and record when they break diapause. In the diapause data below, a 0 indicates diapause and a 1 indicates an insect that has broken diapause. 

diapause<-c(0,1,0,0,1,1,0,1,0,0,0,0,1,0,0,1,1,0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,1,1,0,1,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,1,1,0,1,1,1,1,1,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,0,1,1,1,0,0,1,0,1,0,1,1,1,1,1,1,1,1,1)
temperature<-c(10,10.17,10.34,10.52,10.69,10.86,11.03,11.2,11.37,11.55,11.72,11.89,12.06,12.23,12.4,12.58,12.75,12.92,13.09,13.26,13.43,13.61,13.78,13.95,14.12,14.29,14.46,14.64,14.81,14.98,15.15,15.32,15.49,15.67,15.84,16.01,16.18,16.35,16.53,16.7,16.87,17.04,17.21,17.38,17.56,17.73,17.9,18.07,18.24,18.41,18.59,18.76,18.93,19.1,19.27,19.44,19.62,19.79,19.96,20.13,20.3,20.47,20.65,20.82,20.99,21.16,21.33,21.51,21.68,21.85,22.02,22.19,22.36,22.54,22.71,22.88,23.05,23.22,23.39,23.57,23.74,23.91,24.08,24.25,24.42,24.6,24.77,24.94,25.11,25.28,25.45,25.63,25.8,25.97,26.14,26.31,26.48,26.66,26.83,27)



#3. Build a model to see if temperature predicts breaking diapause. 

dia.mod <- glm(diapause ~ temperature, family = "binomial")
summary(dia.mod)

#4. At what temperature would we expect 50% of the animals to be in diapause.

library(MASS)
dose.p(dia.mod) # 50% of the animals are expected to be in diapause at a temperature of 16.70242





#Run this code and use the object "mod" to answer questions 5 & 6

library(vegan)
data(dune)
data(dune.env)
A1<-dune.env[,1]
mod<-rda(dune~A1,scale=TRUE)



# 5. How much variation in dune vegetation is explained by depth of the A1 soil profile?

summary(mod)

# 8.958% of the variation is explained by the depth of the A1 soil profile as indicated by the Proportion Explained by the rda axis


# 6. What three species of plants vary most along this soil profile gradient?

summary(mod)

# Comapalu, Lolipere, and Poaprat.
# These three species of plants have the highest absolute value species scores on the RDA axis as shown in the model summary and below.

#              RDA1
# Achimill -0.28329
# Agrostol  0.31877
# Airaprae -0.14110
# Alopgeni  0.01804
# Anthodor -0.17927
# Bellpere -0.14813
# Bromhord -0.22546
# Chenalbu  0.11081
# Cirsarve -0.06263
# Comapalu  0.77700 *
# Eleopalu  0.45526
# Elymrepe -0.20199
# Empenigr -0.11081
# Hyporadi -0.17716
# Juncarti  0.13175
# Juncbufo -0.02252
# Lolipere -0.49165 *
# Planlanc -0.18679
# Poaprat  -0.51333 *
# Poatriv  -0.17260
# Ranuflam  0.35778
# Rumeacet -0.03106
# Sagiproc -0.09554
# Salirepe -0.16918
# Scorautu -0.19896
# Trifprat -0.06895
# Trifrepe  0.05842
# Vicilath -0.18533
# Bracruta  0.05033
# Callcusp  0.28426



#Bonus
#I can calculate a p-value based upon our observed F and degrees of freedom.
#pf(1.771,1,18,lower.tail=FALSE)
# [1] 0.1998715

#Why is the p-value different from the one returned from the anova() function?

# The p-value is different from the anova() function because this F-value is not a true F-value, rather it is a pseudo-F. This means that it cannot be plugged directly into the pf() function as this function does not take permutations into account.



