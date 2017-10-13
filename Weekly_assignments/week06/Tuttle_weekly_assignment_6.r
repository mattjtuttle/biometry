#Return to eebbiometry@gmail.com by 11:15am, Monday October 16.
#   on the subject line put    YOURLASTNAME_EXERCISE

#Name: Matthew Tuttle

# Does learning a second language change brain structure? Machelli et al. (2004) tested 22 native Italian speakers who had learned English as a second language. Proficiencies in reading, writing, and speech were assessed by a proficiency score. Gray matter density was measured in the left inferior parietal region of the brain using a neuroimaging technique, as mm^3 of gray matter per voxel. Read in the Brains.csv table:


#1. Calculate the correlation between second language proficiency and gray matter density.

brains <- read.csv(file = "Brains.csv", header = TRUE)

SL.prof <- brains$Second.language.proficiency
GM.density <- brains$Gray.matter.density

r <- cor(SL.prof, GM.density)


#2. Test the null hypothesis when theta = 0

null.test <- cor.test(SL.prof, GM.density, method = "pearson")
p.value <- null.test$p.value # Probability that true correlation is equal to zero


#3. What is the 83% confidence around your estimate of rho?

conf.test <- cor.test(SL.prof, GM.density, method = "pearson", conf.level = 0.83)
conf.interval <- conf.test$conf.int
lower <- conf.interval[1]
upper <- conf.interval[2]


#4. Build a general linear model of the data where proficency score predicts brain matter. Which factor is all of the error      confined to?

hist(SL.prof)
hist(GM.density)
library(fBasics)
normalTest(SL.prof, method = "da")
normalTest(GM.density, method = "da")

model <- lm(GM.density~SL.prof)

# All of the error is confined to the measurement of grey matter density since one of the assumptions of a linear model is that the explanatory variable (in this case second language proficiency) is measured without error.


#5. Theory predicts that the slope should be 0.04. Test this statistical hypothesis.

slope <- cov(GM.density,SL.prof)/var(SL.prof)
theta <- 0.04

df <- length(SL.prof) - 2
SS.error <- sum((GM.density-model$fitted.values)^2)
MS.error <- SS.error/df
std.error.b1 <- sqrt(MS.error/sum((SL.prof-mean(SL.prof))^2))

t.stat <- (slope-theta)/std.error.b1

slope.prob <- pt(abs(t.stat),df,lower.tail=FALSE)*2 # Probability that the slope of the model equals the theory predicted slope


#6. How much variation in gray matter density does your model explain?

explained.var <- summary(model)[[8]] # 67%

