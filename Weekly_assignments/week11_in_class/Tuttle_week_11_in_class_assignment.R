# Name: Matthew Tuttle
# Biometry week 11 in class assignment


#####################
# Experimental design
#####################

# Questions:
# Do parrotfish influence invasive algae cover?
# Does water depth play a role?
# 
# Cage design:
# 5 beaches, total of 160 cages
# 32 cages per beach
# 16 deep, 16 shallow at each beach
# 8 cages excluding parrotfish, 8 open cages that do not exclude parrotfish at each beach/depth
# 
# Measurements:
# At each cage, measure the amount of both native and invasive algae.


#########################
# Analysis of sample data
#########################

# Imports data
algae_data <- read.csv(file = "Weekly_assignments/week11_in_class/AlgaeProject.csv", header = TRUE)


# Adding structure to the error term and allowing for different intercepts using a mixed effects model
library(lme4)

mod1 <- lmer(Invasive.Algae.Mass ~ Treatment + (1|Location), data = algae_data)
summary(mod1)

mod0 <- lmer(Invasive.Algae.Mass ~ (1|Location), data = algae_data)
summary(mod0)
anova(mod0, mod1) # Treatment has a significant effect, keep mod1

# Allow for different slopes
mod2 <- lmer(Invasive.Algae.Mass ~ Treatment + (1+Treatment|Location), data = algae_data)
summary(mod2) # Corr = -0.16, will allow slope to vary
anova(mod1, mod2) # keep mod1, lower AIC

mod3 <- lmer(Invasive.Algae.Mass ~ Treatment * Water.Depth + (1+Water.Depth|Location), data = algae_data)
summary(mod3) # Corr = -1, keep mod1

mod4 <- lmer(Invasive.Algae.Mass ~ Treatment * Water.Depth + (1|Location), data = algae_data)
summary(mod4)
anova(mod1, mod4) # keep mod4, lower AIC

mod5 <- lmer(Invasive.Algae.Mass ~ Treatment + Water.Depth + (1|Location), data = algae_data)
summary(mod5)
anova(mod4, mod5) # keep mod4, lower AIC

mod6 <- lmer(Invasive.Algae.Mass ~ Treatment * Water.Depth * Native.Algae.Mass + (1+Native.Algae.Mass|Location), data = algae_data)
summary(mod6) # Corr = -0.70, will allow slope to vary
anova(mod4, mod6) # keep mod4, lower AIC

mod7 <- lmer(Invasive.Algae.Mass ~ Treatment * Water.Depth + Native.Algae.Mass + (1|Location) + (1+Native.Algae.Mass|Location), data = algae_data)
summary(mod7)
anova(mod4, mod7) # keep mod4, lower AIC

final.mod <- mod4 # To be used for hypothesis testing


# Hypothesis testing
library(multcomp)
tukey.hsd<-glht(final.mod,linfct=mcp(Treatment="Tukey"))
summary(tukey.hsd)

# Using the data at hand and the above generated statistical model, there does not appear to be an effect of parrotfish on invasive Macroalgae cover. Hypothesis testing indicated that difference between the caged vs non-caged treatments is zero with P-value <2e-16. 

