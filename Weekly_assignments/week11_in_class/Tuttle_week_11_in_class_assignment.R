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

library(lme4)
mod <- lmer(Invasive.Algae.Mass ~ Location + Treatment + Water.Depth + Native.Algae.Mass + (1|Location) + (1|Treatment) + (1|Water.Depth) + (1|Native.Algae.Mass), data = algae_data)

summary(mod)

library(car)
Anova(mod)

