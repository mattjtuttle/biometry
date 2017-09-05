#Return to eebbiometry@gmail.com by 11:15pm, Friday September 8.
#   on the subject line put    YOURLASTNAME_EXERCISE
# Save the file as "Your_last_name_Weekly_assignment_2.R"

#Name: Matthew Tuttle

# Rickettsia is a genus of bacteria known to infect insects. Assume for a population of mosquitos the probability of infection is 0.23. 

prob_infected <- 0.23

# 1) You collect 75 mosquitos and find that 8 are infected. How many unique ways could this have happened (i.e., how many different ways can you sample 8 out of 75)?

q1 <- choose(75, 8)

# 2) If you were to collect 75 mosquitos, what is the probability that the first eight that were collected were infected, and the remainder where not?

q2 <- (prob_infected ^ 8) * ((1 - prob_infected) ^ (75 - 8))

# 3) If you were to collect 75 mosquitos, what is the probability that you would have between 17 and 45 (inclusive) infected individuals?

q3 <- pbinom(45, 75, prob_infected) - pbinom(16, 75, prob_infected)


###Rapid HIV tests allow for quick diagnosis without expensive lab equipment. However, their efficacy has been called into question. In a population of 1517 tested individuals in Uganda, 4 had HIV but tested negative (false negatives), 166 had HIV and tested positive, 129 did not have HIV but tested positive (false positives), and 1218 did not have HIV and tested negative.

# 4. What was the probability of a false-positive?

prob_false_pos <- 129 / (129 + 166)

# 5. What was the false negative rate?

prob_false_neg <- 4 / (4 + 1218)

# 6. If a randomly sampled individual from this population tests positive, what is the probability that he or she has HIV?

likelihood <- 1 - prob_false_pos # Probability of an actual positive test

prior <- (4 + 166) / (4 + 166 + 129 + 1218) # Percent of population that actually has HIV

observed <- (likelihood * prior) + (prob_false_pos * (1 - prior)) # Total probability of a positive test

prob_HIV <- (likelihood * prior) / observed # Probability the individual has HIV
