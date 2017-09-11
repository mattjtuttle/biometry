#Return to eebbiometry@gmail.com by 11:15pm, Friday September 8.
#   on the subject line put    YOURLASTNAME_EXERCISE
# Save the file as "Your_last_name_Weekly_assignment_2.R"

#Name:

# Rickettsia is a genus of bacteria known to infect insects. Assume for a population of mosquitos the probability of infection is 0.23. 



# 1) You collect 75 mosquitos and find that 8 are infected. How many unique ways could this have happened (i.e., how many different ways can you sample 8 out of 75)?

choose(n = 75,k = 8)

# 2) If you were to collect 75 mosquitos, what is the probability that the first eight that were collected were infected, and the remainder where not?

p<-0.23
q<- 1-p
prob<-(p^8)*(q^(75-8))
prob
# 3) If you were to collect 75 mosquitos, what is the probability that you would have between 17 and 45 (inclusive) infected individuals?

bet.prob<- (pbinom(16,75,p,lower.tail = FALSE))-(pbinom(45,75,p,lower.tail = FALSE))
bet.prob


###Rapid HIV tests allow for quick diagnosis without expensive lab equipment. However, their efficacy has been called into question. In a population of 1517 tested individuals in Uganda, 4 had HIV but tested negative (false negatives), 166 had HIV and tested positive, 129 did not have HIV but tested positive (false positives), and 1218 did not have HIV and tested negative.

#### First I want to set up a table:

cont.mat<-matrix(c(NA,NA,NA,NA),nrow=2) #An empty 2 by 2 matrix
rownames(cont.mat)<-c("positive","negative") #Rows seperated by test result
colnames(cont.mat)<-c("HIV","No HIV") #columns seperated by condition
cont.mat[1,1]<-166
cont.mat[1,2]<-129
cont.mat[2,1]<-4
cont.mat[2,2]<-1218
cont.mat

#Or better yet, convert to probailities now.

cont.mat1<-matrix(c(NA,NA,NA,NA),nrow=2)
rownames(cont.mat1)<-c("positive","negative")
colnames(cont.mat1)<-c("HIV","No HIV")
cont.mat1[,1]<-cont.mat[,1]/sum(cont.mat[,1])
cont.mat1[,2]<-cont.mat[,2]/sum(cont.mat[,2])
cont.mat1

# 4. What was the probability of a false-positive?
FP<-129/1347
FP #Matches the table we made.


# 5. What was the false negative rate?
FN<-4/170
FN

# 6. If a randomly sampled individual from this population tests positive, what is the probability that he or she has HIV?

likelihood<-cont.mat1[1,1] #0.976 The likelyhood of the data, the positive rate


prior<-170/1517 #0.1121 How many people have HIV out of everyone in the study

p.dat<-cont.mat1[1,1]*prior+cont.mat1[1,2]*(1-prior) #0.1945 The probability of the data

post<-likelihood*prior/p.dat #Bayes
post #0.562

