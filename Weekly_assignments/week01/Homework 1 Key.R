# Homework 1  answer Key

#Please remember that there are a lot of correct ways to code theses answers. This just shows my favorite way.


#1. Assign to a vector called X the values 1,3,5,6,7, and 9. Assign to a vector called Y the values 2,7,4,5,2 and 12.

X <- c(1,3,5,6,7,9)
Y <- c(2,7,4,5,2,12)
X
Y

#2. Combine the two vectors together as one large vector called Z.

Z <- c(X,Y)
Z

#3. Create a new vector that includes all the values in Z that are less than 6.

Z[Z<6]
new.vector<-Z[Z<6]

#4. Using R functions, calculate the mean of Z.

mean(Z)

#5. Create an array where the columns are X and Y. Assign this array to an object called Q.

Q <- cbind(X,Y)
Q
#6. Using R functions, calculate the sum of each row.

apply(Q,1,sum)

#7. Calculate the mean of the rows of Q and add this as a third column.

Q <- cbind(Q, apply(Q,1,mean))
Q
#8. Extract the value of Q at the second row and first column.

Q[2,1]

#9. Write Q to a .csv file.

write.csv(Q, file="testQ.csv")

#10. Read the .csv file and assign it to an object.

something<-read.csv("testQ.csv")


#11 Show that the third number in the first column of Q is equal to the fourth number in the second column

Q[3,1]==Q[4,2]

#12 Make 'Q' a data frame

Q<-as.data.frame(Q)

#13 Using R functions, calculate the length of X?

length(X)

#14 Assign the name "mean" to the third column of Q?

colnames(Q)[3] <- "mean"
