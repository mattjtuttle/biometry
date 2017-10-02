# spearman vs. pearson
#spearman is often called a non-parametric correlation
#spearman correlation is simply the Pearson Product Moment correlation of rank transformed data

length.cm<-c(64,69,71,67,63,62,66,60,65,68)
weight.g<-c(130,148,180,175,121,127,141,118,120,159)

cor.test(length.cm,weight.g,method="spearman")
cor.test(rank(length.cm),rank(weight.g),method="pearson")


#Kendall Tau
# = (C-D)/(C+D)
# Concordance is the number of rankings that are above the focal ranking (the second column)
X<-c(1,2,3,4,5,6,7,8,9,10,11,12)
Y<-c(2,1,4,3,6,5,8,7,10,9,12,11)
plot(X,Y)
dat<-cbind(X,Y);colnames(dat)<-c("Jim's","yours")# for 1, there are 10 rankings higher than 2  for 2, there are 10 rankings higher than 1  for 4, there are 8 rankings higher and so on...
# C<-c(10,10,8,8,6,6,4,4,2,2,0,NA)# add NA for showing cbind dat
# D<-c(1,0,1,0,1,0,1,0,1,0,1,NA)
# cbind(dat,C,D)
(tau<-(sum(C)-sum(D))/(sum(C)+sum(D)))

cor(X,Y,method="kendall")
cor(X,Y,method="spearman")

Y<-c(12,1,2,3,4,5,6,7,8,9,10,11)
dat<-cbind(X,Y);colnames(dat)<-c("Jim's","yours")# 
plot(X,Y)
cor(X,Y,method="kendall")
cor(X,Y,method="spearman")