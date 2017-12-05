library(vegan)

#PCA
#helpful for high dimensional data
#eg. if we have 30 predictors, there are 30(30-1)/2 possible scatter plots
#PCA takes highly dimensional data and captures as much information as possible with fewer variables

#Principle components

#Start with set of predictors
#X¹, X²...,Xp

#The principle component can be written as:

#Z¹ = Φ¹¹X¹ + Φ²¹X² + Φ³¹X³ + .... +Φp¹Xp

#  Z¹ is the first PC
#   Φp¹ is the loading vector comprising loadings (Φ¹, Φ²..)
# Loadings are constrained to a sum of squares = 1 (so that way each PC will have the same variance and the possible problem of variance increasing with large loadings is taken care of). It defines the direction of the principle components along which data varies the most. It results in a line in p dimensional space which is closest to the n observations. Closeness is measured using average squared euclidean distance.
#X¹..Xp  are normalized predictors. Normalized predictors have mean equals to zero and standard deviations equal to one (sound familiar?).

fact<-c("M","M","M","M","M","A","A","A","A","A")
cols<-c(rep("blue",5),rep("red",5))
dat.v<-c(0.19627,0.76549,0.86939,0.63961,0.29376,2.58203,2.79745,1.50102,2.24673,1.34131,2.31652,1.59583,0.8965,3.95341,4.57293,1.93889,2.074,2.17505,1.26667,0.79056,0.00016,4e-05,0.85864,0.14139,0.00955,0.01774,6e-04,0.01042,2e-04,0.6553,2.48705,2.63865,2.37547,0.2656,0.12375,0.46134,0.12795,1.31351,1.4864,2.21283)
dat<-matrix(dat.v,nrow=10);colnames(dat)<-c("falx","humerulus","uncus","elbow")

cor(dat)# just checking out the correlations across the data - we want to take those correlations and put them together on a single axis. That is the goal. Goals are good.

PRCOMP<-prcomp(dat,scale.=TRUE)
summary(PRCOMP)# PC1 captures ~52% of the variation... etc.

names(PRCOMP)
PRCOMP$sdev^2 #squaring this will give the eigenvalue for each PC
PRCOMP$sdev^2/sum(PRCOMP$sdev^2)# the % explained comes from the amount of variance explained. This line of code turns the stdev into variance... thus, we can quantify how much of the variance is captured by each axis.

PRCOMP$rotation#same as loadings


#a few plots - PC1vsPC2 and PC1vsPC3
plot(PRCOMP $x[,1], PRCOMP $x[,2],xlab="PC 1 (51%)",ylab="PC 2 (33%)",col=cols)
plot(PRCOMP $x[,1], PRCOMP $x[,3],xlab="PC 1 (51%)",ylab="PC 3 (15%)",col=cols)

#a different built in function to do the same thing....
PRINCOMP<-princomp(dat,cor=TRUE)#Uses Q-mode (the singular value decomposition methos) 
#considered better for numerical accuracy - I don't like it because it doesn't 
#give you all the loadings
summary(PRCOMP)
PRINCOMP$loadings #same answer



fact<-c("M","M","M","M","A","A","A","A","A","M")#define the different groups
rownames(dat)<-fact
mod<-rda(dat~1,scale=TRUE)#redundancy analysis with no factors IS a PCA
summary(PRCOMP) #the loadings are the same as the PCA above
#1.4379^2 $stdev from PRINCOMP = Eigenvalue of mod

mod<-rda(dat~fact,scale=TRUE)# Now we have a constrained analysis... we want to know the parts of the variation might explain the difference between the two factors
summary(mod) #29% of variation explains difference between the two factors
anova(mod)
coef(mod)
plot(mod,display=c("sites","species","bp"))
plot(mod,display=c("sites","species","bp","cn"))
plot(mod,scaling="site",display=c("sites","species","bp"))#scaline by rows
plot(mod,scaling="species",display=c("sites","species","bp"))#scaling by columns



dat<-read.csv("wingMORPHOMETRICSBIOMETRY.csv",header=TRUE)

head(dat)

n.dat<-dat[,3:34]
popcols<-c(rep("red",18),rep("blue",26),rep("green",10))#blues are CarsonPass
pops<-dat[,2]


#PCA
pca<-princomp(n.dat,cor=TRUE,scores=TRUE)# Uses Q-mode (the singular value decomposition methos) considered better for numerical accuracy - I don't like it because it doesn't give you all the loadings
names(pca)
summary(pca)
pca$sdev## squaring this will give the eigenvalue for each PC
pca$sdev[1]^2/sum(pca$sdev^2) # % of variation decribed by eigen vector 1
pca$loadings
pca$scores
plot(pca$scores,pch=19,col=popcols)


biplot(pca,scale=1)# "sites" scaled
biplot(pca,scale=2)# "species" scaled / eigen vector angles matter
pca$scores #axis 3,4

# # 



#
#PCA and such are metric which is why we can do this
#sqrt of jaccard is too - jaccard =  j11 / (j01+j10+j11)

x1<-c(0,1,1,0,0,1)
x2<-c(1,1,1,1,1,0)
dd<-rbind(x1,x2)
1-2/6 #one minus the similarity gives us jaccard distance (actually, dissimilarity)
vegdist(dd,"jaccard")

library(ade4)
is.euclid(vegdist(n.dat,method="jaccard"))
is.euclid(sqrt(vegdist(n.dat,method="jaccard")))#the sqrt of jaccard is metric!

distb<-vegdist(n.dat,method="jaccard")
rmod<-dbrda(distb~pops)
anova(rmod)
plot(rmod)
scores(rmod)




#NMDS
library(MASS)
nmds<-isoMDS(dism,k=2)


pops<-dat[,2]

popcols<-c(rep("red",18),rep("blue",26),rep("green",10))

plot(nmds$points[,1],nmds$points[,2],col=popcols,pch=19)

