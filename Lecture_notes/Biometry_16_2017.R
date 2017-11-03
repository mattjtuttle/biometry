
x1<-c(4,6,8,8,5,9,6,12,7,3)
x2<-c(9,8,11,14,9,10,6,13,12,8)

SSx1<-sum((x1-mean(x1))^2)
SSx2<-sum((x2-mean(x2))^2)
DF<-length(x1)+length(x2)-2

s.pooled<-sqrt((SSx1+SSx2)/DF)

t.stat<-(mean(x1)-mean(x2))/(s.pooled*sqrt((1/length(x1))+(1/length(x2))))
(pt(t.stat,18))*2 #we don't say lower.tail=TRUE here because t < 0

t.test(x1,x2,var.equal=TRUE)
t.test(x1,x2)#Welch, or Welch-Satterthwaite correct - adjusts degrees of freedom when variances are unequal


#Welch-Satterthwaite ###
x1sd<-sd(x1)
x2sd<-sd(x2)

numer<-((x1sd^2/length(x1))+(x2sd^2/length(x2)))^2
denom<-(1/(length(x1)-1))*(x1sd^2/length(x1))^2 + (1/(length(x2)-1))*(x2sd^2/length(x2))^2
numer/denom

######

set.seed(5)
x<-rnorm(50)
y<-rnorm(50,mean=2.5,sd=10)
t.test(x,y,var.equal=TRUE)
t.test(x,y)



#######
#non-parametrics, distribution free
#Mann-WHitney U test # not paired

a<-c(3,5,4,7,5,6,8,9,6,4,5,2,5,6,9,6,4,5,7)
b<-c(10,7,8,6,0,8,5,4,8,5,6,8,7,9,4,5,12,9,5)
diff<-a-b

mean(a-b)
sd(a-b)/sqrt(length(a))

cbind(a,b)

t.test(a,b,paired=TRUE)
t.test(diff,mu=0)

wilcox.test(a,b)

#alternate way
treat<-c(rep("a",length(a)),rep("b",length(b)))
as.data.frame(cbind(treat,c(a,b)))
wilcox.test(c(a,b)~treat)
t.test(rank(c(a,b))~treat)


#paired  -  
wilcox.test(a,b,paired=TRUE)
#t.test(rank(c(a,b))~treat,paired=TRUE)

#sign test / binomial test
difference<-a-b
sum(difference>0) # 7/19

binom.test(x=7,n=19,p=0.5,alternative="two.sided")
2*pbinom(7,19,prob=0.5)




########Variances

set.seed(2)
x1<-rnorm(50,sd=2)
x2<-rnorm(50,sd=3)

#F test
var.test(x1,x2)
var(x1)/ var(x2)
pf(var(x1)/var(x2),49,49,lower.tail=TRUE)*2





# Levines - a better option
library(car)
?leveneTest
groups<-c(rep("x1",length(x1)),rep("x2",length(x2)))
Data<-c(x1,x2)

leveneTest(Data~as.factor(groups))#Brown&Forsythe test
leveneTest(Data~as.factor(groups),center=mean)#original Levines test


dx1<-abs(x1-median(x1))
dx2<-abs(x2-median(x2))
leveneTest(Data~as.factor(groups))
t.test(dx1,dx2,var.equal=TRUE)#same answer






groups<-c(rep("x1",length(x1)),rep("x2",length(x2)))
Data<-c(x1,x2)

leveneTest(Data~as.factor(groups))
t.test(dx1,dx2,var.equal=TRUE)



### Likelihood ratio approach

mod.comb<-lm(Data~1)
mod.sep<-lm(comb~groups)
llred<-logLik(mod.comb)[1]
llfull<-logLik(mod.sep)[1]

LRT <- 2*(llfull-llred)
pchisq(LRT,1,lower.tail=FALSE)

library(lmtest)
lrtest(mod.comb,mod.sep)




#testing for different variances
samevar <- sum(dnorm(x1,mean=mean(x1),sd=sd(comb),log=TRUE))+sum(dnorm(x2,mean(x2),sd=sd(comb),log=TRUE))

diffvar <- sum(dnorm(x1,mean=mean(x1),sd=sd(x1),log=TRUE))+sum(dnorm(x2,mean(x2),sd=sd(x2),log=TRUE))

LRT <- abs(2*(samevar-diffvar))
pchisq(LRT,1,lower.tail=FALSE)






##########
#multiple tests
p.vals<-c(0.01,0.02,0.04)
p.adjust(p.vals,"bonferroni")
p.vals*3
p.adjust(p.vals,"holm")

p.vals<-c(0.01,0.03,0.04)
p.adjust(p.vals,"bonferroni")
p.adjust(p.vals,"holm")



# a worked example
A<-c(56,63,45,41,71,60,78,50,68,62)
B<-c(40,48,60,38,28,44,66,22,45,54)
C<-c(71,57,64,44,73,50,79,67,84,61)
t.test(A,B)
t.test(B,C)
t.test(A,C)
p.adjust(c(0.016,0.00247,0.3139),"bonferroni")


