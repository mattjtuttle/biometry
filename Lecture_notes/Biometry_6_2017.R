
################
#Covariance

length.cm<-c(64,69,71,67,63,62,66,60,65,68)
weight.g<-c(130,148,180,175,120,127,141,118,120,159)

cbind(length.cm,weight.g)

plot(length.cm,weight.g,pch=19,las=1);points(mean(length.cm),mean(weight.g),pch=19,cex=2,col="red")

length.minus.mean<-length.cm-mean(length.cm)
weight.minus.mean<-weight.g-mean(weight.g)

cbind(length.cm,weight.g,length.minus.mean,weight.minus.mean)

product<-length.minus.mean*weight.minus.mean

cbind(length.cm,weight.g,length.minus.mean,weight.minus.mean,product)


sum.product<-sum(product)
n<-length(product)
Covariance <- sum.product/(n-1)
cov(length.cm,weight.g)

length.in<-length.cm/2.54
weight.lbs<-weight.g*0.00220462
plot(length.in,weight.lbs,pch=19,las=1);#plot(length.cm,weight.g,pch=19,las=1)
cov(length.in,weight.lbs)

length.ft<-length.in/12
cov(length.ft,weight.lbs)

#Put it in z units
#covariance of z-transformed data IS the correlation
z.length<-(length.cm-mean(length.cm))/ sd(length.cm)
z.weight<-(weight.g-mean(length.cm))/sd(weight.g)

cov(z.length,z.weight)
cor(length.cm,weight.g)

cov(scale(length.cm),scale(weight.lbs))
cov(scale(length.in),scale(weight.lbs))







cor(weight.g,length.cm)

SEr <- sqrt((1-cor(weight.g,length.cm)^2 )/(10-2))
t.stat <- cor(weight.g,length.cm)/SEr
one.tailed.p<-pt(t.stat,df=8,lower.tail=FALSE)

t.stat.n <- (cor(weight.g,length.cm)-0.6)/SEr
one.tailed.pn<-pt(t.stat.n,df=8,lower.tail=FALSE)


one.tailed.p<-1-pt(t.stat,df=10-2);
curve(dt(x,df=10-2),xlim=c(-5,5),ylim=c(0,0.5))
abline(v=0,lty=2,col="red",lwd=3)
text(-3,0.4,"50%",cex=4)
text(3,0.4,"50%",cex=4)

curve(dt(x,df=10-2),xlim=c(-5,5),ylim=c(0,0.5))
abline(v=t.stat,lty=2,col="red",lwd=3)
text(0,0.45,paste(round(pt(t.stat,df=10-2)*100,digits=3),"%"),cex=2)
text(4.9,0.3,paste(round((1-pt(t.stat,df=10-2))*100,digits=3),"%"),cex=2,srt=270)


curve(pt(x,df=10-2),xlim=c(-4,5),ylim=c(0,1))
abline(v=0,lty=2,lwd=4,col="red")
text(-1,0.8,"50%",cex=3)
text(1,0.8,"50%",cex=3)



curve(pt(x,df=10-2),xlim=c(-2,5),ylim=c(0,1))
abline(v=t.stat,lty=3,lwd=4,col="red")
p.value<-2*one.tailed.p
abline(h=pt(t.stat,df=10-2),lty=2,lwd=3,col="blue")
arrows(3,0.6,4,pt(t.stat,df=10-2)-0.05)
arrows(3,0.5,4,0.1)
text(3,0.55,paste("p=",round(pt(t.stat,df=10-2),digits=4)),cex=2)

one.tailed.p<-pt(t.stat,df=10-2,lower.tail=FALSE)
two.tailed.p.value<-2*one.tailed.p