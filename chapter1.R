#Chapter 1
#1.
rm(list = ls())
setwd("~/Public/Google Drive/R library/")
data("studentdata")
attach(studentdata)
hist(Dvds, xlab = "Number of movie DVDs owned by students")
summary(Dvds)
table(Dvds)
barplot(table(Dvds))

#2.
output=boxplot(Height~Gender)
output
DHeight=mean(Height[Gender=="male"], na.rm = T)-mean(Height[Gender=="female"], na.rm = T)
DHeight

#3.
plot(ToSleep, WakeUp, xlab = "ToSleep", ylab = "WakeUp", 
     main = "Scatterplot of ToSleep and WakeUp")
Fit <- lm(WakeUp~ToSleep)
abline(Fit, lty = 2, col=2)
x.new <- 0
y.new=fitted(Fit, x.new)points(x.new,mean(y.new), col = "blue", pch=16,cex=1)
abline(v=0,col="blue")
abline(h=mean(y.new),col="blue")

#4.
binomial.conf.interval=function(y,n){
  z=qnorm(.95)
  phat=y/n
  se=sqrt(phat*(1-phat)/n)
  return(c(phat-z*se,phat+z*se))
}
n=20;p=0.05;N=20
#n=20;p=0.5;N=20
conf.int=matrix(nrow=N, ncol = 2)
for(i in 1:N){
  y=rbinom(1,n,p)
  conf.int[i,]=binomial.conf.interval(y,n)
}
true.confint=colMeans(conf.int)
true.confint
  
#5
binomial.conf.interval=function(y,n){
        z=qnorm(.95)
        phat=y/n
        se=sqrt(phat*(1-phat)/n)
        return(c(phat-z*se,phat+z*se))
      }
True.conf.interval <- function(n,p,m){
  conf.int=matrix(nrow=m,ncol=2)
  for(i in 1:m){
    y=rbinom(1,n,p)
    conf.int[i,]=binomial.conf.interval(y,n)
    }
  true.confint=colMeans(conf.int)
  return(true.confint)
}
True.conf.interval(100,.05,1000)
True.conf.interval(100,.25,1000)
True.conf.interval(100,.50,1000)




