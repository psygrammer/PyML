
## Galton's Data
install.packages("UsingR")
library(UsingR)

data(galton)
par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)
par(mfrow=c(1,1))


## to see what value of minimizes the sum of the squared deviations
install.packages("manipulate")
library(manipulate)

myHist<-function(mu){
  hist(galton$child,col="blue",breaks=100)
  lines(c(mu,mu),c(0,150),col="red",lwd=5)
  mse<-mean((galton$child-mu)^2)
  text(63,150,paste("mu=",mu))
  text(63,140,paste("MSE=",round(mse,2)))
}
manipulate(myHist(mu),mu=slider(62,74,step=0.5))


## The least squares estimate is the empirical mean
```{r}
hist(galton$child,col="blue",breaks=100)
meanChild<-mean(galton$child)
lines(rep(meanChild,100),seq(0,150,length=100),col="red",lwd=5)


## Comparing childrens' heights and their parents' heights
plot(galton$parent,galton$child,pch=19,col="blue")


## Regression through the origin
myPlot<-function(beta){
  y<-galton$child-mean(galton$child)
  x<-galton$parent-mean(galton$parent)
  freqData<-as.data.frame(table(x,y))
  names(freqData)<-c("child","parent","freq")
  plot(
    as.numeric(as.vector(freqData$parent)),
    as.numeric(as.vector(freqData$child)),
    pch=21,col="black",bg="lightblue",
    cex=.15*freqData$freq,
    xlab="parent",
    ylab="child"
  )
  abline(0,beta,lwd=3)
  points(0,0,cex=2,pch=19)
  mse<-mean((y-beta*x)^2)
  title(paste("beta=",beta,"mse=",round(mse,3)))
}
manipulate(myPlot(beta),beta=slider(0.6,1.2,step=0.02))

## best fit line
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)

reg<-lm(child~ parent, data=galton)
plot(galton$parent, galton$child)
abline(reg, lty=2, lwd=2, col='red')



