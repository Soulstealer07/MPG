rx<-seq(-10,10,.01)
y<-2+3*x^2-x
plot(y,main="Y is a function of X",xlab="x",col="pink")

x<-seq(0,1,.01)
y<-12*(x)*(1-x)^2
plot(y,main="Beta Distribution with alpha = 2 and beta = 3",xlab="x",col="pink")

install.packages("XLConnect")
library(XLConnect)

data(AutoMPG)
head(AutoMPG,3)
sapply(AutoMPG,class)
AutoMPG$horsepower = as.numeric(AutoMPG$horsepower)
sapply(AutoMPG,class)


any(is.na(as.numeric(AutoMPG$horsepower)))
which(is.na(as.numeric(AutoMPG$horsepower)))


summary(AutoMPG)
sapply(AutoMPG,sd)
pairs(AutoMPG[,-9])
install.packages("corrplot")
library(corrplot)
corrplot(cor(AutoMPG[,-9][complete.cases(AutoMPG[,-9]),]),
                          method = "square",type="upper")
x<-AutoMPG$acceleration
y<-AutoMPG$mpg
plot(x,y,xlab="Acceleration",ylab="Miles Per Gallon",main="Bivarite X and Y Relationship",col="blue",pch=19,xlim=c(0,25),ylim=c(0,47))
cor(x,y)

linearmodel <- lm(y ~ x)
summary(linearmodel)
abline(a=4.9698,b=1.1912,col="pink",lwd=3)

x<-AutoMPG$acceleration
y<-4.9698+1.1912*(x)
length(y)
length(x)
summary(y)
summary(x)
par(mfrow=c(1,1))

plot(y,col="green",pch=19,xlim=c(0,length(y)),ylim=c(0,47),xlab="Observation in Data Set",ylab="MPG",main="Observed vs. Predicted MPG")
points(AutoMPG$mpg,col="blue",pch=19)
legend(50,45,legend=c("Original MPG","Predicted MPG"),col=c("blue","green"),pch=c("o","+"),lty=c(1,2),ncol=1,cex=.9)

#MSE Sum of Squared Residuals divided by n-p


factor<-1/(length(AutoMPG$mpg)-2)
factor
MSE<-factor*sum(linearmodel$residuals^2)
anova(linearmodel)
MSE

#50.42449 is approximately equal to 50.4


loessmodel<-loess(AutoMPG$mpg ~ AutoMPG$acceleration,data=AutoMPG)
loessmodel10<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.10)
loessmodel20<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.20)
loessmodel30<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.30)
loessmodel40<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.40)
loessmodel50<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.50)
loessmodel60<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.60)
loessmodel70<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.70)
loessmodel80<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.80)
loessmodel90<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.90)
loessmodel1 <-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=1.0)
optimalloess<-loess(AutoMPG$mpg ~ AutoMPG$acceleration, data = AutoMPG, span=.386)
summary(optimalloess)
summary(loessmodel)
smoothed75 <- predict(loessmodel)
smoothed10 <- predict(loessmodel10)
smoothed20 <- predict(loessmodel20)
smoothed30 <- predict(loessmodel30)
smoothed40 <- predict(loessmodel40)
smoothed50 <- predict(loessmodel50)
smoothed60 <- predict(loessmodel60)
smoothed70 <- predict(loessmodel70)
smoothed80 <- predict(loessmodel80)
smoothed90 <- predict(loessmodel90)
smoothed1  <- predict(loessmodel1)
optimalsmoothed <- predict(optimalloess)
plot(AutoMPG$acceleration,AutoMPG$mpg,xlim=c(0,25),ylim=c(0,47),col="blue",pch=19,main="Loess Smoothing Technique",xlab="Acceleration",ylab="MPG")
lines(smoothed75,lwd=2,lty=1,col="pink")
lines(smoothed10,lwd=2,lty=1,col="lightgreen")
lines(smoothed20,lwd=2,lty=1,col="red")
lines(smoothed30,lwd=2,lty=1,col="black")
lines(smoothed40,lwd=2,lty=1,col="brown")
lines(smoothed50,lwd=2,lty=1,col="purple")
lines(smoothed60,lwd=2,lty=1,col="darkgreen")
lines(smoothed70,lwd=2,lty=1,col="yellow")
lines(smoothed80,lwd=2,lty=1,col="darkred")
lines(smoothed90,lwd=2,lty=1,col="orange")
lines(smoothed1 ,lwd=2,lty=1,col="gold")
lines(optimalsmoothed, lwd=3,lty=1,col="navyblue")
summary(loessmodel)
summary(loessmodel10)
legend(0,47,legend=c("SP .75","SP .10", "SP .20", "SP .30", "SP .40", "SP .50", "SP .60", "SP .70", "SP .80", "SP .90", "SP 1","Optimal SP .375"),col=c("pink","lightgreen","red","black","brown","purple","darkgreen","yellow","darkred","orange","gold","navyblue"),lty=c(1,1,1,1,1,1,1,1,1,1,1,1),ncol=1,cex=.75)
plot(AutoMPG$acceleration,AutoMPG$mpg,col="blue",pch=19,main="Loess Smoothing Technique",xlab="Acceleration",ylab="MPG")
lines(optimalsmoothed, lwd=3,lty=1,col="navyblue")
