data=read.table('F:\\stats191\\homework1\\NewspapersData2.txt',header = FALSE);
Daily=data[,1];
Sunday=data[,2]
plot(data[,1],data[,2],xlab="Daily",ylab="Sunday")
data.lm=lm(Sunday~Daily,data)
summary(data.lm)
abline(data.lm, col = "red")
confint(data.lm, level=.95)
predict(data.lm,data.frame(Daily=500),interval="confidence")
predict(data.lm,data.frame(Daily=500),interval="prediction")


hubble=read.table('F:\\stats191\\homework1\\GalacticClusters2.txt',header = FALSE);
Distance=hubble[,1]
Velocity=hubble[,2]
hubble.lm=lm(Velocity~Distance-1,hubble)
summary(hubble.lm)

plot(Distance,Velocity)
hubble.lm=lm(Velocity~Distance,hubble)
summary(hubble.lm)
plot(Distance,Velocity-predict(hubble.lm,hubble),ylab="Residuals")
abline(h=0,col='red')
abline(hubble.lm, col = "red")
qqnorm(Velocity-predict(hubble.lm,hubble))
hubble.res = resid(hubble.lm)
par(mfrow=c(2,2))
plot(hubble.lm)
plot(predict(hubble.lm,hubble),Velocity-predict(hubble.lm,hubble),xlab="Fitted value",ylab="Residuals")