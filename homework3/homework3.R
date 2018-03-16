#1(b)
datay = read.table('F:\\stats191\\homework3\\Play.txt',header = FALSE);
data = read.table('F:\\stats191\\homework3\\Locomotion.txt',header = FALSE);
data[,2] = datay
X = data[, 1]
Y = data[, 2]
plot(X, Y,xlab="Xs",ylab="Y", main="X vs Y")
data.lm = lm(Y~X, data)
summary(data.lm)
abline(data.lm, col = "red")
residual = Y - predict(data.lm, data)
qqnorm(residual)
qqline(residual)
hatvalues(data.lm)
library(car)
library(MASS)
rstandard(data.lm)
dffits(data.lm)[1]
dfbetas(data.lm)[1,]
cooks.distance(data.lm)[1]

# is the data normal?

#transformation
dataTrans = data
dataTrans = log(dataTrans)
XTrans = dataTrans[, 1]
YTrans = dataTrans[, 2]
plot(XTrans, YTrans,xlab="XTrans",ylab="YTrans", main="XTrans vs YTrans")
dataTrans.lm = lm(Yqqline(XTrans~XTrans, dataTrans)
summary(dataTrans.lm)
abline(dataTrans.lm, col = "red")
hatvalues(dataTrans.lm)
rstandard(dataTrans.lm)
dffits(dataTrans.lm)[1]
dfbetas(dataTrans.lm)[1,]
cooks.distance(dataTrans.lm)[1]

#delete the point 1
X2 = XTrans[-1]
Y2 = YTrans[-1]
dataTrans2.lm = lm(Y2~X2)
summary(dataTrans2.lm)
abline(dataTrans2.lm, col = "blue")


#2(a)
data = read.table('F:\\stats191\\homework3\\Job.txt',header = FALSE);
stem(data[,1], scale=1,width=80)
stem(data[,2], scale=1,width=80)
stem(data[,3], scale=1,width=80)
stem(data[,4], scale=1,width=80)

#2b
par(mfrow=c(4,1))
plot(data[,1],data[,5])
plot(data[,2],data[,5])
plot(data[,3],data[,5])
plot(data[,4],data[,5])

#2c
cor(data[,1:4])

#2d
data = read.table('F:\\stats191\\homework3\\Job.txt',header = FALSE);
Y = data[,5]
X1 = data[,1]
X2 = data[,2]
X3 = data[,3]
X4 = data[,4]
data.lm = lm(Y~X1 + X2 + X3 + X4)
summary(data.lm)

#2e
library(car)
vif(data.lm)

#2f
residual = Y - predict(data.lm, data)
par(mfrow=c(3,1))
plot(predict(data.lm, data),residual, ylab="residual",xlab="Yfit", main="Residual vs Yfit")
abline(h=0,col='red')
plot( X1, residual, ylab="residual",xylab="X1", main="Residual vs X1")
abline(h=0,col='red')
plot( X2, residual, ylab="residual",xlab="X2", main="Residual vs X2")
abline(h=0,col='red')
par(mfrow=c(2,1))
plot( X3, residual, ylab="residual",xlab="X3", main="Residual vs X3")
abline(h=0,col='red')
plot( X4, residual, ylab="residual",xlab="X4", main="Residual vs X4")
abline(h=0,col='red')
qqnorm(residual)
qqline(residual)

#2g
library(leaps)
data.leaps = leaps(data[,1:4], Y, method=c("adjr2"))
data.leaps$adjr2 
data.leaps = leaps(data[,1:4], Y, method=c("Cp"))
data.leaps$Cp
data.leaps = leaps(data[,1:4], Y, method=c("r2"))
data.leaps$r2

#2i
regfit.fwd <- regsubsets(Y ~ X1 + X2 + X3 + X4, data = data, method = "forward")
reg.summary.fwd <- summary(regfit.fwd)
plot(reg.summary.fwd$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(reg.summary.fwd$cp), reg.summary.fwd$cp[which.min(reg.summary.fwd$cp)], col = "red", cex = 2, pch = 20)

#2k
data2.lm = lm(Y~X1 + X3)
residual2 = Y - predict(data2.lm, data)
press = sum(residual2/(1-hatvalues(data2.lm)) * residual2/(1-hatvalues(data2.lm)))
see = sum(residual2 * residual2)

#2l
test = read.table('F:\\stats191\\homework3\\Jobval.txt',header = FALSE);
cor(test[,1:4])

#2m
Y.test = test[,5]
X1.test = test[,1]
X2.test = test[,2]
X3.test = test[,3]
X4.test = test[,4]
test.lm = lm(Y.test~X1.test + X3.test)
summary(test.lm)

#2n
data.train = read.table('F:\\stats191\\homework3\\Job.txt',header = FALSE);
names(data.train) <- c("X1","X2","X3","X4","Y")
train.lm = lm(Y ~ X1 + X3, data.train)
Y = data.train[,5]
residual = Y - predict(train.lm, data.train)
mse = sum(residual * residual)/25

data.test = read.table('F:\\stats191\\homework3\\Jobval.txt',header = FALSE);
names(data.test) <- c("X1","X2","X3","X4","Y")
Y.test = data.test[,5]
residual2 = Y.test - predict(train.lm, newdata = data.test)
mspr = sum(residual2 * residual2)/25

#2o
train[26:50,] = test
train.lm = lm(data.train[,5] ~ data.train[,1] + data.train[,3])
summary(train.lm)




#(b)
Yfit = predict(research.lm, research)
residual = Yfit - Y
boxplot(residual, ylab="residuals")

#(C)
qqnorm(residual)
plot(Yfit,residual,xlab="Fitted value",ylab="Residuals", main="Residuals VS Fittend Value")
abline(h=0,col='red')

stdres = rstandard(research.lm)
par(mfrow=c(3,2))
plot(X1,stdres,xlab="X1",ylab="Standardized Residuals", main="Standardized Residuals VS X1")
abline(h=0,col='red')
plot(X2,stdres,xlab="X2",ylab="Standardized Residuals", main="Standardized Residuals VS X2")
abline(h=0,col='red')
plot(X3,stdres,xlab="X3",ylab="Standardized Residuals", main="Standardized Residuals VS X3")
abline(h=0,col='red')
plot(X1*X2,stdres,xlab="X1*X2",ylab="Standardized Residuals", main="Standardized Residuals VS X1*X2")
abline(h=0,col='red')
plot(X1*X3,stdres,xlab="X1*X3",ylab="Standardized Residuals", main="Standardized Residuals VS X1*X3")
abline(h=0,col='red')
plot(X2*X3,stdres,xlab="X2*X3",ylab="Standardized Residuals", main="Standardized Residuals VS X2*X3")
abline(h=0,col='red')

#(e)
X4=X1+X3
research.lm2 = lm(Y~X4+X2,research)
anova(research.lm,research.lm2)

#(f)
par(mfrow=c(3,1))
residualWithoutX1=Y-predict(lm(Y~X2+X3,research))
fitX1=X1-predict(lm(X1~X2+X3,research))
plot(residualWithoutX1,fitX1,xlab="X1 Residuals",ylab="y Residuals", main="X1 Residuals VS Y Residuals")


residualWithoutX2=Y-predict(lm(Y~X1+X3,research))
fitX2=X2-predict(lm(X2~X1+X3,research))
plot(residualWithoutX2,fitX2,xlab="X2 Residuals",ylab="y Residuals", main="X2 Residuals VS Y Residuals")



residualWithoutX3=Y-predict(lm(Y~X1+X2,research))
fitX3=X3-predict(lm(X3~X1+X2,research))
plot(residualWithoutX3,fitX3,xlab="X3 Residuals",ylab="y Residuals", main="X3 Residuals VS Y Residuals")


#(g)
par(mfrow=c(1,1))
plot(research.lm)
abline(v=0.333,col='blue')
hatvalues(research.lm)


#(h)
install.packages("car")
library(car)
library(MASS)
studres(research.lm)
outlierTest(research.lm)

#(i)
dffits(research.lm)[19]
dfbetas(research.lm)[19,]
cooks.distance(research.lm)[19]


#(j)
beta=research.lm$coefficients 
sums=0
for (i in 1:24){
  sums=sums+residual[i]*residual[i]  #SSE
}

error=sums/22    #SSE/(n-2)
X=research[,1:3]
X=cbind(1, X)    #Design matrix

D=matrix(nrow=24,ncol=1)  #Enpty matrix for Di
for (j in 1:24) {
  Y0=Y[-j]
  X10=X1[-j]
  X20=X2[-j]
  X30=X3[-j]
  research.lm3=lm(Y0~X10+X20+X30)
  beta1=research.lm3$coefficients
  Dvalue=t(beta-beta1) %*% t(X)%*%as.matrix(X) %*% (beta-beta1)  #Calculate Di
  Dvalue=Dvalue/4
  D[j]=Dvalue/error
  Dvalue=0
}
  


