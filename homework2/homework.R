#(a)
research = read.table('F:\\stats191\\homework2\\math.txt',header = FALSE);
X1=research[,1];
X2=research[,2]
X3=research[,3]
Y=research[,4]
research.lm = lm(Y~X1+X2+X3,research)
summary(research.lm)

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
  


