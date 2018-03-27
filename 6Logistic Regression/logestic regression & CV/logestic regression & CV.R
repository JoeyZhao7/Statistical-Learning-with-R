# ipo6.csv logistic regression with polynomial terms
rm(list=ls())
d0 = ipo
d0
d1=d0[,c(1,2)]
names(d1)=c("Y","X")
d1$X=log(d1$X)
hist(d1$X)

# simple logistic regression
fit = glm(Y~X,binomial,d1)
summary(fit)
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)
#(Intercept) -7.6722 1.8041 -4.253 2.11e-05 ***
#X            0.4441 0.1075 4.130 3.62e-05 ***
#(Dispersion parameter for binomial family taken to be 1)
#    Null deviance: 661.20 on 481 degrees of freedom
#Residual deviance: 643.13 on 480 degrees of freedom
#AIC: 647.13
summary(d1$X)
xx = seq(14,19.27,length=200)
plot(Y~X,d1,pch=19,cex=0.5)
newval=data.frame(X=xx)
yy = predict(fit,newval,type="response")
lines(xx,yy)
grid()

# loess fit
# LOWESS (locally weighted scatterplot smoothing) loess是lowess的general形式
loess = loess(Y~X,d1)
yl = predict(loess, data.frame(X=xx))
lines(xx,yl,lty=2,col="red")

# second order logistic model
#==================================================================
fit5 = glm(Y~poly(X,2),binomial,d1)
summary(fit5)
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.4064     0.1088  -3.734 0.000188 ***
#  poly(X, 2)1  14.2622     2.9099   4.901 9.52e-07 ***
#  poly(X, 2)2 -21.1088     3.4401  -6.136 8.46e-10 ***
#  (Dispersion parameter for binomial family taken to be 1)
#Null deviance: 661.20 on 481 degrees of freedom
#Residual deviance: 588.27 on 479 degrees of freedom
#AIC: 594.27

# plot
xx = seq(14,19.27,length=200)
plot(Y~X,d1,pch=19,cex=0.5)
newval=data.frame(X=xx,X2=xx^2)
yy = predict(fit5,newval,type="response")
lines(xx,yy)
grid()
#加了二次项居然就成了loess的形状

# third order logistic model
#==================================================================
fit6 = glm(Y~poly(X,3),binomial,d1)
summary(fit6)
xx = seq(14,19.27,length=200)
plot(Y~X,d1,pch=19,cex=0.5)
newval=data.frame(X=xx,X2=xx^2,X3=xx^3)
yy = predict(fit6,newval,type="response")
lines(xx,yy)
grid()
lines(xx,yl,lty=2,col="red") #loess

# k-Fold Cross-Validation
#==========================================================
# Leave 10-out models
library(boot) #for cv.glm
set.seed(17)
mspe=rep(0,5) # initialize vector
for (i in 1:5)
{
  models=glm(Y~poly(X,i),binomial,d1)
  mspe[i]=cv.glm(d1,models,K=10)$delta[1]
}
mspe
# 0.2399499 0.2142489 0.2157637 0.2170965 0.2153225

