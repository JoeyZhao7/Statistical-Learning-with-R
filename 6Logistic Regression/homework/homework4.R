#A logistic regression model is fitted. It is found that b0 = ???25 and b1 = 0.20.
#a) (10 pts.) Write the fitted equation.
#f(x)=exp(-25+0.2*x)/(1+exp(-25+0.2*x))
b0=-25
b1=0.20

#b) (10 pts.) For what value of X is the mean response equal to 0.50?
fx=function(x,fx) fx=exp(b0+b1*x)/(1+exp(b0+b1*x))
fxx=function(x,fxx) fxx=exp(b0+b1*x)/(1+exp(b0+b1*x))-0.5
curve(fx,xlim = c(-10,200))
uniroot(fxx,c(-10,200))
curve(fxx,xlim = c(120,130))
#the value of X is 125

#c) (10 pts.) Find the ratio of the odds when X = 151 to that when X = 150.
exp(0.2)
#1.221403 

#a) (10 pts.) Fit separate simple logistic regression models for predicting Y against each
#individual predictor. Write the fitted equation for each one.
d0=read.table("flushots.txt",header = T)
str(d0)
head(d0)
mage=glm(shot~age,binomial,d0)
summary(mage)
#shot = exp(-8.74326+0.10874*age)/(1+exp(-8.74326+0.10874*age))
mindex=glm(shot~index,binomial,d0)
summary(mindex)
#shot = exp(4.9113-0.11931*index)/(1+exp(4.9113-0.11931*index))
mgender=glm(shot~gender,binomial,d0)
summary(mgender)
#shot = exp(-2.0794+0.6444*gender)/(1+exp(-2.0794+0.6444*gender))

#b) (10 pts.) For each model find the probability that male clients aged 55 with a health
#awareness index of 60 will receive a flu shot.

newval = data.frame(age=55,index=60,gender=1)
newval
predict(mage,newval,type="response")
y=predict(mage,newval,type="link")
1/(1+exp(-y)) #link «b0+b1x1+b2x2+...
#0.05937092 
predict(mindex,newval,type="response")
#0.09558884 
predict(mgender,newval,type="response")
#0.1923077

#c) (10 pts.) Fit a multiple logistic regression model with predictors age and health index.
#Create a bubbleplot to show the probability that the client receives a flu shot.
model1 = glm(shot~age+index,binomial,d0)
summary(model1)
prob = predict(model1, type = "response")
plot(age~index,d0,xlim=c(15,80),ylim=c(45,95),pch = ".")
symbols(d0$index,d0$age,circles = prob,add = T)
grid()

#d) (10 pts.) Plot fitted equation and scatterplot for model with predictor age. Add a loess
#curve. Comment.
plot(shot~age,d0,pch=19,cex=0.5)
xx = seq(45,85,length=200)
newval2 = data.frame(age=xx)
yy = predict(mage,newval2,type="response")
lines(xx,yy)
grid()
loess1 = loess(shot~age,d0)
yl = predict(loess1, data.frame(age=xx))
lines(xx,yl,lty=2,col="red")
#comment: the losse smothing curve is similiar with logistic regression line,
#the function above can be used as predict function, if the error rate is acceptable

#e) (10 pts.) Plot fitted equation and scatterplot for model with predictor health index. Add
#a loess curve. Comment.
plot(shot~index,d0,pch=19,cex=0.5)
xx = seq(20,85,length=200)
newval3 = data.frame(index=xx)
yy = predict(mindex,newval3,type="response")
lines(xx,yy)
grid()
loess2 = loess(shot~index,d0)
yl = predict(loess2, data.frame(index=xx))
lines(xx,yl,lty=2,col="red")
#comment: the losse smothing curve is similiar with logistic regression line,
#the function above can be used as predict function, if the error rate is acceptable

#f) (20 pts.) Plot fitted equation and scatterplot for model with predictor health index. Add
#95% CI bands above and below the fitted equation.
plot(shot~index,d0,pch=19,cex=0.5)
xx = seq(20,85,length=200)
newval3 = data.frame(index=xx)
yy = predict(mindex,newval3,type="response")
lines(xx,yy)
grid()
alpha = 0.05
yhat = predict(mindex, newval3, se.fit=T, type="response")
lower95 = yhat$fit - qnorm(1-alpha/2)*yhat$se.fit
upper95 = yhat$fit + qnorm(1-alpha/2)*yhat$se.fit
lines(xx,lower95,col= "red" )
lines(xx,upper95, col="blue")
