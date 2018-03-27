#ISE529 mindterm eaxm
#Name: Zhenyu Zhao 
#USCID: 1657334680

#a download and report
df=F_
dspy=SPY
head(df)
head(dspy)
tail(df)
tail(dspy)

#b find daily returns
Fprice=df$`Adj Close`
SPYprice=dspy$`Adj Close`
n=length(Fprice) # also the rows of SPY
Fret=Fprice[2:n]/Fprice[1:(n-1)]
SPYret=SPYprice[2:n]/SPYprice[1:(n-1)]
head(Fret)
tail(Fret)
head(SPYret)
tail(SPYret)

#C regression
d1=data.frame(SPYret,Fret)
m1=lm(Fret~SPYret)
m1
summary(m1)

#d plot
plot(Fret~SPYret,d1,pch=19,cex=0.6,xlim=c(0.95,1.05),ylim=c(0.95,1.05),col="blue")
abline(m1,lty=2)

#e outlier
which.max(residuals(m1))
identify(Fret~SPYret)
text(Fret~SPYret,d1,labels=ifelse(rownames(d1)==483,rownames(d1),""))

#question 2

#a regression
library(MASS)
d0=insurance
head(d0)
m1=lm(Longevity~.,d0)
m2=stepAIC(m1)
new1=data.frame(Mother=75,Father=65,Smoker=1)
predict(m2,new1,interval="conf")

#b CV
n=nrow(d0)
n/2
set.seed(2)
train=sample(1:n,50) 
m1=lm(Longevity~.,d0[train,])
m2=lm(Longevity~Mother+Father+Smoker,d0[train,])
y=d0$Longevity[-train]
yhat1=predict(m1,d0[-train,])
yhat2=predict(m2,d0[-train,])
MSPE1= mean((y-yhat1)^2)
MSPE2= mean((y-yhat2)^2)
sqrt(MSPE1)
sqrt(MSPE2)

#question 3
#a correlation
dim(prices)
C=cor(prices)
View(C)
dim(C)
which(C==max(C) , arr.ind = T)
for(i in 1:452)
  C[i,i]=0 
which(C==max(C),arr.ind = T)
C[428,205]
names[428,]
names[205,]

#b plot
x=prices[,205]
y=prices[,428]
x=as.list(x)
y=as.list(y)
newd=data.frame(y,x)
newd
plot(newd,pch=19,cex=0.6,col="blue")

#c subset
d1=prices
d2=names
dim(d2)
d2=cbind(d2,c(1:452))
d2
d2$Sector
table(d2$Sector)
#d
d2f=subset(d2,d2$Sector=="Financials")
d2f$Ticker
rows=d2f$`c(1:452)`
rows
colnames(d1)
d1f=d1[,rows]
dim(d1f)
Cf=cor(d1f)
dim(Cf)
which(Cf==max(Cf) , arr.ind = T)
for(i in 1:74)
  Cf[i,i]=0 
which(Cf==max(Cf),arr.ind = T)
d2f[71,]
d2f[32,]
