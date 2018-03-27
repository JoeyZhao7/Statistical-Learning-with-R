#QUESTION1.1
library(PASWR2)
View(HSWRESTLER)
d0=HSWRESTLER
d1=subset(d0,select=c(hwfat,age,ht,wt,abs,triceps,subscap))
d1
x=model.matrix(hwfat~.,d1)[,-1]
y=d1$hwfat
set.seed(1)
n=nrow(d1)
n/2
train=sample(n,n/2)
m1 = lm(hwfat~.,d1,subset = train)
m1
attach(d1)
res = (hwfat-predict(m1,d1))[-train]^2
mean(res) #mspe 14.0963

#QUESTION1.2
library(glmnet)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0,nfolds=15)
summary(cv.out)
plot(cv.out)
bestlam=cv.out$lambda.min
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=bestlam)
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[-train,])
mean((ridge.pred-y[-train])^2) #mspe 14.51665
bestlam #0.9941985

#QUESTION1.3
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,nfolds=15)
summary(cv.out)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam)
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[-train,])
mean((lasso.pred-y[-train])^2) #mspe 13.77674
bestlam #0.0494815

#QUESTION2.1
set.seed(1)
n=100
x=rnorm(n)
e=0.1*rnorm(n)
y=1-0.1*x+0.05*x^2+0.75*x^3+e
d0 = data.frame(y,x,x2=x^2,x3=x^3,x4=x^4,x5=x^5,x6=x^6,x7=x^7,x8=x^8,x9=x^9,x10=x^10)
models=regsubsets(y~.,d0,nvmax=10)
summary(models)
a=summary(models)
a$adjr2
which.max(a$adjr2)
coef(models,id=5)
# best model with predictors x,x2,x3,x5

#QUESTION2.2
set.seed(1)
train=sample(100,50)
train
cv.out=cv.glmnet(polyx[train,],y[train],alpha=1,nfolds=10)
cv.out$lambda.min
bestlam=cv.out$lambda.min
#[1] the best lambda is 0.07034419
which.min(cv.out$cvm)
cv.out$cvm[37]
#[1] the mean cross-validated error is 0.01832092, which is the measurement of test error
lasso.mod=glmnet(polyx[train,],y[train],alpha=1,lambda=bestlam)
lasso.pred=predict(lasso.mod,s=bestlam,newx=polyx[-train,])
mean((lasso.pred-y[-train])^2) #mspe 0.0176616

#QUESTION3.1
d1=subset(homes,select = c(price,lotsize,area,beds,baths,year,garage))
x=model.matrix(price~.,d1)[,-1]
y=d1$price
set.seed(1)
n=nrow(d1)
n/2
train=sample(n,n/2)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0,nfolds=15)
summary(cv.out)
plot(cv.out)
bestlam=cv.out$lambda.min
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=bestlam)
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[-train,])
mean((ridge.pred-y[-train])^2) #mspe 5279909791
bestlam #12660.14

#QUESTION3.2
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,nfolds=15)
summary(cv.out)
plot(cv.out)
which.min(cv.out$cvm)
cv.out$cvm[58] #[1] 4956460412 test error of 15 folds cross validation
bestlam1=cv.out$lambda.min
bestlam1 #574.1219
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam1)
lasso.pred=predict(lasso.mod,s=bestlam1,newx=x[-train,])
mean((lasso.pred-y[-train])^2) #mspe 5084501006
bestlam1 #574.1219

#QUESTION3.3
new=data.frame(lotsize=median(d1$lotsize),area=median(d1$area),beds=median(d1$beds),baths=median(d1$baths),year=median(d1$year),garage=median(d1$garage))
new1=as.matrix(new)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=bestlam)
predict(ridge.mod,s=bestlam,newx=new1) #[1,] 256784 ridge method
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam1)
predict(lasso.mod,s=bestlam1,newx=new1) #[1,] 251426.4 lasso method
