
# college 

library(ISLR)
library(glmnet) # Lasso and Ridge regression

# a) training and test sets
set.seed(0)
n = nrow(College)
train=sample(1:n, n/2)
test = (-train)
dtrain = College[train,]
dtest = College[test,]

# (b) linear model
m1 = lm( Apps ~ ., dtrain )
Y_hat = predict(m1,dtest )
mspe = mean((dtest$Apps - Y_hat)^2) # 1592006.87

# (c) ridge regression
Y = dtrain$Apps
newx=model.matrix(Apps~.,dtest)
head(newx) #第一列全是1
newx=model.matrix(Apps~.,dtest)[,-1]
MM = model.matrix(Apps ~ ., data=dtrain)[,-1]
head(dtrain)
head(MM)

set.seed(0)
cv.out = cv.glmnet(MM,Y, alpha=0) 
#cross validation choose bestlam 
#alpha=0 ridge

bestlam = cv.out$lambda.min
bestlam 
# 415.279563

ridge.mod = glmnet(MM,Y,alpha=0)
Y_hat = predict(ridge.mod,s=bestlam,newx)
mspe2 =mean((dtest$Apps-Y_hat)^2) # 1347349.36

# (d) lasso regression
set.seed(0)
cv.out = cv.glmnet(MM,Y,alpha=1)
bestlam = cv.out$lambda.min
bestlam 
# 3.21536

lasso.mod = glmnet(MM,Y,alpha=1)
Y_hat = predict(lasso.mod,s=bestlam,newx)
mspe3 =mean((dtest$Apps-Y_hat)^2)
mspe3 
# 1585550.63
sprintf("Lasso regression test MSE= %10.f",mspe3,"\n")

# refit with full data set
Y = College$Apps
MM = model.matrix(Apps~.,data=College)[,-1]
out=glmnet(MM,Y,alpha=1)
options(digits=1)
lasso.coef=predict(out,type="coefficients",s=bestlam)
# find coefficients of lasso model
lasso.coef
length(lasso.coef)
lasso.coef[lasso.coef!=0] 
#there are coefficients close to 0,but not =0

