# cars.r
predict.regsubsets <- function(object, newdata, id, ...)
{
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi
}

library(MASS)
d0=Cars93

d1 = subset(d0,select=c(MPG.city,Cylinders,EngineSize,Horsepower,RPM,Passengers,Weight))
d1$Cylinders=as.numeric(d1$Cylinders)

library(leaps) #regsubsets

models=regsubsets(MPG.city~.,d1)
a=summary(models)
a$adjr2

# predict()
m0 = lm(MPG.city~EngineSize+Horsepower+RPM+Weight,d1)
newval=data.frame(Cylinders=4,EngineSize=2.3,Horsepower=200,RPM=5500,Passengers=4,Weight=2950)
predict(m0,newval)
# 21.06858

# predict.regsubsets()
newval=data.frame(MPG.city=0,Cylinders=4,EngineSize=2.3,Horsepower=200,RPM=5500,Passengers=4,Weight=2950)
predict.regsubsets(models,newval,id=4)
#         [,1]
#[1,] 21.06858

# Split data into training and test sets

n = nrow(d1)
n/2
ceiling(n/2)

set.seed(12)
train = sample(1:n,47)   # training row numbers
d1train = d1[train,]
d1test = d1[-train,]

# fit regsubsets
models = regsubsets(MPG.city~.,d1train)

# MSPE for all models found by regsubsets()

mspe = rep(0,6)
y = d1test$MPG.city

for(i in 1:6)
{
  yhat = predict.regsubsets(models,d1test,id=i)
  mspe[i] = mean((y-yhat)^2)
}

mspe
# [1] 9.122425 9.695262 9.819361 9.389211 9.608725 9.736857






