# loop4folds.r

for(j in 1:k)   # loop folds
{
  y = d1$Salary[folds == j]     # y-values in test set
  d2 = d1[folds != j,]          # training set
  best.fit <- regsubsets(Salary ~.,d2,nvmax=19)
  for(i in 1:19)                # i number of predictors in model
  {
    newdata = d1[folds ==j,]   # test set
    yhat <- predict.regsubsets(best.fit,newdata,id=i)
    mspe[j, i] <- mean((y - yhat)^2)
  }
}

