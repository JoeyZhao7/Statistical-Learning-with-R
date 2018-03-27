# HSWRESTLER.r

library(PASWR2)   # dataset
library(leaps)    # regsubsets()

d0 = HSWRESTLER
head(d0)
ig = c(22,27,32,35,60)
d1 = d0[-ig,1:7]
head(d1)

n = nrow(d1)
k = 5   # folds
set.seed(5)
folds = sample(k,n,replace=T)
folds
head(folds)
# [1] 2 4 5 2 1 4
table(folds)
# folds
# 1  2  3  4  5
# 13 17 13  8 22
length(folds)
# [1] 73

mspe = matrix(0,k,6)   # 5-by-6 matrix
mspe
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]    0    0    0    0    0    0
# [2,]    0    0    0    0    0    0
# [3,]    0    0    0    0    0    0
# [4,]    0    0    0    0    0    0
# [5,]    0    0    0    0    0    0

b=d1[folds==1,]
dim(b)
# [1] 13  7
b
#   age     ht    wt abs triceps subscap hwfat
# 5   17 69.500 299.2  54      42    37.0 41.89
# 10  14 70.500 175.4  19      13    11.5 13.08
# 26  15 63.375 123.2  10      10     8.0  7.97
# 31  15 68.750 201.4  37      27    31.0 31.71
# 36  15 63.250 152.6  21      13     9.0 17.83
# 38  14 67.250 124.2  10      10     8.0 13.87
# 39  16 69.000 209.8  41      35    36.0 33.53
# 52  16 69.500 142.2   6       7     7.0  9.91
# 64  17 68.000 162.2  10       8    11.0  7.17
# 65  16 71.000 142.2  15      11    10.0 11.40
# 67  17 71.000 166.0  12       8     7.0 11.27
# 73  17 65.000 143.6  12      10     7.0 10.26
# 78  15 66.000 258.6  45      37    43.0 33.75

y=d1$hwfat[folds==1]
y
#  [1] 41.89 13.08  7.97 31.71 17.83 13.87 33.53  9.91  7.17 11.40 11.27 10.26 33.75

for(j in 1:k)
{
  y=d1$hwfat[folds==j]   # y-values in j-th fold
  d2 = d1[folds !=j,]    # training set ignores j-th fold
  cvmodels = regsubsets(hwfat~.,d2)
  for(i in 1:6)
  {
    newdata = d1[folds==j,]   # test set
    yhat = predict.regsubsets(cvmodels,newdata,id=i)
    mspe[j,i] = mean((y-yhat)^2)
  }
}

mspe
          [,1]      [,2]      [,3]      [,4]      [,5]      [,6]
[1,] 10.208869  5.196952  5.435172  5.998849  5.996626  5.995230
[2,] 14.080017 10.429799 10.516776 11.001260 12.098991 12.058037
[3,] 12.731267  9.748415 11.802072 13.262619 13.689513 13.647675
[4,] 28.058776 12.797397 12.436328 12.913590 13.070446 12.898214
[5,]  6.409412  6.237271  6.122226  6.066597  6.168323  6.154457

CVk = apply(mspe,2,mean)
[1] 14.297668  8.881967  9.262515  9.848583 10.204780 10.150723

> models = regsubsets(hwfat~.,d1)
> aux = which.min(CVk)
> aux
[1] 2
coef(models,aux)
(Intercept)         abs     triceps
  1.9119410   0.3929936   0.4211225

