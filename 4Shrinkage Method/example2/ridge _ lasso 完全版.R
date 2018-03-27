# ridge & lasso regression

library(ISLR) # data base
dim(Hitters)
# [1] 322 20
sum(is.na(Hitters$Salary)) # missing 59 salaries is NA(NOT AVAILIABLE)
# [1] 59
d0=na.omit(Hitters)
dim(d0)
# [1] 263 20
n = nrow(d0)
x=model.matrix(Salary~.,d0)[,-1] # Salary is the only num var
y=d0$Salary
head(d0)
head(x)
# str(x) does not work, since x is a matrix

# Ridge Regression
#===========================================================
library(glmnet)

# lambdas from 10^10 to 10^{-2}
a = seq(from=10,to=-2,length=100)
head(a)
# [1] 10.000000 9.878788 9.757576 9.636364 9.515152 9.393939
tail(a)
# [1] -1.393939 -1.515152 -1.636364 -1.757576 -1.878788 -2.000000
grid=10^a

# 100 ridge regressions (one for each value of lambda)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
# ridge.mod$lambda = grid
head(grid)
# [1] 10000000000 7564633276 5722367659 4328761281 3274549163 2477076356
tail(grid)
# [1] 0.04037017 0.03053856 0.02310130 0.01747528 0.01321941 0.01000000
head(ridge.mod$lambda)
# [1] 10000000000 7564633276 5722367659 4328761281 3274549163 2477076356
tail(ridge.mod$lambda)
# [1] 0.04037017 0.03053856 0.02310130 0.01747528 0.01321941 0.01000000
# coef matrix
dim(coef(ridge.mod))
# [1] 20 100

# plot lambdas
plot(grid)
plot(grid,ylim=c(0,20000))
grid()

# compare reg coef for two lambdas
ridge.mod$lambda[50]
# [1] 11497.57
ridge.mod$lambda[60]
# [1] 705.4802
options(digits=4)
coef(ridge.mod)[,50] #这样也可以看模型的参数
# (Intercept) AtBat Hits HmRun Runs RBI Walks Years
# 407.356050 0.036957 0.138180 0.524630 0.230702 0.239841 0.289619 1.107703
# CRBI CWalks LeagueN DivisionW PutOuts Assists Errors NewLeagueN
# 0.024138 0.025015 0.085028 -6.215441 0.016483 0.002613 -0.020503 0.301434
coef(ridge.mod)[,60]
# (Intercept) AtBat Hits HmRun Runs RBI Walks Years
# 54.32520 0.11211 0.65622 1.17981 0.93770 0.84719 1.31988 2.59640
# CRBI CWalks LeagueN DivisionW PutOuts Assists Errors NewLeagueN
# 0.09780 0.07190 13.68370 -54.65878 0.11852 0.01606 -0.70359 8.61181
# smaller lambda, larger coefficients (exclude intercept)
# l_2 norms
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # [1] 6.361
sqrt(sum(coef(ridge.mod)[-1,60]^2)) # [1] 57.11
# smaller lambda, larger l_2 norm
# new ridge regression for new lambda = 50
options(digits=9)
predict(ridge.mod,s=50,type="coefficients")

# coef plots
#===============================================================
plot(ridge.mod,xvar="lambda")
grid()
# each curve is a regression coef
# left extreme is OLS regression coefs
options(digits=9)
predict(ridge.mod,s=0,type="coefficients")
# (Intercept) 299.4446721950
# AtBat -2.5353835506
# Hits 8.3358501910
# HmRun 11.5983081539
# Runs -9.0597137055
# RBI 2.4532654580
# Walks 9.2177600598
# Years -22.9823958271
# CAtBat -0.1819165075
# CHits -0.1056568836
# CHmRun -1.3172135755
# CRuns 3.3115251855
# CRBI 0.0659068925
# CWalks -1.0724447665
# LeagueN 59.7558727256
# DivisionW -98.9439300481
# PutOuts 0.3408327575
# Assists 0.3415544534
# Errors -0.6531247129
# NewLeagueN -0.6588292982

# as lambda increase coefficients shrink to zero

# MSPE
#=============================================================
set.seed(1)
train=sample(1:n, n/2)
test=(-train)
y.test=y[test]

# MSPE for lambda=4
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict.glmnet(ridge.mod,s=4,x[test,]) # x must be matrix
mean((ridge.pred-y.test)^2)
# 101036.833

# MSPE for lambda=10^10
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
# 193253.057

# MSPE for lambda=0 (this is OLS)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2)
# 114723.615

# Cross validation to select best lambda
#==================================================================
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
cv.out$lambda.min # 211.741585 best lambda

# MSPE for best lambda
bestlam=cv.out$lambda.min
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
# 96015.5127
# better than MSPE with lambda = 4

# refit with full dataset
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
# (Intercept) AtBat Hits HmRun
# 9.8848715652 0.0314399123 1.0088287507 0.1392762360
# Runs RBI Walks Years
# 1.1132078099 0.8731899006 1.8041022920 0.1307438111
# CAtBat CHits CHmRun CRuns
# 0.0111397798 0.0648984332 0.4515854621 0.1290004905
# CRBI CWalks LeagueN DivisionW
# 0.1373771163 0.0290857160 27.1822753486 -91.6341129943
# PutOuts Assists Errors NewLeagueN
# 0.1914925199 0.0425453624 -1.8124447027 7.2120838997

# ridge model requires all predictors

# Lasso Regression (alpha=1)
#===========================================================
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
# coef plot
plot(lasso.mod,xvar="lambda"); grid()
# increasing lambda decreases coefs, some zero
# 10-fold cross validation to select best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,nfolds=10)
cv.out$lambda.min 
# 16.7801585 best lambda

# MSPE for best lambda
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
# 100743.446
# close to MSPE of ridge regression 96016

# refit with full dataset
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
# (Intercept) AtBat Hits HmRun
# 18.539484370 0.000000000 1.873538979 0.000000000
# Runs RBI Walks Years
# 0.000000000 0.000000000 2.217844394 0.000000000
# CAtBat CHits CHmRun CRuns
# 0.000000000 0.000000000 0.000000000 0.207125173
# CRBI CWalks LeagueN DivisionW
# 0.413013209 0.000000000 3.266667729 -103.484545814
# PutOuts Assists Errors NewLeagueN
# 0.220428413 0.000000000 0.000000000 0.000000000

# 12 coefs are zero

# predictors in lasso model
lasso.coef[lasso.coef!=0]
# (Intercept) Hits Walks CRuns
# 18.539484370 1.873538979 2.217844394 0.207125173
# CRBI LeagueN DivisionW PutOuts
# 0.413013209 3.266667729 -103.484545814 0.220428413
# 很具体说明了lasso regression能筛选variables
