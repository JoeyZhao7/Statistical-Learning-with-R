# auto2.r

library(ISLR)   # Auto dataframe

# validation approach
# ===================================================

#train=sample(x=1:10,size=6)

set.seed(1)
train = sample(392,size=196)
train
m1 = lm(mpg~horsepower,Auto,subset=train)

attach(Auto)
res = (mpg-predict(m1,Auto))[-train]^2
head(res)
mspe1 = mean(res)    #  26.14142

# nonlinear

m2 = lm(mpg~poly(horsepower,2),Auto,subset=train)
m2
res = (mpg-predict(m2,Auto))[-train]^2
head(res)
mspe2 = mean(res)    #  19.82259

m3 = lm(mpg~poly(horsepower,3),Auto,subset=train)
res = (mpg-predict(m3,Auto))[-train]^2
head(res)
mspe3 = mean(res)    #   19.78252
mspe3

# Leave-one-out Cross validation
# ======================================================
library(boot)  # cv.glm()

# glm() with no family argument is same as lm()
m1 = lm(mpg~horsepower,Auto)
coef(m1)

#glm1 = glm(mpg~horsepower,Auto) 
#coef(m1)

coef(m1)
# (Intercept)  horsepower
#  39.9358610  -0.1578447
glm1 = glm(mpg~horsepower,data=Auto)
coef(glm1)
#(Intercept)  horsepower
# 39.9358610  -0.1578447

# MSPE from glm1
cverr=cv.glm(Auto,glm1)
summary(cverr)
cverr$delta    # MSPE or CV from LOOCV
# 24.23114


# MSPE for polynomial fittings
cverror = rep(0,5)  # vector to store mspes
for (i in 1:5)
{
  models = glm(mpg~poly(horsepower,i),data=Auto)
  cverror[i] = cv.glm(Auto,models)$delta[1]
}

cverror
# [1] 24.23151 19.24821 19.33498 19.42443 19.03321


# k-fold Cross validation
# =====================================================

set.seed(1)
cverrors =rep(0,10)     # initialize the vector

for(i in 1:10)
{
  models = glm(mpg~poly(horsepower,i),data=Auto)
  cverrors[i]=cv.glm(Auto,models,K=10)$delta[1]
}

cverrors
# [1] 24.10716 19.24186 19.29106 19.45578 19.25644 18.86037 18.84461 18.77549
# [9] 19.66007 19.54238



















 
 
 
 
 
 
 


















