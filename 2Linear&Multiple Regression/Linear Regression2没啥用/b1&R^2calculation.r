# sat.r

library(PASWR2)
setwd("C:/Users/USC Guest/Downloads")  # save eps files

d0 = GRADES #就在PASWR2包里
head(d0)
#   sat  gpa
#1 1410 3.67
#2 1100 2.43
#3 1130 1.95
#4  980 2.05
#5 1070 2.34
#6 1030 2.04

dim(d0)
gpa = d0$gpa
sat=d0$sat

# a) scatterplot
plot(gpa~sat,d0,pch=19,cex=0.6)
grid()
# there is a regression relation between gpa and sat

# b) model
m1 = lm(gpa~sat,d0)

# c) verify assumptions
checking.plots(m1)

# d) fitted equation
coef(m1)
# (Intercept)         sat 
# -1.19206381  0.00309427 

# gpa = -1.19206381 + 0.00309427 sat

# gpa increases 0.003094 when sat increases by one

# e) scatterplot and fitted line

plot(gpa~sat,d0,pch=19,cex=0.6)
abline(m1,col="red",lwd=2)
grid()
identify(d0$gpa~d0$sat,cex=0.8)

# f) predict gpa when sat is 1400

newval=data.frame(sat=1400)
predict(m1,newval)
#       1 
#3.139915 

predict(m1,newval,interval="conf",level=0.96)
#        fit    lwr      upr
# 1 3.139915 3.0183 3.261529

# g) CI on parameters 
confint(m1,level=0.99)
#                    0.5 %       99.5 %
# (Intercept) -1.770631656 -0.613495968
# sat          0.002588489  0.003600052

# h) adequacy values 
summary(m1) 

# Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.1920638  0.2224502  -5.359 2.32e-07 ***
# sat          0.0030943  0.0001945  15.912  < 2e-16 ***

# Residual standard error: 0.3994 on 198 degrees of freedom
# Multiple R-squared:  0.5612,    Adjusted R-squared:  0.5589 
# F-statistic: 253.2 on 1 and 198 DF,  p-value: < 2.2e-16

# Model explains 56.12% of sat variability
# Average distance around fitted line is 0.3994
 
# i) verify slope
sxy = cov(gpa,sat)
b1 = sxy / var(sat)            # 0.00309427
# b1=∑(Xi-X)(Yi-Y)/∑(Xi-X)2

# verify R-squared
r = sxy / sd(sat) / sd(gpa)    # [1] 0.7491015
r2 = r*r
r2                             # [1] 0.5611531
# r=∑(Xi-X)(Yi-Y)/根号[∑(Xi-X)2×∑(Yi-Y)2]