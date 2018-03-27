# cars93.r

library(MASS) #dataframe Cars93
library(PASWR2) #for residuals analystics/residual plots
d0=Cars93
str(d0)

# scatterplot on mileages

plot(MPG.highway~MPG.city,d0)
grid()
identify(d0$MPG.highway~d0$MPG.city) #为了在图上点数据

plot(MPG.highway~MPG.city,d0,xlim=c(10,55),ylim=c(10,55))
grid()
identify(d0$MPG.highway~d0$MPG.city)
d0[c(39,41,17),]

# scatterplots Price mpg
plot(Price~MPG.city,d0,pch=19,cex=0.6,xlim=c(5,55),ylim=c(0,70),col="red",xlab="")
par(new=TRUE)        # to overlap plots
plot(Price~MPG.highway,d0,pch=19,cex=0.6,xlim=c(5,55),ylim=c(0,70),col="blue",xlab="mileage")
grid()

# fit models
m1=lm(Price~MPG.city,d0)
m2=lm(Price~MPG.highway,d0)

# fitted lines
abline(m1,lty=2,col="red")
abline(m2,lty=2,col="blue")
name2 = c("city","highway")
legend("topright",name2,lty=c(pch=10,pch=19),col=c("blue","red"))

# residuals
#================================================================
checking.plots(m1)
checking.plots(m2)
which.max(residuals(m1))
which.max(residuals(m1))
# Commment about regression assumptions using residual plots
# In Normal Q-Q, residuals are aligned (excluding outliers), normality holds

# In residuals vs fitted values (excluding outliers)
# variability is not constant. Constant variance may not hold.

# Residuals histogram slightly asymmetric

# outliers (48,59)
#================================================================
# to display Manufacturer, Model, Price, MPG.city, MPG.highway, Origin
d0b=d0[,c(1,2,5,7,8,26)]
d0b[c(48,59),]
#    Manufacturer Model Price MPG.city MPG.highway  Origin
# 48     Infiniti   Q45  47.9       17          22 non-USA
#59 Mercedes-Benz  300E  61.9       19          25 non-USA


# test the significance of the regression
#==================================================================
anova(m1)
qqnorm
# Analysis of Variance Table
#           Df Sum Sq Mean Sq F value    Pr(>F)    
# MPG.city   1 3034.5 3034.49  49.759 3.308e-10 ***
# Residuals 91 5549.5   60.98                      

# reject Ho: Beta_1 = 0 since pvalue is 3.308e-10
# therefore MPG.city should be in the model to predict Price
# average sq distance to the fitted line is MSE = 60.98 

anova(m2)
# Analysis of Variance Table
#             Df Sum Sq Mean Sq F value    Pr(>F)    
# MPG.highway  1 2698.5 2698.49  41.723 5.072e-09 ***
# Residuals   91 5885.5   64.68                   

# average sq distance to the fitted line is MSE = 64.68    
# reject Ho: Beta_1=0 for both models

# fitted equation
#===================================================================
summary(m1)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  42.3661     3.3399  12.685  < 2e-16 ***
# MPG.city     -1.0219     0.1449  -7.054 3.31e-10 ***

# Residual standard error: 7.809 on 91 degrees of freedom 叫做RSE/S 
# 回归的标准误差 RSE^2=MSE(Mean Square Errors)
# Residual Sum Squares/ Sum Square Residuals = MSE*(n-m) 叫做RSS/SSR/SS(Excel)
# Multiple R-squared:  0.3535,    Adjusted R-squared:  0.3464
# F-statistic: 49.76 on 1 and 91 DF,  p-value: 3.308e-10

# Estimated Price = 42.3661 - 1.0219 MPG.city

# Average price decreases 1021.9 dollars per additional city miles per gallon

summary(m2)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  49.0546     4.6494  10.551  < 2e-16 ***
# MPG.highway  -1.0158     0.1573  -6.459 5.07e-09 ***

# Residual standard error: 8.042 on 91 degrees of freedom
# Multiple R-squared:  0.3144,    Adjusted R-squared:  0.3068
# F-statistic: 41.72 on 1 and 91 DF,  p-value: 5.072e-09

# Estimated Price = 49.0546 - 1.0158 MPG.highway

# Average price decreases 1015.8 dollars per additional highway miles per gallon

# model adequacy
#==================================================================
# model m1
# R-squared = 0.3535
# standard error of the estimate = 7.809

# model explains 35.35% of the price variability
# std deviation of prices around fitted line is 7809 USD

# model m2
# R-squared = 0.3144
# standard error of the estimate = 8.042

# model explains 31.44% of the price variability
# std deviation of prices around fitted line is 8042 USD

# m1 has larger R-squared, thus fits best


# Confidence Intervals
#==================================================================

newval = data.frame(MPG.city = 27.5)
predict(m1,newval,interval="conf")
#       fit      lwr      upr
# 1 14.2626 12.07845 16.44675

# Mean price of a 27.5 city mileage car is in 14262.6 USD
# Mean price of a 27.5 city mileage car is in (12078.45, 16446.75)

newval2 = data.frame(MPG.highway = 27.5)
predict(m2,newval2,interval="conf")
#        fit      lwr      upr
# 1 21.12072 19.39172 22.84973

# Mean price of a 27.5 highway mileage car is 21120.72 USD
# Mean price of a 27.5 highway mileage car is in (19391.72, 22849.73)

# Prediction Intervals
#-------------------------------------------------------------
predict(m1,newval,interval="pred")
#       fit       lwr      upr
# 1 14.2626 -1.402466 29.92767

# Price of a new car that provides 27.5 city mileage is 14262.2 USD
# Price of a new car that provides 27.5 city mileage is in (0, 29927.67)

predict(m2,newval2,interval="pred")
#        fit      lwr      upr
# 1 21.12072 5.052692 37.18876

# Price of a new car that provides 27.5 highway mileage is 21120.72 USD
# Price of a new car that provides 27.5 highway mileage is in (50526.92, 37.18876)

# for 90% interval use
predict(m2,newval2,interval="pred",level=0.90)

