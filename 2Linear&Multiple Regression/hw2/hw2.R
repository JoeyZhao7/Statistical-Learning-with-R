#question 1
library(readxl)
homes <- read_excel("C:/Users/Joey Zhao/Desktop/529/2/homes.xls")
View(homes)
data1=subset(homes,select = c(price, lotsize, area, beds, baths, year,garage))
cor(data1)

#question 2
which.max(data1$price)
data1[73,]

#question 3
m1=lm(price~.,data=data1)
m1
library(PASWR2)
checking.plots(m1)
help(predict) # also can use function residual()
pre1=predict(m1,data1)
res1=(data1$price-pre1)^2
which.max(res1)
data1[73,]

#question 4
#the lm model is price=-3.568e+06 + 1.555e+00 lotsize + 1.257e+02 area + -1.304e+04 beds + 7.988e+03 baths + 1.780e+03 year + 2.253e+04 garage
#beta 2 is 1.257e+02 which is the coefficient of area
summary(m1)
help("confint")
confint(m1,3,0.99)

#question 5
#Find a 95% confidence interval for the mean price of a house with garage for two cars, area of 2650
#square feet, built in 1990, 24500 square feet size, three bedrooms, three bathrooms
new1=data.frame(lotsize=24500,area=2650,beds=3,baths=3,year=1990,garage=2)
predict(m1,new1,interval = "conf" , level = 0.95)

#question 6
help(median)
new2=data.frame(lotsize=median(data1$lotsize),area=median(data1$area),beds=median(data1$beds),baths=median(data1$baths),year=median(data1$year),garage=median(data1$garage))
predict(m1,new2)
install.packages("leaps")
library(leaps)
models=regsubsets(price~.,data1)
models
a=summary(models)
a$adjr2
which.max(a$adjr2)

#question 7
cor(data1)
data2=subset(data1,select = c(price,beds))
data2
m2=lm(price~beds,data2)
m2

#question 8
#b1, which is the coefficient of beds, means when bed increases by 1,then theoretically the price of a residential house will increase 56200 units.
data3=subset(data1,beds>=2 & beds<=4)
#full model means with six predictors
m3=lm(price~.,data3)
m3

#question 9
summary(m3)

#question 10
#in this case, we choose to use model m3, because it has higer R
new3=data.frame(lotsize=26250,area=3150,beds=2,baths=3,year=1996,garage=2)
predict(m3,new3,interval = "prediction", level = 0.95)
