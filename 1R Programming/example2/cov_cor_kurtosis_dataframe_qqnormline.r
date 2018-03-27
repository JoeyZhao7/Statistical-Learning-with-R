# Aug25b.r
d0 = read.csv("Aug25.csv",header=T)
d0 = Aug25
dim(d0)
# [1] 252   7
n = nrow(d0)
n
# [1] 252
structure(d0)
Fprice = d0$'Adj Close'
Fprice
d2 = data.frame(Fprice[-n],Fprice[-1]) #dataframe构建
head(d2)
#   Fprice..n. Fprice..1.
# 1   11.72315   11.80837
# 2   11.80837   11.88413
# 3   11.88413   11.93148
# 4   11.93148   11.77997
# 5   11.77997   11.83678
# 6   11.83678   11.99776
dim(d2)
# [1] 251   2
head(d0)
#         Date  Open  High   Low Close Adj.Close   Volume
# 1 2016-08-26 12.47 12.55 12.34 12.38  11.72315 22645900
# 2 2016-08-29 12.38 12.50 12.38 12.47  11.80837 22243200
# 3 2016-08-30 12.47 12.56 12.43 12.55  11.88413 26040400
# 4 2016-08-31 12.48 12.61 12.48 12.60  11.93148 26030600
# 5 2016-09-01 12.66 12.72 12.35 12.44  11.77997 40510400
# 6 2016-09-02 12.53 12.57 12.46 12.50  11.83678 21079800
d2 = data.frame(Fprice=Fprice[-n],lag1=Fprice[-1])
head(d2)
#     Fprice     lag1
# 1 11.72315 11.80837
# 2 11.80837 11.88413
# 3 11.88413 11.93148
# 4 11.93148 11.77997
# 5 11.77997 11.83678
# 6 11.83678 11.99776
plot(Fprice~lag1,d2)
plot(Fprice~lag1,d2,pch=19,cex=0.6,xlab="previous day price")
grid()
cor(d2$Fprice,d2$lag1) #相关系数0到1
# [1] 0.9603814
cov(d2$Fprice,d2$lag1) #协方差
# [1] 0.2593886

Fret = Fprice[2:n]/Fprice[1:(n-1)]-1
head(Fret)
# [1]  0.007269890  0.006415447  0.003984053 -0.012698512  0.004823189  0.013600065

qqnorm(Fret) #看某一个数据是不是正态分布
qqline(Fret)
grid()
hist(Fret)

set.seed(12)
series = rnorm(n)
qqnorm(series)
qqline(series)
grid()

install.packages("moments")       
library(moments)
kurtosis(series) #正态分布峰度为3，Kurtosis>0尖顶峰，Kurtosis<0平顶峰
skewness(series) #正态分布偏度为0，Skewness>0右偏，Skewness<0左偏
# [1] 2.722017
kurtosis(Fret)
skewness(Fret)
# [1] 5.147586

