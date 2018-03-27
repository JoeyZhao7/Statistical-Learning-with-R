#QUESTION1 mean=-1.25 var=0.36
p1=pnorm(-1.9, mean = -1.25, sd =0.6 )
p2=1-p1
p2
qnorm(0.25, mean = -1.25, sd = 0.6)

#QUESTIONS2 F&GM
df=read.csv("F.csv")
dgm=read.csv("GM.csv")
str(df)
dfacls=df$Adj.Close
dgmacls=dgm$Adj.Close
cor(dfacls,dgmacls)

n=length(dfacls)
rf=dfacls[2:n]/dfacls[1:(n-1)]-1
rgm=dgmacls[2:n]/dgmacls[1:(n-1)]-1
cor(rf,rgm)

library("moments")
help(kurtosis)
kurtosis(rgm)

dfrm=data.frame(rf,rgm)
plot(dfrm)
rgrs=lm(rgm~rf)
abline(rgrs)
summary(rgrs)

#QUESTIONS3 
install.packages("PASWR2")
library("PASWR2")
df1=VIT2005
head(df1)
df2=subset(df1,,subset= totalprice>400000 & garage==1)
dim(df2)

h1=hist(df1$totalprice)
tp=df1$totalprice
str(h1)
h1$counts=h1$counts/sum(h1$counts)
plot(h1,ylab="relative frequencies") #这个图叫相对平率分布直方图

hh=df1$totalprice
hist(hh,freq = F)
curve(dnorm(x, mean = mean(hh), sd = sd(hh)), min(hh), max(hh), add = TRUE)

str(df1)
num=data.frame(df1$totalprice,df1$area,df1$age,df1$floor,df1$rooms,df1$toilets,df1$garage,df1$elevator,df1$storage)
cormtx=cor(num)
covmtx=cov(num)
options(digits=1)
cormtx
covmtx
write.csv(covmtx, file="D:/covariance matrix.csv")
write.csv(cormtx, file="D:/correlation matrix.csv")


plot(df1$area,df1$totalprice)
rgs1=lm(df1$totalprice~df1$area)
abline(rgs1)
summary(rgs1)
est=abs(df1$totalprice-(40822.4+2704.8*df1$area))
df2=data.frame(df1$totalprice,df1$area,est)
index=order(df2$est)
d3=df2[index,]
d3