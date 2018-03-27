# br.r

n = 250
mx = 70
sdx = 3
my = 162
sdy = 14
rho = -0.80

mu = c(mx,my)
mu
cova = rho*sdx*sdy
cova
aux = c(sdx^2,cova,cova,sdy^2)
sigma = matrix(aux,nrow=2)
sigma

library(MASS)
d0 = mvrnorm(n,mu,sigma)
d0 = data.frame(d0)
x = d0[,1]
y = d0[,2]

plot(y~x,pch=19,cex=0.6)

m1 = lm(y~x)
abline(m1)
grid()

# same scaling
plot(y~x,pch=19,cex=0.6,xlim=c(10,130),ylim=c(110,210))
abline(m1)
grid()

# principal components

pc1 = prcomp(d0)
rot = pc1$rotation
rot

# 1st PC axis, largest variance
slope1 = rot[2,1]/rot[1,1]
int1 = my - mx*slope1
abline(int1,slope1,col="red",lty=2)

# 2nd PC axis, smallest variance
slope2 = rot[2,2]/rot[1,2]
int2 = my - mx*slope2
abline(int2,slope2,col="blue",lty=2)

legend("topright",c("PC1","PC2","LSq"),col=c("red","blue",1),lty=c(1,1,1))

# enclose 95% of obs in an ellipse

library(mixtools)
ellipse(mu,sigma,0.05,2000,col="red")





















