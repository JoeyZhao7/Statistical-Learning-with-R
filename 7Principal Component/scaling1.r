# scaling1.r

d0=read.table("P1-4.txt")
names(d0)=c("sales","profit","assets")

# only sales and profit
d1=d0[,c(1,2)]

d1
# no centering, no scaling
#===================================================================

pr1=prcomp(d1,center=F,scale=F)
names(pr1)
# [1] "sdev"     "rotation" "center"   "scale"    "x"       
pr1$center
pr1$scale
rot = pr1$rotation
rot
as.matrix(d1)%*%rot
#              PC1        PC2
#  [1,] -109.31806 -8.0512140
#  [2,] -153.20956 -3.9531452
#  [3,]  -95.61630 -3.0253929
#  [4,]  -66.39404 -8.6875789
#  [5,]  -63.54104 -4.2881256
#  [6,] -265.18004 -3.4461289
#  [7,] -265.81530  3.4197684
#  [8,] -285.38543  7.8608183
#  [9,]  -92.36463 -0.4751719
# [10,] -166.03325  2.5880293
sqrt(var(d1$sales))
d1x = pr1$x
d1x
#              PC1        PC2
#  [1,] -109.31806 -8.0512140
#  [2,] -153.20956 -3.9531452
#  [3,]  -95.61630 -3.0253929
#  [4,]  -66.39404 -8.6875789
#  [5,]  -63.54104 -4.2881256
#  [6,] -265.18004 -3.4461289
#  [7,] -265.81530  3.4197684
#  [8,] -285.38543  7.8608183
#  [9,]  -92.36463 -0.4751719
# [10,] -166.03325  2.5880293

# covariance matrix

var(d1)
           sales    profit
sales  7476.4532 303.61862
profit  303.6186  26.19032


          PC1        PC2
PC1 7475.6278 -313.58116
PC2 -313.5812   27.01573

# correlation

cor(d1x)
#           PC1        PC2
#PC1  1.0000000 -0.6977788
#PC2 -0.6977788  1.0000000

# still correlated

# centering (default), prcomp() agrees with eigen()
#===================================================================

pr2=prcomp(d1) #¿ªcenter
pr2$center
pr2$scale
pr2$sdev^2    # [1] 7488.80605   13.83751
pr1$sdev^2    # [1] 34614.78368    30.63902
# the standard deviation of the principal components
# =the square roots of the eigenvalues of the covariance matrix
eigen(var(d1))
# $values
# [1] 7488.80605   13.83751

pr2$rotation
# $vectors
#             [,1]        [,2]
# [1,] -0.99917338  0.04065165
# [2,] -0.04065165 -0.99917338

d2x = pr2$x
var(d2x)
#               PC1           PC2
# PC1  7.488806e+03 -3.446818e-14
# PC2 -3.446818e-14  1.383751e+01
apply(d2x,2,var)
#       PC1        PC2 
#7488.80605   13.83751 

# PCs uncorrelated
# but sum of eigenvals do not add up to p=2

# centering and scaling
#=================================================================

pr3=prcomp(d1,scale=T)
pr3$scale
d3x = pr3$x
var(d3x)
#              PC1          PC2
# PC1 1.686136e+00 7.378598e-19
# PC2 7.378598e-19 3.138640e-01

sum(diag(var(d3x)))   # [1] 2

