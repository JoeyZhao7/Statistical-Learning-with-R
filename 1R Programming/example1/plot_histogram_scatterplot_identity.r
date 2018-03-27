# Aug25a.r

1:10
#  [1]  1  2  3  4  5  6  7  8  9 10
a = 1:10
a
#  [1]  1  2  3  4  5  6  7  8  9 10
a <- 1:10
a <- 1:10/5
a
#  [1] 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0
b = 5
b
# [1] 5
2+3*5
# [1] 17

install.packages("ISLR")
library(ISLR)
library(help=ISLR)
d2 = Auto
head(d2)
#   mpg cylinders displacement horsepower weight acceleration year origin                      name
# 1  18         8          307        130   3504         12.0   70      1 chevrolet chevelle malibu
# 2  15         8          350        165   3693         11.5   70      1         buick skylark 320
# 3  18         8          318        150   3436         11.0   70      1        plymouth satellite
# 4  16         8          304        150   3433         12.0   70      1             amc rebel sst
# 5  17         8          302        140   3449         10.5   70      1               ford torino
# 6  15         8          429        198   4341         10.0   70      1          ford galaxie 500
tail(d2)
#     mpg cylinders displacement horsepower weight acceleration year origin             name
# 392  27         4          151         90   2950         17.3   82      1 chevrolet camaro
# 393  27         4          140         86   2790         15.6   82      1  ford mustang gl
# 394  44         4           97         52   2130         24.6   82      2        vw pickup
# 395  32         4          135         84   2295         11.6   82      1    dodge rampage
# 396  28         4          120         79   2625         18.6   82      1      ford ranger
# 397  31         4          119         82   2720         19.4   82      1       chevy s-10
dim(d2)
# [1] 392   9
str(d2)
# 'data.frame':   392 obs. of  9 variables:
#  $ mpg         : num  18 15 18 16 17 15 14 14 14 15 ...
#  $ cylinders   : num  8 8 8 8 8 8 8 8 8 8 ...
#  $ displacement: num  307 350 318 304 302 429 454 440 455 390 ...
#  $ horsepower  : num  130 165 150 150 140 198 220 215 225 190 ...
#  $ weight      : num  3504 3693 3436 3433 3449 ...
#  $ acceleration: num  12 11.5 11 12 10.5 10 9 8.5 10 8.5 ...
#  $ year        : num  70 70 70 70 70 70 70 70 70 70 ...
#  $ origin      : num  1 1 1 1 1 1 1 1 1 1 ...
#  $ name        : Factor w/ 304 levels "amc ambassador brougham",..: 49 36 231 14 161 141 54 223 241 2 ...
summary(d2)
#       mpg          cylinders      displacement     horsepower        weight      acceleration        year           origin
#  Min.   : 9.00   Min.   :3.000   Min.   : 68.0   Min.   : 46.0   Min.   :1613   Min.   : 8.00   Min.   :70.00   Min.   :1.000
#  1st Qu.:17.00   1st Qu.:4.000   1st Qu.:105.0   1st Qu.: 75.0   1st Qu.:2225   1st Qu.:13.78   1st Qu.:73.00   1st Qu.:1.000
#  Median :22.75   Median :4.000   Median :151.0   Median : 93.5   Median :2804   Median :15.50   Median :76.00   Median :1.000
#  Mean   :23.45   Mean   :5.472   Mean   :194.4   Mean   :104.5   Mean   :2978   Mean   :15.54   Mean   :75.98   Mean   :1.577
#  3rd Qu.:29.00   3rd Qu.:8.000   3rd Qu.:275.8   3rd Qu.:126.0   3rd Qu.:3615   3rd Qu.:17.02   3rd Qu.:79.00   3rd Qu.:2.000
#  Max.   :46.60   Max.   :8.000   Max.   :455.0   Max.   :230.0   Max.   :5140   Max.   :24.80   Max.   :82.00   Max.   :3.000

plot(cylinders,mpg)
# Error in plot(cylinders, mpg) : object 'cylinders' not found

plot(d2$cylinders,d2$mpg)
d2$cylinders=factor(d2$cylinders)
str(d2)
# 'data.frame':   392 obs. of  9 variables:
#  $ mpg         : num  18 15 18 16 17 15 14 14 14 15 ...
#  $ cylinders   : Factor w/ 5 levels "3","4","5","6",..: 5 5 5 5 5 5 5 5 5 5 ...
#  $ displacement: num  307 350 318 304 302 429 454 440 455 390 ...
#  $ horsepower  : num  130 165 150 150 140 198 220 215 225 190 ...
#  $ weight      : num  3504 3693 3436 3433 3449 ...
#  $ acceleration: num  12 11.5 11 12 10.5 10 9 8.5 10 8.5 ...
#  $ year        : num  70 70 70 70 70 70 70 70 70 70 ...
#  $ origin      : num  1 1 1 1 1 1 1 1 1 1 ...
#  $ name        : Factor w/ 304 levels "amc ambassador brougham",..: 49 36 231 14 161 141 54 223 241 2 ...
plot(d2$cylinders,d2$mpg)
mpg = d2$mpg
ls() #a list of setted variables
# [1] "a"   "b"   "d2"  "mpg"
hist(mpg)
hist(mpg,freq=F) #频率分布直方图 density
h1=hist(mpg,freq=T) #频数分布直方图 frequency
plot(h1)
summary(h1)
#          Length Class  Mode
# breaks   10     -none- numeric
# counts    9     -none- numeric
# density   9     -none- numeric
# mids      9     -none- numeric
# xname     1     -none- character
# equidist  1     -none- logical
h1$breaks
#  [1]  5 10 15 20 25 30 35 40 45 50
# not relative freq since bars width is not equal to 1
hh = hist(mpg)
hh$counts
hh$counts = hh$counts/sum(hh$counts)
plot(hh) #这个做出来是伪频率图

# scatterplot
plot(d2$horsepower,d2$weight)
plot(weight~horsepower,d2)
plot(weight~horsepower,d2,pch=19,cex=0.6)
grid()

# fitted line
m1 = lm(weight~horsepower,d2)
abline(m1)
abline(m1,col="red",lwd=2)
res = resid(m1)
id = which(res==min(res)) #寻找某一个点
id
14
d2[14,]
#    mpg cylinders displacement horsepower weight acceleration year origin
# 14  14         8          455        225   3086           10   70      1

pairs(d2)
pairs(~mpg+displacement+horsepower+weight+acceleration,d2) #~要加，但是变量在前在后无所谓
hp = d2$horsepower
plot(hp,mpg,pch=19,cex=0.6)
identify(hp,mpg,rownames(d2)) #能用，但是要点右上角的finish

plot(hp,mpg,pch=19,cex=0.6)
identify(hp,mpg,d2$name)
