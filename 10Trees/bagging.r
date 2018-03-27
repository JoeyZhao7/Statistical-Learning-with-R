# bagging.r
# all 13 predictors should be considered at each split (mtry=p)

library(MASS)      # Boston dataset
library(randomForest)
setwd("C:/Users/Cesar/Favorites/Downloads")  # to save eps files

dim(Boston)  # [1] 506  14
# medv is response, p=13 predictors
n = nrow(Boston)
train = sample(1:n,n/2)   # 253 train rows

# Bagging and Random Forests
#=================================================================
set.seed(1)
bag1=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance = T)
bag1
# randomForest(formula = medv ~ ., data = Boston, mtry = 13, importance = T,subset = train)
#                Type of random forest: regression
#                      Number of trees: 500
# No. of variables tried at each split: 13
#           Mean of squared residuals: 10.54712
#                     % Var explained: 87.23

# 10.54 is train MSE
# p = 13 predictors 
# how to plot one of the 500 trees?
# n. terminal nodes?
# ask to find importance of predictors needed for:  importance(bag1)

names(bag1)
#  [1] "call"            "type"            "predicted"       "mse"            
#  [5] "rsq"             "oob.times"       "importance"      "importanceSD"   
#  [9] "localImportance" "proximity"       "ntree"           "mtry"           
# [13] "forest"          "coefs"           "y"               "test"           
# [17] "inbag"           "terms"          
summary(bag1)
#                 Length Class  Mode     
# call              6    -none- call     
# type              1    -none- character
# predicted       253    -none- numeric  
# mse             500    -none- numeric  
# rsq             500    -none- numeric  
# oob.times       253    -none- numeric  
# importance       26    -none- numeric  
# importanceSD     13    -none- numeric  
# localImportance   0    -none- NULL     
# proximity         0    -none- NULL     
# ntree             1    -none- numeric  
# mtry              1    -none- numeric  
# forest           11    -none- list     
# coefs             0    -none- NULL     
# y               253    -none- numeric  
# test              0    -none- NULL     
# inbag             0    -none- NULL     
# terms             3    terms  call     

# 500 train MSEs

# times train obs was OOB
table(bag1$oob.times)
# 144 153 158 160 161 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180 181 182 183 184 185 
#   1   1   1   1   1   3   1   2   3   2   4   2   2   3   9   6   9   9   6  11  12  10  11   7   8  11   9  10 
# 186 187 188 189 190 191 192 193 194 195 196 197 198 200 201 202 203 204 205 206 207 211 212 215 216 217 
#   6   9   8   9   6   8   5   1   7   6   3   5   2   1   3   5   2   2   2   2   1   1   1   1   1   1 

head(bag1$predicted)
#      175      410      307      222      391      344 
# 21.90482 16.87879 34.48303 17.70609 14.03278 29.22515 
head(bag1$y)
#  175  410  307  222  391  344 
# 22.6 27.5 33.4 21.7 15.1 23.9 
head(Boston[train,"medv"])
# [1] 22.6 27.5 33.4 21.7 15.1 23.9
 

# test set performance
y.test=Boston[-train,"medv"]         # y values in test set
yhat.bag = predict(bag1,newdata=Boston[-train,])

# residuals and  row numbers
res = y.test - yhat.bag
a = rownames(as.matrix(yhat.bag))   # as.matrix is required

# plot means of terminal regions (yhat) vs y
plot(yhat.bag~y.test,pch=19,cex=0.5,ylim=c(10,50))
abline(0,1)
grid()
text(yhat.bag~y.test,labels=ifelse(res>5,a,""),pos=1,offset=0.25,cex=0.4)
# dots seem to cluster around 45 degree line

# MSEP
mean((yhat.bag-y.test)^2)    #[1] 12.81118    (this value changes)
# half of CV best tree MSEP -good!

# limit n. of trees to 25
bag2=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag2,newdata=Boston[-train,])
mean((yhat.bag-y.test)^2)    # [1] 14.29294    (this value changes)


# random forest (mtry < p) 
#=================================================================
set.seed(1)
forest1=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=T)
forest1
# randomForest(formula = medv ~ ., data = Boston, mtry = 6, importance = T,subset = train)
#                Type of random forest: regression
#                      Number of trees: 500
# No. of variables tried at each split: 6
#           Mean of squared residuals: 11.64401
#                     % Var explained: 85.9

# test set performance
yhat.rf = predict(forest1,newdata=Boston[-train,])
mean((yhat.rf-y.test)^2)      #[1] 11.20823
# little improvement over bagging

# importance of each predictor

importance(forest1)
#            %IncMSE IncNodePurity
# crim    12.4532926    1025.78688
# zn       2.7950457      52.75363
# indus   10.8424427     982.29679
# chas     0.9750709      67.04497
# nox     11.9825575    1264.03961
# rm      34.0973035    6667.19323
# age      9.6228463     458.13995
# dis     14.4846189    1280.22577
# rad      4.0478715     106.03032
# tax      7.6757169     481.81702
# ptratio 12.6021811    1030.07392
# black    6.7831663     373.05591
# lstat   27.4485257    6825.52957

# IncMSE - avg increase in MSE when predictor is excluded from model
# IncNodePurity - avg increase in RSS from splits using this predictor

# plot these two columns - for convenience
varImpPlot(forest1,main="")

# rm & lstat most important predictors
