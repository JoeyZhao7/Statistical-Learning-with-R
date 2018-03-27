# boosting.r   p331

library(MASS)      # Boston dataset
library(gbm)       # gbm()
setwd("C:/Users/Cesar/Favorites/Downloads")  # to save eps files

dim(Boston)
# [1] 506  14
# medv is response, p=13 predictors
n = nrow(Boston)
set.seed(1)
train = sample(1:n,n/2)   # 253 train rows

# boosted regression trees
#==============================================================================================
set.seed(1)
boost1=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
# for categorical response use distribution="bernoulli"
# depth of each tree limited to 4 splits
# 5000 trees (default is 100 trees)

names(boost1)
#  [1] "initF"             "fit"               "train.error"       "valid.error"
#  [5] "oobag.improve"     "trees"             "c.splits"          "bag.fraction"
#  [9] "distribution"      "interaction.depth" "n.minobsinnode"    "num.classes"
# [13] "n.trees"           "nTrain"            "train.fraction"    "response.name"
# [17] "shrinkage"         "var.levels"        "var.monotone"      "var.names"
# [21] "var.type"          "verbose"           "data"              "Terms"
# [25] "cv.folds"          "call"              "m"

# lambda -default value
boost1$shrinkage       #[1] 0.001

# importance of predictors
summary(boost1); grid()
#             var     rel.inf
# lstat     lstat 45.96758091
# rm           rm 31.22024973
# dis         dis  6.80540085
# crim       crim  4.06995214
# nox         nox  2.55687077
# ptratio ptratio  2.27547427
# black     black  1.79481300
# age         age  1.65100042
# tax         tax  1.36245454
# indus     indus  1.26933836
# chas       chas  0.80150463
# rad         rad  0.21054835
# zn           zn  0.01481202

# creates a plot
# lstat and rm best predictors

# test MSPE
y.test=Boston[-train,"medv"]       # y values in test set
yhat.boost=predict(boost1,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-y.test)^2)        # [1] 11.84445
# mspe similar to Random Forest mspe

# lambda = 0.2   (shrinkage)
boost2=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost2,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-y.test)^2)        # [1] 11.51109

# n.trees in predict function cannot exceed
# n.trees in gbm()
