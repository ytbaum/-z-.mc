source("helpers.R")

# initial read-in and prep of data
library(data.table)
train <- fread("data/zip.train")

# as per the exercise, only use rows where the digit is 2 or 3
train <- train[V1 %in% c(2,3), ]

# discard the last column of the data table, which is NA for every row
train <- train[, which(!grepl("^V258$", colnames(train))), with=FALSE]

# isolate the response variable
train.resp <- train[,V1]

# isolate the predictor variables
train.predictors <- train[, which(!grepl("^V1$", colnames(train))), with=FALSE]

# similar procedures as above, but for testing data
test <- fread("data/zip.test")
test <- test[V1 %in% c(2,3),]
test <- test[, which(!grepl("^V258$", colnames(test))), with=FALSE]
test.resp <- test$V1
test.predictors <- test[, which(!grepl("^V1$", colnames(test))), with=FALSE]

# get the LM predictor its predictions
zip.lm <- lm(V1~., data=train)
train.preds <- predict(zip.lm, train.predictors)
train.preds <- get.classification(train.preds)
train.prec <- length(which(train.preds == train.resp)) / length(train.resp)

test.preds <- predict(zip.lm, test.predictors)
test.preds <- get.classification(test.preds)

# get the precision of predictions on the testing data
test.prec <- length(which(test.resp == test.preds)) / length(test.resp)

print(paste("Precision of linear regression classifier's predictions on training set:", train.prec))
print(paste("Precision of linear regression classifier's predictions on testing set:", test.prec))

# run the same tests for knn
library(FNN)

# the suggested values of k to use in running a knn classifier
ks <- c(1,3,5,7,15)
knn.precs <- sapply(ks, knn.prec.at.k, train = train.predictors, test = test.predictors,
       cl = train.resp, true.values = test.resp)
Map(print.knn.prec, ks, knn.precs)
