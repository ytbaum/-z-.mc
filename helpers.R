get.classification <- function(x)
{
  threes <- x > 2.5
  x[threes] <- 3
  x[!threes] <- 2
  
  x  
}

get.knn.preds <- function(train, test, cl, k)
{
  knn.preds <- knn(train, test, cl, k)
  knn.preds <- as.numeric(levels(knn.preds))[knn.preds]
  
  knn.preds
}

knn.prec.at.k <- function(k, train, test, cl, true.values)
{
  knn.preds <- get.knn.preds(train, test, cl, k)
  prec <- length(which(knn.preds == true.values)) / length(true.values)
  
  prec  
}