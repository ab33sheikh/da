ad = read.csv("C:/Users/NAVAMI/Desktop/student.csv")


indices <- sample(nrow(ad), size = 0.7 * nrow(ad))
train <- ad[indices,]
test  <- ad[-indices,]

mlr <- function(x, y) {
  beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y
  y_hat = x %*% beta_hat
  
  X = x - mean(x)
  Y = y - mean(y)
  
  e = y - y_hat
  rss = sum(e^2)
  tss = sum(Y^2)
  
  R_squared = 1 - rss/tss
  
  RSE = sqrt(rss / (length(x) - 2))
  
  # corr = sum (X * Y) / sqrt( sum(X^2) *  sum(Y^2) )
  cat(rss, '\n', tss, '\n', R_squared, '\n', RSE, '\n')
  
  return(beta_hat)
}

# column bind
X = as.matrix(cbind(1, train$s2, train$s3))
y = as.matrix(train$s1)
beta_hat = mlr(X, y)
cat("beta", beta_hat)

# test validation
test

mod1 <- lm(train$s1 ~ ., data = train)
summary(mod1)
