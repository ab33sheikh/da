ad = read.csv("C:/Users/NAVAMI/Desktop/data/mlr_pub.csv")


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
x = as.matrix(cbind(1, train$train_program, train$past_pub))
y = as.matrix(train$curr_pub)
beta_hat = mlr(x, y)
cat("beta", beta_hat)

# test validation
test

mod1 <- lm(train$curr_pub ~ ., data = train)
summary(mod1)
