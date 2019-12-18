ad = read.csv("C:/Users/NAVAMI/Desktop/data/Advertising.csv")


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
X = as.matrix(cbind(1, train$TV, train$radio, train$newspaper))
y = as.matrix(train$sales)
beta_hat = mlr(X, y)

# test validation
test
cat(beta_hat)
mod1 <- lm(sales ~ ., data = train)
summary(mod1)
