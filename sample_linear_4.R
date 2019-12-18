ad <- read.csv('C:/Users/NAVAMI/Desktop/budget_lin.csv')
train_index <- sample(nrow(ad), 0.8 * nrow(ad))

train <- ad[train_index, ]
test <- ad[-train_index,]$budget
# budget values
# test = c(200, 500)

lin_reg <- function(x, y, test) {
  x_bar = mean(x)
  y_bar = mean(y)
  X = x - x_bar
  Y = y - y_bar
  
  B1 = sum(X * Y) / sum(X^2)
  B0 = y_bar - B1 * x_bar
  cat(B0, B1, '\n')
  
  # values predicted by model
  y_hat = B0 + B1 * x
  
  rss = sum( (y - y_hat) ** 2 )
  tss = sum(Y**2)
  
  R_squared = 1 - rss/tss
  
  RSE = sqrt(rss / (length(x) - 2))
  
  corr = sum (X * Y) / sqrt( sum(X**2) *  sum(Y**2) )
  cat(R_squared, '\n', B0 + B1 * test, '\n', RSE, '\n', corr)
}

lin_reg(x = train$budget, y = train$sales, test)

# y ~ x
mod1 <- lm(sales ~ budget, data = train)
mod1$coefficients

summary(mod1)
