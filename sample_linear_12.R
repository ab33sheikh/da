ad <- read.csv('C:/Users/NAVAMI/Desktop/student.csv')
train_index <- sample(nrow(ad), 0.8 * nrow(ad))
train <- ad[train_index, ]

# s1 marks depends on s2 marks

test <- ad[-train_index,]$s2
x <- train$s2
y <- train$s1

# # s2 depends on s3
# 
# test <- ad[-train_index,]$s3
# x <- train$s3
# y <- train$s2

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

lin_reg(x, y, test)

# y ~ x
mod1 <- lm(y ~ x)
mod1$coefficients

summary(mod1)
