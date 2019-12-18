library(MASS)
library(ggplot2)
bankDS = read.csv("C:/Users/NAVAMI/Desktop/data/lda_qda1.csv")
dim(bankDS)

str(bankDS)

summary(bankDS)

attach(bankDS)
UniqueValue = function (x) {length(unique(x)) }
apply(bankDS, 2, UniqueValue)

NaValue = function (x) {sum(is.na(x)) }
apply(bankDS, 2, NaValue)

BlankValue = function (x) {sum(x=="") }
apply(bankDS, 2, BlankValue)

MissPercentage = function (x) {100 * sum (is.na(x)) / length (x) }
apply(bankDS, 2, MissPercentage)

bankDS$creditcard[is.na(bankDS$creditcard)] = mean(bankDS$creditcard, na.rm=TRUE)
apply(bankDS, 2, MissPercentage)

set.seed(1)
row.number = sample(1:nrow(bankDS), 0.6*nrow(bankDS))
train = bankDS[row.number,]
test = bankDS[-row.number,]
dim(train)
dim(test)


# LDA model
attach(train)
lda.model = lda (factor(default)~factor(creditcard)+balance, data=train)
lda.model


##Predicting training results.
predmodel.train.lda = predict(lda.model, data=train)
table(Predicted=predmodel.train.lda$class, Survived=creditcard)

ldahist(predmodel.train.lda$x[,1], g= predmodel.train.lda$class)


attach(test)
predmodel.test.lda = predict(lda.model, newdata=test)
table(Predicted=predmodel.test.lda$class, Survived=test$creditcard)

par(mfrow=c(1,1))
plot(predmodel.test.lda$x[,1], predmodel.test.lda$default, col=test$creditcard)



# QDA model

