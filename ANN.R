#refer to https://rpubs.com/julianhatwell/annr

library(MASS) #for Boston Data
library(ggplot2)
library(ggcorrplot)
library(neuralnet)
#check for NAs
sum(is.na(Boston))

abs(cor(Boston))>0.8 #only tax and rad are highly correlated

ggcorrplot(cor(Boston),method="circle")

#Linear regression model
index <- sample(1:nrow(Boston),round(0.75*nrow(Boston)))
train <- Boston[index,]
test <- Boston[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
MSE.lm

#ANN
#need to scale both response variable and predictors
Boston.scaled <- as.data.frame(scale(Boston))
#and response variable must be between 0 and 1
min.medv <- min(Boston$medv)
max.medv <- max(Boston$medv)

Boston.scaled$medv <- scale(Boston$medv
                            , center = min.medv
                            , scale = max.medv - min.medv)

train <- Boston.scaled[index,]
test <- Boston.scaled[-index,]

n <- names(train)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=T)

plot(nn)

#Predicting with ANN
pr.nn <- compute(nn,test[,1:13])
pr.nn <- pr.nn$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)
test.r <- (test$medv)*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)

MSE.nn <- sum((test.r - pr.nn)^2)/nrow(test)
MSE.nn
MSE.lm

#Cross-Validation
par(mfrow=c(1,2))
plot(test$medv,pr.nn,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

library(boot)
set.seed(200)
lm.fit <- glm(medv~.,data=Boston)
cv.glm(Boston,lm.fit,K=10)$delta[1]

#ANN CV error
set.seed(450)
cv.error <- NULL
k <- 10

#spliting data randomly for 10 times
for(i in 1:k){
  index <- sample(1:nrow(Boston),round(0.9*nrow(Boston)))
  train.cv <- Boston.scaled[index,]
  test.cv <- Boston.scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
}

mean(cv.error)
