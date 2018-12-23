######################################
#R basic
#####################################

#vector
x <- c(1,3,2,5)
x
length(x)
y <- c(1,4,3)
length(y)
x+y #automate circle

ls()
rm(x,y)
ls()
#remove all at once
rm(list=ls())

#matrix
x <- matrix(c(1,2,3,4),2,2,byrow = TRUE)
x
#sqrt root and powers
sqrt(x)
x^2

#random numbers
x <- rnorm(50)
y <- x+rnorm(50,mean=50,sd=.1)
#correlation
cor(x,y)
set.seed(1303)
rnorm(50)

#summary statistics
mean(y)
var(y)
sqrt(var(y)) #sd
sd(y)
summary(Auto)
summary(Auto1)
summary(Auto$mpg)

#basic plot
set.seed(1)
x <- rnorm(100)
set.seed(2)
y <- rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",
     ylab="this is the y-axis",
     main="Plot of X vs Y")

#create pdf
pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()

#sequence
x <- seq(1,10)
x
x <- 1:10
x
x <- seq(-pi,pi,length=50)
x

#contour plot
y <- x
f <- outer(x,y,function(x,y) cos(y)/(1+x^2))
contour(x,y,f,nlevels=45,add=T)
fa <- (f-t(f))/2
contour(x,y,fa,nlevels=15)
#heatmap
image(x,y,fa)
persp(x,y,fa,theta=30,phi=40)

#indexing data
a <- matrix(1:16,4,4,byrow=TRUE)
a
a[2,3]
a[c(1,3),c(2,4)]
a[1:3,2:4]
a[1:2,]
a[,1:2]
a[-c(1,3),]
dim(a) #number of rows and columns

#Loading data
Auto <- read.table("R-intro to statistical learning/Auto.data",header=T,na.strings="?")
#view table
fix(Auto)
dim(Auto)
Auto[1:4,]
Auto1 <- na.omit(Auto) #remove 5 rows containing missing observations
dim(Auto1)

#convert quantitative var into qualitative var
Auto1$cylinders <- as.factor(Auto1$cylinders)
plot(Auto1$cylinders,Auto1$mpg) #automatic boxplot

#customize boxplot
plot(Auto1$cylinders,Auto1$mpg, col="red",
     varwidth=T,
     horizontal=T,
     xlab="cylinders",ylab="MPG")

#histgram
hist(Auto1$mpg,col=2,breaks=15)
#scatterplotmatix
pairs(Auto)
pairs(~mpg+displacement+horsepower+weight+acceleration,Auto)
plot(Auto1$horsepower,Auto1$mpg)

##############################################################################
#Linear Regression
###################################
library(MASS)
library(ISLR)

#Boston dataset
fix(Boston)
names(Boston)

#simple linear regression
lm.fit <- lm(medv ~ lstat, Boston)
#basic information
lm.fit
#summary
summary(lm.fit)
names(lm.fit)

#confidence interval
confint(lm.fit)
#prediction and prediction intervals
predict(lm.fit,newdata=data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,newdata=data.frame(lstat=c(5,10,15)),interval="prediction")

#plot
plot(lstat~medv,data=Boston)
abline(lm.fit,col="red",lwd=3) #there's some evidence for non-linearity
#diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)
#get residule
plot(predict(lm.fit),residuals(lm.fit)) #there's evidence of non-linaerity
plot(predict(lm.fit),rstudent(lm.fit))
#compute hatvalues/leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) #Largest element of a vector

#multiple linear regression
lm.fit2 <- lm(medv ~ lstat + age, data=Boston)
summary(lm.fit2)
lm.fit3 <- lm(medv ~.,data=Boston)
summary(lm.fit3)
#r square and RSE
summary(lm.fit3)$r.sq
summary(lm.fit3)$sigma #sqrt(MSE)
#VIF - multicolinearity in car library
library(car)
vif(lm.fit3)
lm.fit4 <- update(lm.fit3,~.-age)
vif(lm.fit4)
summary(lm.fit4)

#interaction term
summary(lm(medv~lstat*age,data=Boston))
#non-linear trasform
lm.fit5 <- lm(medv~lstat+I(lstat^2),data=Boston)
#comparison of models
anova(lm.fit,lm.fit5)
plot(lm.fit5)
#polynomial term, log term
summary(lm(medv~log(rm),data=Boston))

#qualitative predictors
#Carseats dataset
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc) #coding of dummy variables


##############################################################################
#Model selection method
###################################
library(ISLR)
names(Hitters)
sum(is.na(Hitters$Salary)) #missing values

#remove missing value
dim(Hitters)
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

#subset selection in leaps library
library(leaps)
reg.full <- regsubsets(Salary~.,data=Hitters,nvmax=19) #nvmax:number of variable
summary(reg.full)
#decide the best model through plot
names(summary(reg.full))
rs.summary <- summary(reg.full)
min.RSS <- which.min(rs.summary$rss)
min.RSS
min.bic <- which.min(rs.summary$bic)
min.bic
min.cp <- which.min(rs.summary$cp)
min.cp
max.rsq <- which.max(rs.summary$adjr2)
max.rsq
par(mfrow=c(2,2))
plot(rs.summary$rss,xlab = "number if variables",ylab="RSS")
points(min.RSS,rs.summary$rss[min.RSS],col="red",pch=20)
plot(rs.summary$adjr2,xlab = "number if variables",ylab="Adjusted R squred")
points(max.rsq,rs.summary$adjr2[max.rsq],col="red",pch=20)
plot(rs.summary$cp,xlab = "number if variables",ylab="Cp")
points(min.cp,rs.summary$cp[min.cp],col="red",pch=20)
plot(rs.summary$bic,xlab = "number if variables",ylab="BIC") #bigger penalty
points(min.bic,rs.summary$bic[min.bic],col="red",pch=20)

#buildin plot
par(mfrow=c(1,1))
plot(reg.full,scale = "adjr2")
#cefficient of model 
coef(reg.full,6) #when using six variables
#forward selection and backard selection
reg.bw <- regsubsets(Salary~.,data=Hitters,nvmax = 19,method = "backward")
reg.fw <- regsubsets(Salary~.,data=Hitters,nvmax = 19,method = "forward")
plot(reg.bw,scale = "adjr2")
coef(reg.bw,9)
plot(reg.fw,scale="adjr2")
coef(reg.fw,9)

#creat a model matrix
train.mat <- model.matrix(Salary~.,data=Hitters)
#predict on train
coefficient <- coef(reg.bw,9)
train.pred <- train.mat[,names(coefficient)]%*%coefficient
#train MSE
mean((train.pred-Hitters$Salary)^2)

#Test set cross-validation
#test/train split - method 1: use TRUE and FALSE vector
set.seed(1)
train <- sample(c(TRUE,FALSE),nrow(Hitters),replace = TRUE)
test <- !train
#best subset selection on train set
regbest <- regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
#test matrix
test.matrix <- model.matrix(Salary~.,data=Hitters[test,])
#calculate every MSE of each size i
cv.mse <- rep(FALSE,19)
for (i in 1:19) {
  coefficient <-  coef(regbest,i)
  pred <- test.matrix[,names(coefficient)]%*%coefficient
  cv.mse[i] <- mean((pred-Hitters[test,]$Salary)^2)
}
plot(cv.mse)


###########################################################################
#ridge regression and lasso
###################################
#Ridge regression and the Lasso
#From glmnet package
library(glmnet)
#Using it must transfrom data to a X matrix and Y vector first
X=model.matrix(Salary~.,data = Hitters)[,-1]
Y=Hitters$Salary
#Redge regression, alpha=0
grid <- 10^seq(10,-2,length=100)
rid.fit <- glmnet(X,Y,alpha=0,lambda=grid) #by default, glmnet standardized data
#get coefficient by a given lambda
coef(rid.fit)[,10]
rid.fit$lambda[10]
coef(rid.fit)[,99]
rid.fit$lambda[99]
#assign a new lambda
predict(rid.fit,s=50,type="coefficients")[1:20,]
#split data into train/test: method2, use random numbers
set.seed(1)
train <- sample(1:nrow(X),nrow(X)/2)
test <- -train
Y.test <- Y[test]
#Ridge regression
#fit on train
rid.mod <- glmnet(X[train,],Y[train],lambda = grid,alpha = 0)
#predict on test
rid.pred <- predict(rid.mod,s=4,newx = X[test,])
#MSE
mean((rid.pred-Y.test)^2)

#buidin function (cv.glmnet) to choose lambda
cv.rid <- cv.glmnet(X[train,],Y[train],alpha=0)
plot(cv.rid)
cv.rid$lambda.min

#tes MSE using the best lambda
rid.pred <- predict(rid.mod,s=cv.rid$lambda.min,newx = X[test,])
mean((rid.pred-Y.test)^2)

#At last: build model on full data and check coefficient with best lambda
rid.fit <- glmnet(X,Y,alpha=0)
predict(rid.fit,type="coefficient",s=cv.rid$lambda.min)[1:20,]

########################################################################
#Lasso
##############################################
#The Lasso: alpha=1
las.mod <- glmnet(X[train,],Y[train],alpha=1,lambda=grid)
plot(las.mod)

#choose lambda by cv
cv.las <- cv.glmnet(X[train,],Y[train],alpha=1)
bestlambda <- cv.las$lambda.min
las.pred <- predict(las.mod,s=bestlambda,newx = X[test,])
mean((las.pred-Y[test])^2)

#check lasso's coefficient
las.fit <- glmnet(X,Y,alpha = 1,lambda = grid)
predict(las.fit,type = "coefficient",s=bestlambda)[1:20,]


######################################################################
#PCA + Regression (all in one function of the package): 
#different from PCA 
#############################
#PCR(in pls package)
library(pls)
#make sure there's no missing value
sum(is.na(Hitters))
pcr.fit <- pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
#plot MSEP
validationplot(pcr.fit,val.type = "MSEP")

#fit on train
pcr.fit <- pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
#choose 7 to test
pcr.pred <- predict(pcr.fit,X[test,],ncomp=7)
#test MSE
mean((pcr.pred-Y[test])^2)

#PLS
set.seed(1)
pls.fit <- plsr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
summary(pls.fit)

#######################################################################
#Polynomial regression, spline, GAMs ,lowess
###############################
library(ISLR)
attach(Wage)
library(splines)

#####################
#Polynomial regression
ploy.fit <- lm(wage~poly(age,4),data=Wage)
summary(ploy.fit)

#predit with grid of age
agelims <- range(age)
age.grid <- seq(from=agelims[1],to=agelims[2])
preds <- predict(ploy.fit,newdata=list(age=age.grid),se=TRUE)
se.bands <- cbind(preds$fit-2*preds$se.fit,preds$fit+2*preds$se.fit)
#plot data
plot(age,wage,col="lightgrey",cex=.5)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="red",lty=3)

#Method1: choosing polynomial degree using ANOVA / Other method: can use CV
fit1 <- lm(wage~poly(age,2),data=Wage)
fit2 <- lm(wage~poly(age,3),data=Wage)
fit3 <- lm(wage~poly(age,4),data=Wage)
anova(fit1,fit2,fit3)

############################3
#GAM
library(gam)
#using natural spline
gam1 <- lm(wage~ns(year,4)+ns(age,5)+education,data = Wage)
#using smoothing spline
gam2 <- lm(wage~s(year,4)+s(age,5)+education,data = Wage)
plot(gam2,se=T,col="blue")
plot(gam1,se=T,col="red")

###########################
#spline
#regression spline
fit <- lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
pred <- predict(fit,newdata = list(age=age.grid),se=T)
plot(age,wage,data=Wage,col="grey")
lines(age.grid,pred$fit,col="red")
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

#natural spline
fit2 <- lm(wage~ns(age,df=4),data=Wage)
pred2 <- predict(fit2,newsdata = list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="blue")

#smooth spline
fit <- smooth.spline(age,wage,cv=TRUE)
fit$df

#lowess
fit2 <- loess(wage~age,span = .2,data = Wage)
plot(fit2)

###########################################################################
#classification
#################################################################

############################################################################
#Logistic regression
########################################################
#for classification
class.fit <- glm(I(wage>250)~poly(age,4),data=Wage,family="binomial")
class.pred <- predict(class.fit,newdata = list(age=age.grid),se=T)
#!!! the response is: log(p(x)/1-p(X))
#need transformation back
pfit <- exp(class.pred$fit)/(exp(class.pred$fit)+1)

library(ISLR)
#Smarket Dataset
names(Smarket)
str(Smarket)
summary(Smarket)
#correlation matrix
cor(Smarket[,-9])
plot(Smarket$Volume)

#Logistic regression
glm.fits <- glm(Direction ~ Volume + Lag1 +Lag2 +Lag3 +Lag4 + Lag5,
                data = Smarket,
                family=binomial)
summary(glm.fits)
#predict probability
glm.probs <- predict(glm.fits,type="response") #training data
glm.probs[1:10]
#convert probabilities to class labels
glm.pred <- rep("Up",nrow(Smarket))
glm.pred[glm.probs<0.5] <- "Down" #0.5 threshold

#confusion matrix
table(Smarket$Direction,glm.pred)
#overall error rate
mean(glm.pred!=Smarket$Direction)

#Split data to train and test set
train=(Smarket$Year<2005)
Smarket.2005 <- Smarket[!train,]
Direction.2005 <- Smarket.2005$Direction
glm.fit2 <- glm(Direction~Volume+Lag1+Lag2+Lag3+Lag4+Lag5,
                data=Smarket,subset=train,
                family = binomial)
glm.pro2 <- predict(glm.fit2,newdata=Smarket.2005,type="response")
glm.preds2 <- rep("Down",nrow(Smarket.2005))
glm.preds2[glm.pro2>0.5] <- "Up"
table(Direction.2005,glm.preds2)

####################################################################
#KNN
##########################
library(class)
#prepare data
train.X <- cbind(Smarket$Lag1,Smarket$Lag2)[train,]
test.X <- cbind(Smarket$Lag1,Smarket$Lag2)[!train,]
train.Y <- Smarket$Direction[train]
set.seed(1)
test.Y <- knn(train.X,test.X,train.Y,k=1)
table(Direction.2005,test.Y)
mean(test.Y!=Direction.2005)

###################################################################
#Decision Trees
##########################
library(tree)
library(ISLR)
attach(Carseats)
#create a new column
High <- ifelse(Sales<=8,"No","Yes")
#Merge high with dataset
Carseats <- data.frame(Carseats,High)

#######classification tree
#fit classification tree
car.tree <- tree(High~.-Sales,data=Carseats)
summary(car.tree)
plot(car.tree)
text(car.tree,pretty=0)

car.tree

#fit on the train set and predict on test set
set.seed(1)
train <- sample(1:nrow(Carseats),200)
carseats.test <- Carseats[-train,]
High.test <- High[-train]
carseats.tree <- tree(High~.-Sales,data=Carseats,subset=train)
carseats.pred <- predict(carseats.tree,newdata=carseats.test,type="class")
table(carseats.pred,High.test)

#decide how to prune the tree
set.seed(1)
cv.carseats <- cv.tree(carseats.tree,FUN=prune.misclass)
cv.carseats
plot(cv.carseats$size,cv.carseats$dev,type="b")

#prune the tree
carseats.prune <- prune.misclass(carseats.tree,best=10)

#predict use pruned tree
prune.predict <- predict(carseats.prune,newdata = carseats.test,type="class")
table(prune.predict,High.test)
plot(carseats.prune)
text(carseats.prune)

########regression tree
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston),nrow(Boston)/2)
boston.tree <- tree(medv~.,data=Boston,subset = train)
summary(boston.tree)
plot(boston.tree)
text(boston.tree)
#to decide whether prune the tree
cv.boston <- cv.tree(boston.tree)
plot(cv.boston$size,cv.boston$dev,type="b")
#prune the tree
prune.boston <- prune.tree(boston.tree,best=4)
plot(prune.boston)
text(prune.boston)

#use unpruned tree to predict
boston.pred <- predict(boston.tree,newdata = Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(boston.test,boston.pred)
abline(0,1)

##################################################################
#bagging and random forest
####################################
library(randomForest)
set.seed(1)
#bagging, mtry=number of predictors
bag.boston <- randomForest(medv~.,data=Boston,suset=train,mtry=13,importance=TRUE)
bag.boston
importance(bag.boston) #out of bag importance

bag.pred <- predict(bag.boston,newdata = Boston[-train,])
plot(bag.pred,boston.test)

#random forests
#mtry=1/3 number of predictors
rf.boston <- randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
rf.pred <- predict(rf.boston,newdata=Boston[-train,])
plot(rf.pred,boston.test)
importance(rf.boston)
varImpPlot(rf.boston)
varImpPlot(bag.boston)

###############################################################
#Boosting
##################
library(gbm)
boost.boston <- gbm(medv~.,data=Boston[train,],
                    distribution = "gaussian",n.trees = 5000,interaction.depth = 4)
summary(boost.boston)
#partial dependece plots
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
boost.pred <- predict(boost.boston,newdata=Boston[-train,],n.trees = 5000)
plot(boost.pred,boston.test)
mean((boost.pred-boston.test)^2)

##############################################################
#SVM
###################
library(e1071)
#generating data - two classes
set.seed(1)
x <- matrix(rnorm(20*2),ncol=2)
y <- c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1
plot(x,col=(3-y))
dat <- data.frame(x=x,y=as.factor(y))  

#support vector classifier(kernel=linear)
svm.fit <- svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE) #best cost can get through c-v
plot(svm.fit,dat)
svm.fit$index
summary(svm.fit)

#support vector machine
#generate data 
x <- matrix(rnorm(200*2),ncol=2)
y <- c(rep(1,150),rep(2,50))
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
dat <- data.frame(x=1,y=as.factor(y)) # classification, y must be factor
plot(x,col=y)
train <- sample(200,100)
#kernel=radial
svm.fit <- svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
summary(svm.fit)

#produce ROC curve
library(ROCR)
#function to plot ROC curve
rocplot=function(pred,truth,...){
  predob <- prediction(pred,truth)
  perf <- performance(predob,"tpr","fpr")
  plot(perf,...)
}
svm.fit <- svm(y~.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
fitted <- attributes(predict(svm.fit,dat[train,],decision.values=TRUE))$decision.values
rocplot(fitted,dat[train,"y"])

##################################################################
######Dimention Reduction
#################################################################

############################################################################
#PCA
# load swiss bank notes
x<-read.table("bank2.dat")
#####princomp: covariance matrix 
spr <-princomp(x)
U<-spr$loadings
L<-(spr$sdev)^2
Z <-spr$scores
# scatterplot of input x
pairs(x,pch=c(rep(1,100),rep(3,100)),col=c(rep("blue",100),rep("red",100)))
# scatterplot of principal components
pairs(Z,pch=c(rep(1,100),rep(3,100)),col=c(rep("blue",100),rep("red",100)))

# variances of each principal component
par(mfrow=c(1,2))
plot(L,type="b",xlab="component",ylab="lambda",main="Scree plot")
plot(cumsum(L)/sum(L)*100,ylim=c(0,100),type="b",xlab="component",ylab="cumulative propotion (%)",main="Cum. Scree plot")
# biplot
par(mfrow=c(1,1))
biplot(spr, choices = 1:2, scale = 1, pc.biplot = FALSE)

par(mfrow=c(1,2))
barplot(spr$loadings[,1],main="PC1 loadings")
barplot(spr$loadings[,2],main="PC2 loadings")

#Correlation matrix PCA
#correlation PCA
cov(x)
#first apply scale function
sx<-scale(x)
spr3 <-prcomp(sx)
U3<-spr3$rotation
L3<-(spr3$sdev)^2
Z3 <-spr3$x


##################################################################
##Clustering
##################################
library(MASS)
library(car)
data(iris)
attach(iris) 

########################################################################3
#k-means
##########################
k <- 3
kmeansobj<-kmeans(iris[1:4],k)
kmeansobj
pairs(iris[,1:4],col = c("red", "green3", "blue")[kmeansobj$cluster] )
pairs(iris[,1:4],col = c("red", "green3", "blue")[unclass(iris$Species)] )

########################################################
#Hierachical
##########################
d = dist(iris[1:4])
tree.avg = hclust(d, method="average")
plot(tree.avg)
membership <- cutree(tree.avg, k = 3)
pairs(iris[,1:4],col = c("red", "green3", "blue")[membership] )

library(cluster)
gap <- clusGap(iris[1:4], FUN = kmeans, K.max = 8)
plot(gap) #decide k?

k <- 2
kmeansobj1<-kmeans(iris[1:4],k)
kmeansobj1
pairs(iris[,1:4],col = c("red","blue")[kmeansobj1$cluster] )

library(mclust)
mixclust = Mclust(iris[,1:4],G=3)
summary(mixclust)
plot(mixclust)

##############################################################
#manova
################################
attach(iris)
iris.manova <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length,                            Petal.Width) ~ Species)
iris.manova

# Doing the MANOVA test with default method (i.e., Pillai's trace)
summary(iris.manova)
# Doing the MANOVA test using Wilks' Lambda:
summary(iris.manova, test="Wilks")
# Using the other test statistics:
summary(iris.manova, test="Roy")
summary(iris.manova, test="Hotelling-Lawley")

############################################################
#Cross - validation
############################################################
#k-fold Cross-validation (sample code)
k<-10
x<-c(1:150)
s<-sample(x,150,replace=FALSE)
mis<-rep(0,k)
for (i in 1:10){
  index<-seq((i-1)*length(x)/k+1,i*length(x)/k,by=1)
  train=x[-s[index]]
  z<-lda(Species ~.,iris[train,], subset=train)
  mis[i]<-sum(predict(z, iris[-train, ])$class!=iris$Species[-train])/length(iris$Species[-train])
}
mean(mis)  


