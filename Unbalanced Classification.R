#refer to https://www.analyticsvidhya.com/blog/2017/03/imbalanced-classification-problem/

# For highly unbalanced classification problem, conventional machine learning are not
# that effecitve anymore, because they are designed to improve the accuracy of the result
# while ignor the distribution of classes.


# classification accuracy is evaluated by confusion matrix which contains both actual class
# and predicted class.

#Then how to handle imbalabced dataset?

# There are two types of solutions: tranform data or choose techinique.

#1.data solution
# to balance the class in the data
# there are two directions to balance the data: increase the minority data proportion (oversample) 
# or decrease the majority data (under sample)
# To increase the minority data may lead to likelhood of overfitting, 
# while decreasing the majority data may lead to informaiton loss.
# Can also use cluster based oversample, k-means the data first then over sample in each cluster and 
# let each cluster have the same instances number.
# Better one:  Synthetic Minority oversampling technique (SMOTE) :
#A subset of data is taken from the minority class as an example and 
#then new synthetic similar instances are created. 
#These synthetic instances are then added to the original dataset. 

satisf<- read.csv("DM-Project/train.csv", header = TRUE)
satisf$TARGET <-  as.factor(satisf$TARGET)
summary(satisf$TARGET)
library(unbalanced)
output <- satisf$TARGET
input <- satisf[,-ncol(satisf)]
#use ubSMOTE in the package of unbalanced
data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
#Balanced Data#
TARGET = data$Y
balancedData<-cbind(data$X,TARGET)
table(balancedData$TARGET)


#2.Algorithm
#main idea: ensemble classifiers to improve the performance of single classifiers
#constructing several two-stage classifiers and then aggregate predictions

#bagging, boosting, adaptive boosting (Ada Boost):
#each iteration only focus on instances that are misclassified or hard to classify (update weight)
#each ietration use simple and weak classifier. However, this method is sensitive to noisy data.
#Gradient Tree Boosting: calculate loss in each iteration and use loss  build an improved learner.
#XG-boost (ExtremeGradient Tree Boosting):
#advanced and more efficient implementation of Gradient Boosting: faster,highly flexible, grow the tree to maximum depth then prune

#XG-boost
train <- sample(length(satisf),length(satisf)*0.9)
trainData <- satisf[train,]
testData <- satisf[-train,]
library(xgboost)

#parameters
#objective = "binary:logistic": we will train a binary classification model ;
#max_depth = 2: the trees won’t be deep, because our case is very simple ;
#nthread = 2: the number of cpu threads we are going to use;
#nrounds = 2: there will be two passes on the data, the second one will enhance the model by further reducing the difference between ground truth and prediction.
labels = trainData$TARGET
df_train = trainData[,-371]
xgb <- xgboost(data = data.matrix(df_train), 
               label = labels, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)
#probability
y_pred <- predict(xgb, data.matrix(testData[,-371]))

