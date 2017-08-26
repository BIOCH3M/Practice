#library
library(tidyverse)
library(e1071)
library(MASS)
library(tree)
library(randomForest)
library(gbm)
library(dplyr)
library(nnet)
#bring in data
data<-read.csv("/Users/asurin/Documents/GITHUB_PUBLIC/HR_comma_sep.csv")
names(data)
refcols <- c("left")
#
data <- data[, c(setdiff(names(data), refcols), refcols )]
names(data)

#look for categorical variables... I will convert them to factor
head(data)
# data$sales<-as.factor(data$sales)
# data$salary<-as.factor(data$salary)
# data$left<-as.factor(data$left)
# data$number_project<-as.factor(data$number_project)
data$salary<-as.numeric(factor(data$salary))
data$sales<-as.numeric(factor(data$sales))  
data<-as.data.frame(apply(data, 2, function(x){
  as.numeric(x)
}))
#Check for NA values
nrow(data)
data <- na.omit(data)
nrow(data)#same as before...so no NA values in the dataset
pairs(data)
#create test and training sets
set.seed(1)
train <- sample(1:nrow(data),.8*nrow(data))
training <- data[train,]
#final number of train set
dim(training)
#select all data which is not train for test
test <- data[-train,]
#final number of test set
dim(test)

#test records 'left' column
actual.left<-test$left

##########################LOG

#Log---using binomial since it is a Yes or No category
glm.fit<-glm(left~., family=binomial, data=training)
summary(glm.fit)

glm.prob<-predict(glm.fit, test,type = "response")
#####Confusion Matrix
#assign "No"(0) to all test records
glm.pred=rep(0,3000)
#now substitue "Yes"(1) for all varialbes which were over .5
glm.pred[glm.prob>.5]= 1
#finally create confusion matrix
table(glm.pred,actual.left)
#calculate test error
glm.fit.err<-mean(glm.pred!=actual.left)
#actually right
mean(glm.pred==actual.left)

##########################LDA

lda.fit<-lda(left~., data=training)
lda.pred<-predict(lda.fit, test)
lda.class<-lda.pred$class
table(lda.class, actual.left)
#Error
lda.fit.err<-mean(lda.class !=actual.left)
lda.fit.err

##########################QDA

qda.fit<-qda(left~., data=training)
qda.pred<-predict(qda.fit, test)
qda.class<-qda.pred$class
table(qda.class, actual.left)
#Error
qda.fit.err<-mean(qda.class !=actual.left)
qda.fit.err


########################## Classification TREES
tree.fit<-tree(as.factor(left)~., data=training)
summary(tree.fit)
plot(tree.fit)
text(tree.fit,pretty = 0)
#look at the ERROR
tree.pred<-predict(tree.fit,test, type="class")
table(tree.pred, actual.left)
#Error
tree.fit.err<-mean(tree.pred !=actual.left)
tree.fit.err

#Use cross-validation to determine optimal tree complexity
cv.tree.fit<-cv.tree(tree.fit)
names(cv.tree.fit)
par(mfrow=c(1,2))
#you can choose the tree with more pruned see if it helps by this graph
plot(cv.tree.fit$size,cv.tree.fit$dev, xlim = c(0,15))
prune.tree.fit<-prune.tree(tree.fit,best=9)
plot(prune.tree.fit)
text(prune.tree.fit,pretty = 0)
#look at the ERROR
prune.tree.pred<-predict(prune.tree.fit,test, type="class")
table(prune.tree.pred, actual.left)
#Error
prune.tree.fit.err<-mean(prune.tree.pred !=actual.left)
prune.tree.fit.err

######################Bagging Tree

set.seed(1)
bag.tree.fit<-randomForest(as.factor(left)~.,data =training, mtry=9,importance=TRUE)
bag.tree.fit
#look at the ERROR
bag.tree.pred<-predict(bag.tree.fit,test, type="class")
table(bag.tree.pred, actual.left)
#Error
bag.tree.fit.err<-mean(bag.tree.pred !=actual.left)
bag.tree.fit.err


#change numbeer of tree argument
bag.tree.fit.2<-randomForest(as.factor(left)~.,data =training, mtry=9,importance=TRUE, ntree=25)
bag.tree.fit.2
#look at the ERROR
bag.tree.pred.2<-predict(bag.tree.fit.2,test, type="class")
table(bag.tree.pred.2, actual.left)
#Error
bag.tree.fit.2.err<-mean(bag.tree.pred.2 !=actual.left)
bag.tree.fit.2.err

######################BOOSTING

#mutate left training to 0 and 1
#training<-mutate(training, left.Binary=(ifelse(left=="Yes", 1, 0)))
#run boost
boost.tree.fit<-gbm(left~., data=training, distribution="bernoulli", n.trees = 500, interaction.depth=2)
summary(boost.tree.fit)

#look at the ERROR
boost.tree.pred<-round(predict(boost.tree.fit,test, type="response", n.trees=500))
table(boost.tree.pred, actual.left)
#Error
boost.tree.fit.err<-1-((2260)/(740+2260))
boost.tree.fit.err

#Alternate shrinkage
boost.tree.fit.2<-gbm(left ~.,data=training, distribution="bernoulli", n.trees = 5000, interaction.depth=6,shrinkage = .2)
summary(boost.tree.fit.2)

#look at the ERROR
boost.tree.pred.2<-round(predict(boost.tree.fit.2,test, type="response", n.trees=5000))
table(boost.tree.pred.2, actual.left)
#Error
boost.tree.fit.err.2<-1-((2243+717)/(2243+23+17+717))
boost.tree.fit.err.2

######################################NEURAL NETWORKS

#SINGLE LAYER...
#single layer
ns <- nnet(left ~ .,data=training,size=5,linout=FALSE,decay=5e-4)

#Predict
pr.ns <- predict(ns,test[,1:9])
pr.nn_2 <- pr.ns*sd(data$left) + mean(data$left)
mse.ns <- sum((test$left - pr.nn_2)^2)/nrow(pr.nn_2)
mse.ns

##############################
models<-c("LOG REG","LDA","QDA","CLASS. TREE", "PRUNED TREE","BAGGING.1","BAGGING.2",
          "BOOSTING.1","BOOSTING.2","SINGLE-LAYER_NN")

mse.result<-c(glm.fit.err,lda.fit.err,qda.fit.err,tree.fit.err,prune.tree.fit.err,bag.tree.fit.err,bag.tree.fit.2.err,
              boost.tree.fit.err,boost.tree.fit.err.2,mse.ns )
try<-data.frame(models, mse.result)




