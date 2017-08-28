#library
library(tidyverse)
library(e1071)
library(MASS)
library(tree)
library(randomForest)
library(gbm)
library(dplyr)
library(nnet)
library(highcharter)
#bring in data


data<-read.csv("/Users/asurin/Documents/GITHUB_PUBLIC/HR_comma_sep.csv")
names(data)
refcols <- c("left")
#bringing respnonse variable 'left' to the end of the dataframe
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


##########################################################################################

data.2<-read.csv("/Users/asurin/Documents/GITHUB_PUBLIC/HR_comma_sep.csv")
names(data.2)
refcols <- c("left")
#bringing respnonse variable 'left' to the end of the data.2frame
data.2 <- data.2[, c(setdiff(names(data.2), refcols), refcols )]
names(data.2)
summary(data.2)

#look for categorical variables... I will convert them to factor
head(data.2)
# data.2$sales<-as.factor(data.2$sales)
# data.2$salary<-as.factor(data.2$salary)
# data.2$left<-as.factor(data.2$left)
# data.2$number_project<-as.factor(data.2$number_project)
data.2$salary<-as.numeric(factor(data.2$salary))
data.2$sales<-as.numeric(factor(data.2$sales))  
data.2<-as.data.frame(apply(data.2, 2, function(x){
  as.numeric(x)
}))
#Check for NA values
nrow(data.2)
data.2 <- na.omit(data.2)
nrow(data.2)#same as before...so no NA values in the data.2set
pairs(data.2)
#create test.2 and training.2 sets
set.seed(1)
train <- sample(1:nrow(data.2),.8*nrow(data.2))
training.2 <- data.2[train,]
#final number of train set
dim(training.2)
#select all data.2 which is not train for test.2
test.2 <- data.2[-train,]
#final number of test.2 set
dim(test.2)

#test.2 records 'left' column
actual.left.2<-test.2$left

##########################LOG

#Log---using binomial since it is a Yes or No category
glm.fit.good<-glm(left~., family=binomial, data =training.2)
summary(glm.fit.good)

glm.prob.good<-predict(glm.fit.good, test.2,type = "response")
#####Confusion Matrix
#assign "No"(0) to all test.2 records
glm.pred.good=rep(0,3000)
#now substitue "Yes"(1) for all varialbes which were over .5
glm.pred.good[glm.prob.good>.5]= 1
#finally create confusion matrix
table(glm.pred.good,actual.left.2)
#calculate test.2 error
glm.fit.err.good<-mean(glm.pred.good!=actual.left.2)
#actually right
mean(glm.pred.good==actual.left.2)

##########################LDA

lda.fit.good<-lda(left~., data=training.2)
lda.pred.good<-predict(lda.fit.good, test.2)
lda.class.good<-lda.pred.good$class
table(lda.class.good, actual.left.2)
#Error
lda.fit.err.good<-mean(lda.class.good !=actual.left.2)
lda.fit.err.good

##########################QDA

qda.fit.good<-qda(left~., data=training.2)
qda.pred.good<-predict(qda.fit.good, test.2)
qda.class.good<-qda.pred.good$class
table(qda.class.good, actual.left.2)
#Error
qda.fit.err.good<-mean(qda.class.good !=actual.left.2)
qda.fit.err.good


########################## Classification TREES
tree.fit.good<-tree(as.factor(left)~., data=training.2)
summary(tree.fit.good)
plot(tree.fit.good)
text(tree.fit.good,pretty = 0)
#look at the ERROR
tree.pred.good<-predict(tree.fit.good,test.2, type="class")
table(tree.pred.good, actual.left.2)
#Error
tree.fit.err.good<-mean(tree.pred.good !=actual.left.2)
tree.fit.err.good

#Use cross-validation to determine optimal tree complexity
cv.tree.fit.good<-cv.tree(tree.fit.good)
names(cv.tree.fit.good)
par(mfrow=c(1,2))
#you can choose the tree with more pruned see if it helps by this graph
plot(cv.tree.fit.good$size,cv.tree.fit.good$dev, xlim = c(0,15))
prune.tree.fit.good<-prune.tree(tree.fit.good,best=9)
plot(prune.tree.fit.good)
text(prune.tree.fit.good,pretty = 0)
#look at the ERROR
prune.tree.pred.good<-predict(prune.tree.fit.good,test.2, type="class")
table(prune.tree.pred.good, actual.left.2)
#Error
prune.tree.fit.err.good<-mean(prune.tree.pred.good !=actual.left.2)
prune.tree.fit.err.good

######################Bagging Tree

set.seed(1)
bag.tree.fit.good<-randomForest(as.factor(left)~.,data=training.2, mtry=9,importance=TRUE)
bag.tree.fit.good
#look at the ERROR
bag.tree.pred.good<-predict(bag.tree.fit.good,test.2, type="class")
table(bag.tree.pred.good, actual.left.2)
#Error
bag.tree.fit.err.good<-mean(bag.tree.pred.good !=actual.left.2)
bag.tree.fit.err.good


#change numbeer of tree argument
bag.tree.fit.2.good<-randomForest(as.factor(left)~.,data=training.2, mtry=9,importance=TRUE, ntree=25)
bag.tree.fit.2.good
#look at the ERROR
bag.tree.pred.2.good<-predict(bag.tree.fit.2.good,test.2, type="class")
table(bag.tree.pred.2.good, actual.left.2)
#Error
bag.tree.fit.2.err.good<-mean(bag.tree.pred.2.good !=actual.left.2)
bag.tree.fit.2.err.good

######################BOOSTING

#mutate left training.2 to 0 and 1
#training.2<-mutate(training.2, left.Binary=(ifelse(left=="Yes", 1, 0)))
#run boost
boost.tree.fit.good<-gbm(left~., data=training.2, distribution="bernoulli", n.trees = 500, interaction.depth=2)
summary(boost.tree.fit.good)

#look at the ERROR
boost.tree.pred.good<-round(predict(boost.tree.fit.good,test.2, type="response", n.trees=500))
table(boost.tree.pred.good, actual.left.2)
#Error
boost.tree.fit.err.good<-1-((2260)/(740+2260))
boost.tree.fit.err.good

#Alternate shrinkage
boost.tree.fit.2.good<-gbm(left ~.,data=training.2, distribution="bernoulli", n.trees = 5000, interaction.depth=6,shrinkage = .2)
summary(boost.tree.fit.2.good)

#look at the ERROR
boost.tree.pred.2.good<-round(predict(boost.tree.fit.2.good,test.2, type="response", n.trees=5000))
table(boost.tree.pred.2.good, actual.left.2)
#Error
boost.tree.fit.err.2.good<-1-((2243+717)/(2243+23+17+717))
boost.tree.fit.err.2.good

######################################NEURAL NETWORKS

#SINGLE LAYER...
#single layer
ns <- nnet(left ~ .,data=training.2,size=5,linout=FALSE,decay=5e-4)

#Predict
pr.ns.good <- predict(ns,test.2[,1:9])
pr.nn_2.good <- pr.ns.good*sd(data.2$left) + mean(data.2$left)
mse.ns.good <- sum((test.2$left - pr.nn_2.good)^2)/nrow(pr.nn_2.good)
mse.ns.good

##############################
models.good<-c("LOG REG","LDA","QDA","CLASS. TREE", "PRUNED TREE","BAGGING.1","BAGGING.2",
          "BOOSTING.1","BOOSTING.2","SINGLE-LAYER_NN")

mse.result.good<-c(glm.fit.err.good,lda.fit.err.good,qda.fit.err.good,tree.fit.err.good,prune.tree.fit.err.good,
              bag.tree.fit.err.good,bag.tree.fit.2.err.good,
              boost.tree.fit.err.good,boost.tree.fit.err.2.good,mse.ns.good )
try.good<-data.frame(models.good, mse.result.good)

