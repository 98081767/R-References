#https://eight2late.wordpress.com/2017/02/07/a-gentle-introduction-to-support-vector-machines-using-r/
#Support Vector Machines
# - Useful for multiple class predictions
# - https://www.quora.com/What-is-the-difference-between-Linear-SVMs-and-Logistic-Regression

#set working directory if needed (modify path as needed)
setwd("C:/Users/arche/Documents/UTS/R-References/R-references-Git/R-References/Supervised (prediction)")
#load required library
library(e1071)
#load built-in iris dataset
data(iris)
#set seed to ensure reproducible results
set.seed(42)
#split into training and test sets
iris[,"train"] <- ifelse(runif(nrow(iris))<0.8,1,0)

#separate training and test sets
trainset <- iris[iris$train==1,]
testset <- iris[iris$train==0,]
#get column index of train flag
trainColNum <- grep("train",names(trainset))
#remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]
#get column index of predicted variable in dataset
typeColNum <- grep("Species",names(iris))

#build model – linear kernel and C-classification (soft margin) with default cost (C=1)
svm_model <- svm(Species~ ., data=trainset, method="C-classification", kernel="linear")
svm_model

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  1 
# gamma:  0.25 
# 
# Number of Support Vectors:  25

svm_model$SV


#training set predictions
# - pred_train has predicted class based on the model
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Species)
#[1] 0.9826087


#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Species)
#[1] 0.9142857


#The test prediction accuracy indicates that the linear performs quite well on this dataset, confirming that it is indeed near linearly separable. 
#Another point is that  we have used a soft-margin classification scheme with a cost C=1. You can experiment with this by explicitly changing the value of C. Again, I’ll leave this for you an exercise.
#The C classification (cost) is how many errors you want on the wrong side of the decision boundary.


#----------------------ANOTHER EXAMPLE WITH DIFFERENT kernal types eg. linear, radial 

#load required library (assuming e1071 is already loaded)
library(mlbench)
#load Sonar dataset
data(Sonar)
#set seed to ensure reproducible results
set.seed(42)
#split into training and test sets
Sonar[,"train"] <- ifelse(runif(nrow(Sonar))<0.8,1,0)
#separate training and test sets
trainset <- Sonar[Sonar$train==1,]
testset <- Sonar[Sonar$train==0,]
#get column index of train flag
trainColNum <- grep("train",names(trainset))
#remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]
#get column index of predicted variable in dataset
typeColNum <- grep("Class",names(Sonar))
#build model – linear kernel and C-classification with default cost (C=1)
svm_model <- svm(Class~ ., data=trainset, method="C-classification", kernel="linear")
#training set predictions
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Class)
#[1] 0.969697
#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Class)
#[1] 0.6046512

#The important point to note here is that the performance of the model with the test set is quite dismal compared to the previous case. 
#This simply indicates that the linear kernel is not appropriate here.  Let’s take a look at what happens if we use the RBF kernel with default values for the parameters:

#build model: radial kernel, default params
svm_model <- svm(Class~ ., data=trainset, method="C-classification", kernel="radial")
#print params
svm_model$cost
#[1] 1
svm_model$gamma
#[1] 0.01666667
#training set predictions
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Class)
#[1] 0.9878788
#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Class)
#[1] 0.7674419

# That’s a pretty decent improvement from the linear kernel. Let’s see if we can do better by doing some parameter tuning. To do this we first invoke tune.svm and use the parameters it gives us in the call to svm:

#find optimal parameters in a specified range
tune_out <- tune.svm(x=trainset[,-typeColNum],y=trainset[,typeColNum],gamma=10^(-3:3),cost=c(0.01,0.1,1,10,100,1000),kernel="radial")
#print best values of cost and gamma
tune_out$best.parameters$cost
#[1] 10
tune_out$best.parameters$gamma
#[1] 0.01
#build model
svm_model <- svm(Class~ ., data=trainset, method="C-classification", kernel="radial",cost=tune_out$best.parameters$cost,gamma=tune_out$best.parameters$gamma)
#training set predictions
pred_train <-predict(svm_model,trainset)
mean(pred_train==trainset$Class)
#[1] 1
#test set predictions
pred_test <-predict(svm_model,testset)
mean(pred_test==testset$Class)
#[1] 0.8139535