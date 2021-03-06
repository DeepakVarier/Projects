
# Problem Statement:
# Classify the species based on the flower attributes

# Read in the data
IrisData = read.table(file.choose(),header=FALSE, sep=",")

str(IrisData)

# change column names for the data
colnames(IrisData) <- c("Sepal Length","Sepal Width","Petal Length","Petal Width","Species")

str(IrisData)

# each variable plotted against others
pairs(IrisData[1:4], main = "Iris Data (red=setosa,green=versicolor,blue=virginica)",
      pch = 21, bg = c("red", "green3", "blue")[unclass(IrisData$Species)])

summary(IrisData)
names(IrisData)
table(IrisData$Species)

# Library to Split the data into training and test data sets
library(caTools)

#Set seed so that split is same always
set.seed(88)

spl = sample.split(IrisData$V5, SplitRatio = 0.6)
spl

# Train Data Set
IrisTrain = subset(IrisData, spl==TRUE)

# Test Data set
IrisTest = subset(IrisData, spl==FALSE)

#klaR package contains Naive Bayes classifier ... e1071 package also will work
install.packages("klaR")

#Classification and REgression Training package
install.packages("caret")


library("klaR")
library("caret")

install.packages("quantreg")
library(quantreg)

#Required for loading caret library
install.packages("minqa")
library(minqa)

install.packages('e1071', dependencies = TRUE)
library(e1071)

#generate a Naive Bayes model, using 10-fold cross-validation
model = train(IrisTrain[,-5],IrisTrain$Species,'nb',trControl=trainControl(method='cv',number=10))

model
#Naive Bayes 

#90 samples
# 4 predictor
# 3 classes: 'Iris-setosa', 'Iris-versicolor', 'Iris-virginica' 

#No pre-processing
#Resampling: Cross-Validated (10 fold) 
#Summary of sample sizes: 81, 81, 81, 81, 81, 81, ... 
#Resampling results across tuning parameters:

#  usekernel  Accuracy   Kappa
#  FALSE      0.9666667  0.95 
#   TRUE      0.9666667  0.95 

#Tuning parameter 'fL' was held constant at a value of 0
#Tuning parameter 'adjust' was held
# constant at a value of 1
#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model were fL = 0, usekernel = FALSE and adjust = 1. 

#Prediction function
predict(model$finalModel,IrisTest[,-5])

#This will printout a bunch of lines. Near the top you can see the classes it predicted, 
#then you will see the posterior probabilities in the bottom half. 
#As we are only interested in the class predictions, we can grab only those with the 
#following line.

predict(model$finalModel,IrisTest[,-5])$
  
#Confusion Matrix to visuallize the classification error
table(predict(model$finalModel,IrisTest[,-5])$class,IrisTest$Species)
