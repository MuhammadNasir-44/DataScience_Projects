---
title: "GH-Multiple Regression"
---

  
#Step1: Install.packages("caret")####
library(caret)
library(corrplot)
library(readr)
library(e1071)
library(randomForest)
library(kernlab)
library(fastDummies) 



setwd("/Users/Muhammad Nasir/youfilepathhere")

#Step2: Pre processing, Data Cleansing, Data Wrangling, View}####
data1 <- read.csv("existingproductattributes2017.csv", stringsAsFactors = F, header = T)
data1<-dummy_cols(data1,select_columns = "ProductType")
View(data1)
str(data1)
data1$BestSellersRank <- NULL 
data1$x5StarReviews <- NULL
data1$x3StarReviews <- NULL
data1$x1StarReviews  <- NULL
data1$ProductNum <- NULL
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{Step 3: Correlation}
#Step3  Correlation####
new_data <- data.frame(data1$Price, data1$NegativeServiceReview, data1$ProfitMargin, data1$Volume)
View(new_data)
Correlation <- cor(new_data)
corrplot(Correlation)

#Step 4: Create Data Participation, Training and Testing}####
set.seed(321)
testSize<-nrow(data1)-trainSize
trainSize
testSize
training_indices<-sample(seq_len(nrow(data1)), size = trainSize)
trainSet<-data1[training_indices,]
testSet<- data1[-training_indices,]


#Step 5: Model Implementation, Result and Plotting}####

#LinearModel###
Linmod<-lm(trainSet$Volume ~. ,data = trainSet)
summary(Linmod)

#kNN###
ct1<- trainControl(method = "repeatedcv", number = 10, repeats = 2)
set.seed(132)
knn_fit<-knn3(Volume~.,data = trainSet,tuneLength = 10,trControl=ct1)
knn_fit
summary(knn_fit)
p1<- predict(knn_fit, newdata = testSet)
str(p1)
plot(knn_fit)
#RandomForest###
ct<- trainControl(method = "repeatedcv", number = 10, repeats = 2)
set.seed(123)
RF <- randomForest(Volume ~ .,
                   data = trainSet,
                   method = "rf",
                   tunelength=15,
                   trControl=ct,
                   preProc = c("center", "scale"))
RF
summary(RF)
p2<- predict(RF, newdata = testSet)
p2
plot(RF)
#Support Vector Machine (SVM)
ct2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
svm_model <- svm(Volume ~., data = trainSet, 
                 method = "svmLinear",
                 tuneLength = 10,
                 preProcess = c("center", "scale"))
svm_model
summary(svm_model)
p3<- predict(svm, newdata = testSet)
p3
plot(p3)



###End####
