
# Display the file path
train <- read.csv("C:/Users/e1094772/Desktop/Housing Project.Final/train.csv")
#Where we are putting the dataset 
train <- read.table ("C:/Users/e1094772/Desktop/Housing Project.Final/train.csv", header=T, sep=",")
#Useful packages 
library(dplyr) ##dplyr is designed to abstract over how the data is stored for example if the data store is integer, character, etcc
library(caret) ## Misc functions for training and plotting classification and regression models.
library(ggplot2) ## data visualiaztion package
library(corrplot) ## A graphical display of a correlation matrix or general matrix. It also contains some algorithms to do matrix reordering. In addition,
##corrplot is good at details, including choosing color, text labels,color labels, layout, etc.


##Text Mining
##install.packages("tm")  # for text mining
##install.packages("SnowballC") # for text stemming
##install.packages("wordcloud") # word-cloud generator 
##install.packages("RColorBrewer") # color palettes

##Descriptive Statistics

#Display the number of rows and number of columns, respectively.
nrow(train)
ncol(train)
#List of all the variables in data set
names(train)
#List of string variables in data set
str(train)
#view the data set
View(train)
#Summary of data set
summary(train)



#Missing values from the data set
missing <- sapply(train, function(x) sum(is.na(x)))
missing
sum(is.na(train))

#Omitting insiginifcant variables from the data set
train[c("PoolQC", "Fence", "MiscFeature", "Alley", "FireplaceQu")] <- NULL

##Data Curation:
library(rpart)
#1. Using "rpart"(decision trees) to create the classification of variables to take care of the missing variables(NA)
#2. Classification trees for factor variables like garagequal /regression tress for numerical data like saleprice
#3. Recursive partitioning for classification and regression trees
#4. Creating a predictive model based on the data we have missing model
#5. To summarize, below code is telling us if a variable is NA, create a prediction model using rpart to fill in the values

model_GarageQual <- rpart(GarageQual ~ ., data = train,na.action = na.omit)
model_BsmtFinType2 <- rpart(BsmtFinType2 ~ .,data=train, na.action = na.omit)
model_MasVnrArea <- rpart(MasVnrArea ~ .,data=train, na.action = na.omit)
model_MasVnrType <- rpart(MasVnrType ~ .,data=train, na.action = na.omit)
model_GarageFinish <- rpart(GarageFinish ~ .,method = "class",data=train, na.action = na.omit)
model_Electrical <- rpart(Electrical ~ .,method = "class",data=train, na.action = na.omit)
model_BsmtFinType1 <- rpart(BsmtFinType1 ~ .,data=train, na.action = na.omit)
model_GarageYrBlt <- rpart(GarageYrBlt ~ .,data=train, na.action = na.omit)
model_BsmtExposure <- rpart(BsmtExposure ~ .,method = "class",data=train, na.action = na.omit)
model_GarageType <- rpart(GarageType ~ .,method = "class",data=train, na.action = na.omit)


model_BsmtCond <- rpart(BsmtQual ~ .,method = "class",data=train, na.action = na.omit)

model_GarageCond <- rpart(GarageCond ~ .,method = "class",data=train, na.action = na.omit)


model_LotFrontage <- rpart(LotFrontage ~ .,data=train, na.action = na.omit)
##~. everything data 

# 6. Below codes will take in the prediction made from above in rpart and create a decision tree model
# 7. Round is used for numerical variables and "class" method is used for factor variables to group variables in similar trees
# 8. The end result of below codes is rpart will fill in the NA's with the prediction made after running the decision trees various times
train$LotFrontage <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$LotFrontage[x]),round(predict(model_LotFrontage,train[x,])),train$LotFrontage[x])
}))
View(train)
train$GarageQual <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$GarageQual[x]),predict(model_GarageQual,train[x,]),train$GarageQual[x])
}))

train$BsmtFinType2 <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$BsmtFinType2[x]),predict(model_BsmtFinType2,train[x,]),train$BsmtFinType2[x])
}))

train$MasVnrArea <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$MasVnrArea[x]),round(predict(model_MasVnrArea,train[x,])),train$MasVnrArea[x])
}))

train$MasVnrType <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$MasVnrType[x]),predict(model_MasVnrType,train[x,]),train$MasVnrType[x])
}))

train$GarageFinish <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$GarageFinish[x]),predict(model_GarageFinish,train[x,]),train$GarageFinish[x])
}))

train$Electrical <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$Electrical[x]),predict(model_Electrical,train[x,]),train$Electrical[x])
}))

train$BsmtFinType1 <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$BsmtFinType1[x]),round(predict(model_BsmtFinType1,train[x,])),train$BsmtFinType1[x])
}))

train$GarageYrBlt <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$GarageYrBlt[x]),round(predict(model_GarageYrBlt,train[x,])),train$GarageYrBlt[x])
}))
train$BsmtExposure <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$BsmtExposure[x]),predict(model_BsmtExposure,train[x,]),train$BsmtExposure[x])
}))

train$GarageType <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$GarageType[x]),predict(model_GarageType,train[x,]),train$GarageType[x])
}))

train$BsmtCond <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$BsmtCond[x]),predict(model_BsmtCond,train[x,]),train$BsmtCond[x])
}))


train$GarageCond <- unlist(lapply(seq(1,nrow(train)), function(x){
  ifelse(is.na(train$GarageCond[x]),round(predict(model_GarageCond,train[x,])),train$GarageCond[x])
}))
View(train)

##Model Creation:
#1. Taking the train.csv data and splitting it into 80% for train and 20% for test

#2. 80% sample size for train data set
smp_size <- floor(0.80 * nrow(train))

#3. Setting the seed to make partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train_data <- train[train_ind, ]
test_data <- train[-train_ind, ]
# Summary of train and test data
nrow(train_data)
nrow(test_data)


# Using randomforest model to create the prediciton model for sale price
library(randomForest)
model <- randomForest(SalePrice ~., data = train_data, na.action=na.exclude, ntree = 1000)
# Get the R-squared of our model
model
# Get the most important variables in our analysis to predict the sales price
varImpPlot(model)
# Ranking of the most important variables
model$importance
# Get the predicted values for sale price in test data
preds <- predict(model, test_data)
preds
#Plot of predicted values in test data
plot(preds)

##Model Evaluation

#1. Evaluate our model on the test data
col_plot = ifelse(preds>test_data$SalePrice, "red", "blue")
#2. Plot of predicted  sale price
plot(preds, test_data$SalePrice, col = col_plot, pch = 16)
abline(0,1, col = "purple", lwd = 2, lty = 2)

#2. Residuals 
#3. Plot of how close did we get to the actual value provided in training data set. In other words, 
#    below code will show the difference between actual and predictor test data
plot(preds-test_data$SalePrice, col = col_plot, pch = 16, title = "Model Performance")
abline(0,0, col = "purple", lwd = 2, lty = 2)


##Reevaluate model parameter:

#1. Based on the evalauation, make some changes to the model we have if the results are unexpected
#2. Removing few other unimportant variables that were on bottom of the list from randomforest
#3. Transforming the data we have, we can also mess with the data split for training and test
#4. Changing the parameters in randomforest model, i.e., number of trees, nodes, size of bagging sample, etc..,

?randomForest
