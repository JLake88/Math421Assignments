

```R
---
title: "Titanic Data Set Analysis"
author: "Josh Lake"
date: "September 12, 2018"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
```
```{r include = FALSE}
Titanic_Dataset <- read.csv("~/titanic.csv")
Titanic_Dataset$Survived = as.factor(Titanic_Dataset$Survived)
Titanic_Dataset$Pclass = as.factor(Titanic_Dataset$Pclass)
```
# Passenger Description Information
## Plot 1: Distribution of Passenger Age
```{r echo= FALSE, warning= FALSE}
qplot(Age, data = Titanic_Dataset, geom = "histogram", binwidth = 10)
```

## Plot 2: Distribtuion of Ticket Prices
```{r echo= FALSE, warning= FALSE}
qplot(Fare, data = Titanic_Dataset, geom = "histogram", binwidth = 40)
```

## Plot 3: Ticket Price Plotted Against Age
```{r echo= FALSE, warning= FALSE}
qplot(Age, Fare, data = Titanic_Dataset)
```

## Plot 4: Pclass Plotted Against Age
```{r echo= FALSE, warning= FALSE}
qplot(Pclass, Age, data = Titanic_Dataset, geom = "boxplot")
```

## Plot 5: Pclass Plotted Against Fare
```{r echo= FALSE, warning= FALSE}
qplot(Pclass, Fare, data = Titanic_Dataset, geom = "boxplot")
```

# Passenger Survival Information
## Plot 6: Count of Survival Rates, By Passenger Sex
```{r echo= FALSE, warning= FALSE}
qplot(Survived, data = Titanic_Dataset, geom = "bar", fill = Sex)
```

## Plot 7: Count of Survival Rates, By Passenger Class
```{r echo= FALSE, warning= FALSE}
qplot(Survived, data = Titanic_Dataset, geom = "bar", fill = Pclass)
```

## Plot 8: Count of Survival Rates, By Port of Embarkment
```{r echo= FALSE, warning= FALSE}
qplot(Survived, data = Titanic_Dataset, geom = "bar", fill = Embarked)
```
```


    Error in parse(text = x, srcfile = src): attempt to use zero-length variable name
    Traceback:
    



```R
---
title: "Assignment 3"
author: "Josh Lake"
date: "September 19, 2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Data Manipulation with R
## Load the dataset, check for missing and remove unnecessary columns/rows with missing categorical data
```{r}
Titanic = read.csv("titanic.csv")
Titanic$Survived = factor(Titanic$Survived)
Titanic$Pclass = factor(Titanic$Pclass)
sum(is.na(Titanic))
T_Update = subset(Titanic, select = -c(Name,SibSp,Parch,Ticket,Cabin))
T_Update_2 = T_Update[complete.cases(T_Update), ]
```

## Replace missing numerical values with column averages
```{r}
Mage <- mean(T_Update_2$Age, na.rm = TRUE)
T_Update_2[is.na(T_Update_2$Age),]$Age == Mage
Mfare <- mean(T_Update_2$Fare, na.rm = TRUE)
T_Update_2[is.na(T_Update_2$Fare),]$Fare == Mfare
```

## Calculate select means
```{r}
mean(T_Update_2$Age)
mean(T_Update_2[T_Update_2$Sex == "female",]$Age)
mean(T_Update_2[T_Update_2$Sex == "female" & T_Update_2$Age >= "13" & T_Update_2$Age < "20",]$Fare)
mean(T_Update_2[T_Update_2$Sex == "female" & T_Update_2$Age >= "13" & T_Update_2$Age < "20" & T_Update_2$Survived == "1" & T_Update_2$Pclass == "1",]$Fare)
mean(T_Update_2[T_Update_2$Sex == "female" & T_Update_2$Age >= "13" & T_Update_2$Age < "20" & T_Update_2$Survived == "1" & T_Update_2$Pclass == "2",]$Fare)
mean(T_Update_2[T_Update_2$Sex == "female" & T_Update_2$Age >= "13" & T_Update_2$Age < "20" & T_Update_2$Survived == "1" & T_Update_2$Pclass == "3",]$Fare)
```

## Calculate select medians
```{r}
median(T_Update_2[T_Update_2$Pclass == "1",]$Fare)
median(T_Update_2[T_Update_2$Pclass != "1" & T_Update_2$Sex == "female",]$Fare)
median(T_Update_2[T_Update_2$Pclass != "3" & T_Update_2$Sex == "female" & T_Update_2$Survived == "1",]$Age)
```

## Calculate select ratios
```{r}
Mfare = mean(T_Update_2$Fare)
T_Update_2a = T_Update_2[T_Update_2$Fare>Mfare,]
prop.table(table(T_Update_2a$Survived))
```

## Add new variables
```{r}
T_Update_2$sfare <- (T_Update_2$Fare - Mfare)/sd(T_Update_2$Fare)
T_Update_2$cfare [T_Update_2$Fare < Mfare]<- "cheap"
T_Update_2$cfare [T_Update_2$Fare > Mfare]<- "expensive"
T_Update_2$cage [T_Update_2$Age >= 0 & T_Update_2$Age < 10] <- "0"
T_Update_2$cage [T_Update_2$Age >= 10 & T_Update_2$Age < 20] <- "1"
T_Update_2$cage [T_Update_2$Age >= 20 & T_Update_2$Age < 30] <- "2"
T_Update_2$cage [T_Update_2$Age >= 30 & T_Update_2$Age < 40] <- "3"
T_Update_2$cage [T_Update_2$Age >= 40 & T_Update_2$Age < 50] <- "4"
T_Update_2$cage [T_Update_2$Age >= 50 & T_Update_2$Age < 60] <- "5"
T_Update_2$cage [T_Update_2$Age >= 60 & T_Update_2$Age < 70] <- "6"
T_Update_2$cage [T_Update_2$Age >= 70 & T_Update_2$Age < 80] <- "7"
T_Update_2$cage [T_Update_2$Age >= 80 & T_Update_2$Age < 90] <- "8"
T_Update_2$cage [T_Update_2$Age >= 90 & T_Update_2$Age < 100] <- "9"
```

## Select plots
```{r}
library(ggplot2)
youngPass <- subset(T_Update_2, Age < Mage)
ggplot(youngPass, aes(Survived)) + geom_bar(aes(fill = youngPass$Sex))
lowFare <- subset(T_Update_2, Fare > Mfare & Survived == "1")
ggplot(lowFare, aes(Age)) + geom_density()
ggplot(T_Update_2, aes(Embarked == "S" | Embarked =="Q")) + geom_density()
ggplot(T_Update_2, aes(cage)) + geom_bar(aes(fill = T_Update_2$Survived)) + facet_grid(~ Pclass)
```
```


    Error in parse(text = x, srcfile = src): attempt to use zero-length variable name
    Traceback:
    



```R
---
title: "Assignment 4_Submission"
author: "Josh Lake"
date: "September 20, 2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Question 1
```{r}
function1 = function (x)
{
  return(x^2 + 3*x + 1)
}
```

## Question 2
```{r}
prodvector = function (v)
{
  return(prod(v))
}
```

## Question 3
```{r}
customreplace = function (v, n)
{
  replace(v,is.na(v), n)
}
```

## Question 4
```{r}
meanreplace = function (v)
{
  replace(v,is.na(v), mean(v, na.rm = TRUE))
}
```

## Question 5
```{r}
dfreplace = function (df)
{
  for(i in 1:ncol(df))
  {
    if(is.numeric(df[,i])) 
       {df[is.na(df[,i]), i] <- mean(df[,i], na.rm = TRUE)}
  }
  return(df)
}
```

## Question 6
```{r}
custombar = function (df, column)
{
  ggplot(df, aes(column)) + geom_bar()
}
```

## Question 7
```{r}
custombar2 = function (df, column1, column2)
{
  ggplot(df, aes(column1)) + geom_bar(aes(fill = column2))
}
```
```


    Error in parse(text = x, srcfile = src): attempt to use zero-length variable name
    Traceback:
    



```R
---
title: "Assignment 9_Submission"
author: "Josh Lake"
date: "September 26, 2018"
output: word_document
---

```{r setup}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
```

##Question 1
```{r}
q1a = function(df)
{
  sum(is.na(df))
}
colrep = function(df)
{
    apply(df, 2, q1a)
}
```

##Question 2
```{r}
colprint = function(df)
{
  for(i in 1:ncol(df))
  {
    if(sum(is.na(df[i]!=0))) print(names(df[i]))
  }
}
```

##Question 3
```{r}
colprint2 = function(df)
{
    for(i in 1:ncol(df))
  {
    if(sum(is.na(df[i]!=0))) 
        {
          print(names(df[i]))
          print(sum(is.na(df[,i])))
        }
  }
}
```

##Question 4
```{r}
question4 = function(x)
{
  for(i in 1:ncol(x))
  {
    if(sum(is.na(x[,i]))!=0 & class(x[[i]]) == "numeric")
    {
      x[,i][is.na(x[,i])] = mean(x[[i]], na.rm = TRUE)
    }

    else
    {
      x[,i][is.na(x[,i])] = which.max(table(a9[,i]))
    }
  }
  return(x)
}
```

##Question 5 
```{r}
sub1 = function(x)
{
  y= NULL
  z= NULL
  for(i in 1:ncol(x))
  {
    if((class(x[[i]])) == "numeric")
    {
      y = cbind(y, x[[i]])
    }
    else
    {
      z = cbind(z, x[[i]])
    }
  }
  print(y)
  print(z)
}
```

##Question 6
```{r}
den= function(x)
{
  for(i in 1:ncol(x))
  {
    if((class(x[[i]])) == "numeric")
    {
      print((ggplot(x, aes(x[[i]])) + geom_density()))
    }
  }
}
```

##Question 7
```{r}
bar= function(x)
{
  for(i in 1:ncol(x))
  {
    if((class(x[[i]])) != "numeric")
    {
      print(ggplot(x, aes(x[[i]])) + geom_bar())
    }
  }
}
```

##Question 8
```{r}
bar2= function(x, i, j)
{
  if(class(x[[i]]) != "numeric" & class(x[[j]]) != "numeric")
  {
    print(ggplot(x, aes(x[[i]])) + geom_bar())
    print(ggplot(x, aes(x[[j]])) + geom_bar())
  } else {
    print("Either i or j is not a categorical variables")
  }
}
```

##Question 9
```{r}
scatter= function(x, i, j)
{
  if(class(x[[i]]) == "numeric" & class(x[[j]]) == "numeric")
  {
    print(ggplot(x, aes(x = x[[i]], y = x[[j]])) + geom_point())
  } else {
    print("Either i or j is not a numeric variables")
  }
}
```

##Question 10
```{r}
den2 = function(x, i, j)
{
  if(class(x[[i]]) == "numeric" & class(x[[j]]) != "numeric")
  {
    print(ggplot(x, aes(x[[i]])) + geom_density() + facet_wrap(x[[j]]))
  } else {
    print("Variable classes not suitable for density plot, i must be numeric and j must be categorical")
  }
}
```

##Question 11
```{r}
allbar = function(x)
{
  for(i in 1:ncol(x))
    {
      for(j in 1:ncol(x))
      {
        if(class(x[[i]]) == "factor")
        {
        if(class(x[[j]]) == "factor")
        {
          print(ggplot(x, aes(x[[i]])) + geom_bar(aes(fill = x[[j]]), position = "dodge"))
        }
      }
    }
  }
}
```

##Question 12
```{r}
#See code for question 6
```

##Question 13
```{r}
visual= function(x, i, j)
{
  for(i in 1:ncol(x))
  {
    if((class(x[[i]])) == "numeric")
    {
      print((ggplot(x, aes(x[[i]])) + geom_density()))
    }
  }
  for(i in 1:ncol(x))
  {
    if((class(x[[i]])) != "numeric")
    {
      print(ggplot(x, aes(x[[i]])) + geom_bar())
    }
  }
  for(i in 1:ncol(x))
    {
      for(j in 1:ncol(x))
      {
        if(class(x[[i]]) == "factor")
        {
        if(class(x[[j]]) == "factor")
        {
          print(ggplot(x, aes(x[[i]])) + geom_bar(aes(fill = x[[j]]), position = "dodge"))
        }
      }
    }
  }
}
```
```


    Error in parse(text = x, srcfile = src): attempt to use zero-length variable name
    Traceback:
    



```R
---
title: "Assignment 10"
author: "Josh Lake"
date: "October 15, 2018"
output:
  word_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(caret)
library(rpart)
library(rattle)
```

##Bring in adult dataset and clean data
```{r}
df <- read.csv("adult.csv", header = FALSE, na.strings = c(" ?", "NA", "."))
names(df) = c("age", "workclass", "fnlwgt", "education", "educationNum", "maritalStatus", "occupation", "relationship", "race", "sex", "capitalGain", "capitalLoss", "hoursPerWeek", "nativeCountry", "target")
df$age <- as.numeric(df$age)
df$workclass <- as.factor(df$workclass)
df$fnlwgt <- as.numeric(df$fnlwgt)
df$education <- as.factor(df$education)
df$educationNum <- as.numeric(df$educationNum)
df$maritalStatus <- as.factor(df$maritalStatus)
df$occupation <- as.factor(df$occupation)
df$relationship <- as.factor(df$relationship)
df$race <- as.factor(df$race)
df$sex <- as.factor(df$sex)
df$capitalGain <- as.numeric(df$capitalGain)
df$capitalLoss <- as.numeric(df$capitalLoss)
df$hoursPerWeek <- as.numeric(df$hoursPerWeek)
df$nativeCountry <- as.factor(df$nativeCountry)
```

##Replace missing variables
```{r}
repnum = function(x){
  for(i in 1:ncol(x)){
    if(is.numeric(x[,i])){
     x[is.na(x[,i]), i] <- mean(x[,i], na.rm = TRUE)
    }
  }
  return(x)
}
repnum(df)

repcat = function(x){
  for(i in 1:ncol(x)){
    if(is.factor(x[,i])){
     x[,i][is.na(x[,i])] = levels(x[,i])[which.max(table(x[,i]))]
    }
  }
  return(x)
}
repcat(df)
```

##Data visualization
```{r}
ggplot(df, aes(workclass)) + geom_bar(aes(fill = sex))
ggplot(df, aes(race)) + geom_bar(aes(fill = sex))
ggplot(df, aes(age, colour = sex)) + geom_density()
ggplot(df, aes(age, colour = race)) + geom_density()
ggplot(df, aes(age, colour = education)) + geom_density()
ggplot(df, aes(hoursPerWeek, colour = sex)) + geom_density()
ggplot(df, aes(hoursPerWeek, colour = race)) + geom_density()
```

##Model Building: rpart/Rattle
```{r}
splitIndex <- createDataPartition(df$target, p = .60, list = FALSE, times = 1)
train <- df[ splitIndex,]
test <- df[-splitIndex,]
dftree <- rpart(target ~ ., data = train, method = "class")
fancyRpartPlot(dftree)
dfpred <- predict(dftree,test, type = "class")
cm=confusionMatrix(data = dfpred, reference = test$target, positive = " >50K")
cm
```

##Model Building: caret
```{r}
#split<-createDataPartition(y = df$target, p = 0.6, list = FALSE)
#train2 <- df[split,]
#test2 <- df[-split,]
#Fit <- train(
#  target ~ .,
#  data = train,
#  method = "pls",
#  preProc = c("center", "scale"),
#  tuneLength = 15,
#  trControl = ctrl,
#  metric = "ROC"
#)
#Classes <- predict(Fit, newdata = test)
#confusionMatrix(data = Classes, test$target)
```

##Model Building Function
```{r}
buildModel = function( dfNameStr, s){
  splitIndex <- createDataPartition(df$target, p = s, list = FALSE, times = 1)
  train <- df[ splitIndex,]
  test <- df[-splitIndex,]
  tree <- rpart(target ~ ., data = train, method = "class")
  fancyRpartPlot(tree)
}
buildModel(df, .7)
buildModel(df, .8)
buildModel(df, .9)
```
```


    Error in parse(text = x, srcfile = src): attempt to use zero-length variable name
    Traceback:
    



```R
---
title: "Assignment 11_Submission"
author: "Josh Lake"
date: "October 20, 2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(caret)
library(ranger)
library(rpart)
library(rattle)
library(e1071)
```

##Clean Data
```{r}
df <- read.csv("adult.csv", header = FALSE, na.strings = c(" ?", "NA", "."))
names(df) = c("age", "workclass", "fnlwgt", "education", "educationNum", "maritalStatus", "occupation", "relationship", "race", "sex", "capitalGain", "capitalLoss", "hoursPerWeek", "nativeCountry", "target")
df$age <- as.numeric(df$age)
df$workclass <- as.factor(df$workclass)
df$fnlwgt <- as.numeric(df$fnlwgt)
df$education <- as.factor(df$education)
df$educationNum <- as.numeric(df$educationNum)
df$maritalStatus <- as.factor(df$maritalStatus)
df$occupation <- as.factor(df$occupation)
df$relationship <- as.factor(df$relationship)
df$race <- as.factor(df$race)
df$sex <- as.factor(df$sex)
df$capitalGain <- as.numeric(df$capitalGain)
df$capitalLoss <- as.numeric(df$capitalLoss)
df$hoursPerWeek <- as.numeric(df$hoursPerWeek)
df$nativeCountry <- as.factor(df$nativeCountry)
```

##Replace Missing Categorical Variables
```{r}
repcat = function(x){
  for(i in 1:ncol(x)){
    if(is.factor(x[,i])){
     x[,i][is.na(x[,i])] = levels(x[,i])[which.max(table(x[,i]))]
    }
  }
  return(x)
}
df = repcat(df)
```

##Variable Categories
```{r}
levels(df$education) = c("HS", "HS", "HS", "Ele", "Middle", "Middle", "HS", "UGrad", "UGrad", "UGrad", "PGrad", "HS", "PGrad", "Ele", "UGrad", "UGrad")
levels(df$occupation) = c("WCollar", "Military", "BCollar", "WCollar", "BCollar", "BCollar", "BCollar", "Other", "BCollar", "WCollar", "BCollar", "WCollar", "WCollar", "BCollar")
levels(df$nativeCountry) = c("Asia", "NAmerica", "Asia", "SAmerica", "CAmerica", "CAmerica", "SAmerica", "SAmerica", "Europe", "Europe", "Europe", "Europe", "CAmerica", "CAmerica", "Europe", "CAmerica", "Asia", "Europe", "Asia", "Asia", "Europe", "Europe", "CAmerica", "Asia", "Asia", "NAmerica", "CAmerica", "NAmerica", "SAmerica", "Asia", "Europe", "Europe", "CAmerica", "Europe", "Asia", "Asia", "Asia", "SAmerica", "NAmerica", "Asia", "Europe")
```

##Encoding Variables:
```{r}
df$workclass <- is.numeric(df$workclass)
df$relationship <- is.numeric(df$relationship)
```
##To run model without encoding data, do not run this previous chunk

##Model Build 1: Impute by Median
```{r}
preProcess_missingdata_model <- preProcess(df, method= c('medianImpute', "center", "scale"))
trainData <- predict(preProcess_missingdata_model, newdata = df)
dummies_model <- dummyVars(target ~ ., data=trainData)
trainData_mat <- predict(dummies_model, newdata = trainData)
trainData <- data.frame(trainData_mat)
trainData$target <- df$target

set.seed(2018)
splitIndex <- createDataPartition(trainData$target, p = .70, list = FALSE, times = 1)
train <- trainData[ splitIndex,]
test <- trainData[-splitIndex,]
dfmodel <- train(target~.,data = train, method = "rpart")
pred=predict(dfmodel,test)
cm=confusionMatrix(pred, test$target, positive=" >50K")
cm
```

##Ranger Model
```{r}
dfmodel=ranger(target ~., data = train)
levels(test$target) = c("0", "1")
pred2=predict(dfmodel, data = test)$predictions
levels(pred2) = c("0", "1")
cm2=confusionMatrix(pred2, test$target, positive="1")
cm2
```

##Model Build 2: Impute by Mean
```{r}
repnum = function(x){
  for(i in 1:ncol(x)){
    if(is.numeric(x[,i])){
     x[is.na(x[,i]), i] <- mean(x[,i], na.rm = TRUE)
    }
  }
  return(x)
}
repnum(df)

df = repnum(df)

preProcess_missingdata_model <- preProcess(df, method= c("center", "scale"))
trainData <- predict(preProcess_missingdata_model, newdata = df)
dummies_model <- dummyVars(target ~ ., data=trainData)
trainData_mat <- predict(dummies_model, newdata = trainData)
trainData <- data.frame(trainData_mat)
trainData$target <- df$target

set.seed(2018)
splitIndex <- createDataPartition(trainData$target, p = .70, list = FALSE, times = 1)
train <- trainData[ splitIndex,]
test <- trainData[-splitIndex,]
dfmodel <- train(target~.,data = train, method = "rpart")
pred=predict(dfmodel,test)
cm=confusionMatrix(pred, test$target, positive=" >50K")
cm

dfmodel=ranger(target ~., data = train)
levels(test$target) = c("0", "1")
pred2=predict(dfmodel, data = test)$predictions
levels(pred2) = c("0", "1")
cm2=confusionMatrix(pred2, test$target, positive="1")
cm2
```

##Model Build 3: Impute by Knn Impute
```{r}
preProcess_missingdata_model <- preProcess(df, method= c('knnImpute', "center", "scale"))
trainData <- predict(preProcess_missingdata_model, newdata = df)
dummies_model <- dummyVars(target ~ ., data=trainData)
trainData_mat <- predict(dummies_model, newdata = trainData)
trainData <- data.frame(trainData_mat)
trainData$target <- df$target

set.seed(2018)
splitIndex <- createDataPartition(trainData$target, p = .70, list = FALSE, times = 1)
train <- trainData[ splitIndex,]
test <- trainData[-splitIndex,]
dfmodel <- train(target~.,data = train, method = "rpart")
pred=predict(dfmodel,test)
cm=confusionMatrix(pred, test$target, positive=" >50K")
cm

dfmodel=ranger(target ~., data = train)
levels(test$target) = c("0", "1")
pred2=predict(dfmodel, data = test)$predictions
levels(pred2) = c("0", "1")
cm2=confusionMatrix(pred2, test$target, positive="1")
cm2
```


## Model 4: Encoding with different methods
```{r}
df1 <- df
df1$education <- as.numeric(df1$education)
df1$occupation <- as.numeric(df1$occupation)
df1$nativeCountry <- as.numeric(df1$nativeCountry)

preProcess_missingdata_model <- preProcess(df1, method= c('knnImpute', "center", "scale"))
trainData <- predict(preProcess_missingdata_model, newdata = df1)
dummies_model <- dummyVars(target ~ ., data=trainData)
trainData_mat <- predict(dummies_model, newdata = trainData)
trainData <- data.frame(trainData_mat)
trainData$target <- df$target

set.seed(2018)
splitIndex <- createDataPartition(trainData$target, p = .70, list = FALSE, times = 1)
train <- trainData[ splitIndex,]
test <- trainData[-splitIndex,]
dfmodel <- train(target~.,data = train, method = "rpart")
pred=predict(dfmodel,test)
cm=confusionMatrix(pred, test$target, positive=" >50K")
cm

dfmodel=ranger(target ~., data = train)
levels(test$target) = c("0", "1")
pred2=predict(dfmodel, data = test)$predictions
levels(pred2) = c("0", "1")
cm2=confusionMatrix(pred2, test$target, positive="1")
cm2
```
```


    Error in parse(text = x, srcfile = src): attempt to use zero-length variable name
    Traceback:
    



```R
---
title: "Assignment 12 Submission"
author: "Josh Lake"
date: "October 29, 2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ranger)
library(caret)
library(readr)
library(rpart)
library(glmnet)
```

## Question 1: Import and Preprocess Data
```{r}
df <- read.csv("adult.csv", header = FALSE, na.strings = c(" ?", "NA", "."))
names(df) = c("age", "workclass", "fnlwgt", "education", "educationNum", "maritalStatus", "occupation", "relationship", "race", "sex", "capitalGain", "capitalLoss", "hoursPerWeek", "nativeCountry", "target")
df$age <- as.numeric(df$age)
df$workclass <- as.factor(df$workclass)
df$fnlwgt <- as.numeric(df$fnlwgt)
df$education <- as.factor(df$education)
df$educationNum <- as.numeric(df$educationNum)
df$maritalStatus <- as.factor(df$maritalStatus)
df$occupation <- as.factor(df$occupation)
df$relationship <- as.factor(df$relationship)
df$race <- as.factor(df$race)
df$sex <- as.factor(df$sex)
df$capitalGain <- as.numeric(df$capitalGain)
df$capitalLoss <- as.numeric(df$capitalLoss)
df$hoursPerWeek <- as.numeric(df$hoursPerWeek)
df$nativeCountry <- as.factor(df$nativeCountry)

repnum = function(x){
  for(i in 1:ncol(x)){
    if(is.numeric(x[,i])){
     x[is.na(x[,i]), i] <- mean(x[,i], na.rm = TRUE)
    }
  }
  return(x)
}
df = repnum(df)

repcat = function(x){
  for(i in 1:ncol(x)){
    if(is.factor(x[,i])){
     x[,i][is.na(x[,i])] = levels(x[,i])[which.max(table(x[,i]))]
    }
  }
  return(x)
}
df = repcat(df)

levels(df$education) = c("HS", "HS", "HS", "Ele", "Middle", "Middle", "HS", "UGrad", "UGrad", "UGrad", "PGrad", "HS", "PGrad", "Ele", "UGrad", "UGrad")
levels(df$occupation) = c("WCollar", "Military", "BCollar", "WCollar", "BCollar", "BCollar", "BCollar", "Other", "BCollar", "WCollar", "BCollar", "WCollar", "WCollar", "BCollar")
levels(df$nativeCountry) = c("Asia", "NAmerica", "Asia", "SAmerica", "CAmerica", "CAmerica", "SAmerica", "SAmerica", "Europe", "Europe", "Europe", "Europe", "CAmerica", "CAmerica", "Europe", "CAmerica", "Asia", "Europe", "Asia", "Asia", "Europe", "Europe", "CAmerica", "Asia", "Asia", "NAmerica", "CAmerica", "NAmerica", "SAmerica", "Asia", "Europe", "Europe", "CAmerica", "Europe", "Asia", "Asia", "Asia", "SAmerica", "NAmerica", "Asia", "Europe")
levels(df$workclass) = c("FederalGov", "LocalGov", "NeverWorked", "Private", "SelfEmpInc", "SelfEmpNotInc", "StateGov", "WithoutPay")
levels(df$maritalStatus) = c("Divorved", "MarriedAFSpouse", "MarriedCivSpouse", "MarriedSpouseAbsent", "NeverMarried", "Separated", "Widowed")
levels(df$relationship) = c("Husband", "NotInFamily", "OtherRelative", "OwnChild", "Unmarried", "Wife")
levels(df$race) = c("AmerIndianEskimo", "AsianPacIslander", "Black", "Other", "White")
levels(df$target) = c("Less50k", "More50k")
```

## Question 2: Build Random Forest
```{r}
myGrid <- expand.grid(mtry = 2, splitrule = c("gini"), min.node.size = c(1:3))
model <- train(target~.,data = df, method = "ranger", 
               trControl = trainControl(method ="cv", number = 7, verboseIter = TRUE),
               tuneGrid = myGrid)
model
```

## Question 3: Train Decision Tree
```{r}
splitIndex <- createDataPartition(df$target, p = .70, list = FALSE, times = 1)
train <- df[ splitIndex,]
test <- df[-splitIndex,]
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree <- train(target ~., data = train, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree
plot(dtree)
```

## Question 5: Print Out Random Forest
```{r}
plot(model)
```

## Question 6: Random Forest with Different Parameters
```{r}
myGrid2 <- expand.grid(mtry = 5, splitrule = c("gini","extratrees"), min.node.size = c(1:5))
model2 <- train(target~.,data = df, method = "ranger", 
               trControl = trainControl(method ="cv", number = 7, verboseIter = TRUE),
               tuneGrid = myGrid2)
model2
```

## Question 8: glmnet Model
```{r}
glmcntrl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)
myGrid3 <- expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 10))
model3 <- train(target ~ ., data = df, tuneGrid = myGrid3, method = "glmnet", trControl = glmcntrl)
model3
```
```


    Error in parse(text = x, srcfile = src): attempt to use zero-length variable name
    Traceback:
    



```R
---
title: "Assignment 12 Submission"
author: "Josh Lake"
date: "October 29, 2018"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ranger)
library(caret)
library(readr)
library(rpart)
library(glmnet)
```

## Question 1: Import and Preprocess Data
```{r}
df <- read.csv("titanic.csv")
df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)
df$SibSp <- as.numeric(df$SibSp)
df$Parch <- as.numeric(df$Parch)

levels(df$Survived) = c("dnSurvive", "Survive")

df$PassengerId <- NULL
df$Name <- NULL
df$Ticket <- NULL
df$Cabin <- NULL

repnum = function(x){
  for(i in 1:ncol(x)){
    if(is.numeric(x[,i])){
     x[is.na(x[,i]), i] <- mean(x[,i], na.rm = TRUE)
    }
  }
  return(x)
}
df = repnum(df)

repcat = function(x){
  for(i in 1:ncol(x)){
    if(is.factor(x[,i])){
     x[,i][is.na(x[,i])] = levels(x[,i])[which.max(table(x[,i]))]
    }
  }
  return(x)
}
df = repcat(df)
```

## Question 2: Build Random Forest
```{r}
myGrid <- expand.grid(mtry = 2, splitrule = c("gini"), min.node.size = c(1:3))
model <- train(Survived ~.,data = df, method = "ranger", 
               trControl = trainControl(method ="cv", number = 7, verboseIter = TRUE),
               tuneGrid = myGrid)
model
```

## Question 3: Train Decision Tree
```{r}
splitIndex <- createDataPartition(df$Survived, p = .70, list = FALSE, times = 1)
train <- df[ splitIndex,]
test <- df[-splitIndex,]
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree <- train(Survived ~., data = train, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree
plot(dtree)
```

## Question 5: Print Out Random Forest
```{r}
plot(model)
```

## Question 6: Random Forest with Different Parameters
```{r}
myGrid2 <- expand.grid(mtry = 7, splitrule = c("gini","extratrees"), min.node.size = c(1:20))
model2 <- train(Survived ~.,data = df, method = "ranger", 
               trControl = trainControl(method ="cv", number = 7, verboseIter = TRUE),
               tuneGrid = myGrid2)
model2
```

## Question 8: glmnet Model
```{r}
glmcntrl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)
myGrid3 <- expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 10))
model3 <- train(Survived ~ ., data = df, tuneGrid = myGrid3, method = "glmnet", trControl = glmcntrl)
model3
```
```


    Error in parse(text = x, srcfile = src): attempt to use zero-length variable name
    Traceback:
    



```R
'Quick Clean Function'
quick_clean <- function(x, parameter){
  if(parameter == 1){
    checkcol=function(x){
      if(is.numeric(x) == TRUE){
        x = x[is.na(x)] = mean(x, na.rm = TRUE)
      } else {
          levels = unique(x)
          x = x[is.na(x)]=levels[which.max(tabulate(match(x, x=levels)))]
      }
    }
    x = lapply(x,checkcol)
    return(data.frame(x))
  } else if(parameter == 2){
      x = x[complete.cases(x),]
  } else print("Invalid parameter, choose 1 for recolnaplace or 2 for remove")
}

'Quick Visual Function'
quick_visual <- function(x, parameter){
  library(ggplot2)
  if(is.data.frame(x) & task == 1){
    df_factors <- x[,sapply(df, is.factor)]

    cat.5 <- function(x)
    {
      l <- (length(unique(x)) < 5)
      return(l)
    }

    df_factors5 <- df_factors[, sapply(df_factors, cat.5)]

    combinations <- combn(names(df_factor5), 2, simplify = TRUE)

    bar.charts <- function(d){
      chart <- ggplot(d) + geom_bar(mapping = aes(x=d[,1], fill=d[,2]), position = "dodge") +
        xlab(names(d[1])) +
        labs(fill = names(d[2]))

      return(chart)
    }

    c <- function(x){
      bar.charts(df_factors5[,x])
    }

    charts <- apply(combinations, 2, c)

    return(charts)
  } else if (taskNumber == 2){
      df_factors <- x[,sapply(x, is.factor)]

      cat.5 <- function(x)
      {
        l <- (length(unique(x)) < 5)
        return(l)
      }

      df_factors5 <- df_factors[, sapply(df_factors, cat.5)]

      combinations <- combn(names(df_factor5), 2, simplify = TRUE)

      density.curves <- function(d){
        chart <- ggplot(d) + geom_density(aes(x=d[,1], color=d[,2]), position = "dodge") +
          xlab(names(d[1])) +
          labs(fill = names(d[2]))

        return(chart)
      }

      c <- function(x){
        density.curves(df_factors5[,x])
      }

      charts <- apply(combinations, 2, c)

      return(charts)
    } else if (taskNumer == 3){
        nums <- unlist(lapply(x, is.numeric))
        xnum <- x[ , nums]
        pairs(xnum[,1:ncol(xnum)], lower.panel = NULL)
    } else {print("Invalid taskNumber, choose between 1, 2, or 3")}}

'Quick Model Function'
quick_model <- function(x){
  myGrid <- expand.grid(mtry = 2, splitrule = c("gini"), min.node.size = c(1:3))
  model <- train(target~.,data = x, method = "ranger",
                 trControl = trainControl(method ="cv", number = 7, verboseIter = TRUE),
                 tuneGrid = myGrid)
  model}

```


'Quick Clean Function'



'Quick Visual Function'



'Quick Model Function'



```R
\name{Assignment14}
\alias{Assignment 14}
\title{Assignment 14: Quick Functions}
\description{This package contains 3 functions: quick_clean can be used ot clean missing data; quick_visual can be used to create visualizations of data; and quick_model can be used to create models of your data.}
\usage{
quick_clean(x, parameter)
quick_visual(x, parameter)
quick_model(x)
}
\arguments{
  \item{x}{x represents your dataset}
  \item{parameter}{parameter represents the option you are selecting; a numeric value}
}
\details{Parameter for quick_clean is either 1,remove missing, or 2, replace missing.

Parameter for quick_visual can be either 1, all the possible the barcharts of two small categorical variables; 2, all density curves of numeric variables partitioning on each small categorical variable; 3, scatter plots of  all possible pair of numeric variables.

NOTE: for quick_model, the name of the target variable must be specificed as "target" for the function to work.}

\author{Josh Lake}
```


    Error in parse(text = x, srcfile = src): <text>:1:1: unexpected input
    1: \
        ^
    Traceback:
    



```R

```
