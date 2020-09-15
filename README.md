# Exercise-Performance
---
title: "Exercise Performance"
author: "Lavnish Singh Bisht"
---
The goal of the project is to predict the manner in which they did the exercise.
the "classe" variable in the dataset represents that variable.
```{r , echo=TRUE}
library(readr)
training <- read_csv("pml-training.csv")
testing <- read_csv("pml-testing.csv")
  

```
as can be seen in this data their too many columns and so many of them have no data(NA)
so the columns which have more than 19000 observations empty should be removed


```{r , echo=TRUE}
library(caret)
a=1
b=1
for(i in c(1:160))
{  if(sum(is.na(training[,i]))>19000)
{b[a]= i
a=a+1}}
training= training[,-b]
testing= testing[,-b]
dim(training)
```
we can see now their are only 60 columns

Some exploraty data analysis
```{r , echo=TRUE}
str(training)
unique(training$user_name)
```
their are 5 different users on which test was conducted

Deleting the columns with useless information for prediction model - like name, time, window etc
i.e from column 1-7
```{r , echo=TRUE}
training= training[,-c(1:7)]
testing= testing[,-c(1:7)]
```



```{r , echo=TRUE}
training$classe= factor(training$classe)
plot(training$classe, col="green", main = "classe frequency", xlab = "classe", ylab = "frequency")
```

we will prepare validation dataset for cross validation and name it "vald"
```{r , echo=TRUE}
ind= createDataPartition(training$classe, p=.75,list = FALSE)
ind= as.numeric(ind)
vald= training[-ind,]
training= training[ind,]

```


now we will build models to predict the classe of the observation
classification tree
```{r , echo=TRUE}
library(rpart.plot)
library(rattle)
modelrp= train(classe~., data= training, method="rpart")
predrp= predict(modelrp, vald)
fancyRpartPlot(modelrp$finalModel)
confusionMatrix(predrp, vald$classe)
```

linear Discreeminative analysis
```{r,echo=TRUE}
modld= train(classe~., data= training, method="lda")
predlda= predict(modld, vald)
confusionMatrix(predlda, vald$classe)



```
we have built 2 models and compared the accuracy


and lilnear discreminative analsis has better accuracy so is the finalmodel
predictions of testing data from final model
```{r, echo=TRUE}
results=predict(modld,newdata =testing)
results
```



