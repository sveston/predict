Prediction of Exercise Manner
========================================================

## 1. Synopsis

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.  

In this report, I will be to using data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website [here] http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).    

### Data  

The training data for this project are available here:   

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv  

The test data are available here:   

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv   

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 


## 2. Data Preprocessing  

Looking at the training raw data, one will noticed that there are many "NA"s or empty columns.  My first step of cleaning the data is to find in each column, what percentage are not made up of "NA"s. On doing so, I realized there are only 2 thresholds in each of the columns i.e. 1 (no "NA"s) and 0.021 (almost 98% of "NA"s). So a natural selection is to subset the data with full non "NA"s data.  This cut the variables from 160 to 60. After removing the 100 variables, I looked into the description and the entries of the variables. I realized there are 7 additional variables, namely X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window and num_window. Either they have no logical relation to the manner or the entries have no relation to the manner.  This leave me with just 53 variables. The code are as shown here :-


```r
# Load training raw data
trainRawData <- read.csv("pml-training.csv", na.strings = c("NA", ""))

# Find the proportion of non 'Na's in each column
P_nonNa <- as.data.frame(apply(trainRawData, 2, function(x) sum(!is.na(x)))/19622)
table(P_nonNa[, 1])
```

```
## 
## 0.0206910610539191                  1 
##                100                 60
```

```r

# Looking into the rest of the variables to remove
validvar <- subset(P_nonNa, P_nonNa > 0.5)
head(validvar, 10)
```

```
##                      apply(trainRawData, 2, function(x) sum(!is.na(x)))/19622
## X                                                                           1
## user_name                                                                   1
## raw_timestamp_part_1                                                        1
## raw_timestamp_part_2                                                        1
## cvtd_timestamp                                                              1
## new_window                                                                  1
## num_window                                                                  1
## roll_belt                                                                   1
## pitch_belt                                                                  1
## yaw_belt                                                                    1
```

```r

# Creating the valid Training Data Set for training
NAs <- apply(trainRawData, 2, function(x) {
    sum(is.na(x))
})
validData <- trainRawData[, which(NAs == 0)]
cols <- c(1:7)
validData <- validData[, -cols]
```


## 3. Training the Model  

Next, the data set will be needed to be partition into 2 parts, one for training and the other for testing. For this set, I have partition the data into 50:50. And then using Random Forest, the model is shown as below.


```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
# Partition the data in 50/50 training and testing
trainIndex <- createDataPartition(y = validData$classe, p = 0.5, list = FALSE)
trainData <- validData[trainIndex, ]
testData <- validData[-trainIndex, ]

# Using Random Forest to create the prediction model
modFit <- train(trainData$classe ~ ., data = trainData, method = "rf")
```

```
## Loading required package: randomForest
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
modFit
```

```
## Random Forest 
## 
## 9812 samples
##   52 predictors
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 9812, 9812, 9812, 9812, 9812, 9812, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
##   2     1         1      0.003        0.004   
##   30    1         1      0.003        0.004   
##   50    1         1      0.006        0.007   
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

```r
modFit$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 27
## 
##         OOB estimate of  error rate: 1.14%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 2785    4    1    0    0    0.001792
## B   22 1859   17    0    1    0.021064
## C    0   17 1684   10    0    0.015780
## D    0    4   22 1580    2    0.017413
## E    0    1    3    8 1792    0.006652
```


By subtracting the class.error from 1, one can see that the Sensitivity are all well over 98%.


## 4. Testing of Prediction Model  

The model is use to predict the test set and the confusion matrix is output as shown below.


```r
# Apply the prediction on the test data
pred <- predict(modFit, newdata = testData)
confusionMatrix(pred, testData$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2786   24    0    0    0
##          B    1 1869   13    2    2
##          C    0    5 1697   19    5
##          D    1    0    1 1584   10
##          E    2    0    0    3 1786
## 
## Overall Statistics
##                                         
##                Accuracy : 0.991         
##                  95% CI : (0.989, 0.993)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.989         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.999    0.985    0.992    0.985    0.991
## Specificity             0.997    0.998    0.996    0.999    0.999
## Pos Pred Value          0.991    0.990    0.983    0.992    0.997
## Neg Pred Value          0.999    0.996    0.998    0.997    0.998
## Prevalence              0.284    0.193    0.174    0.164    0.184
## Detection Rate          0.284    0.191    0.173    0.161    0.182
## Detection Prevalence    0.286    0.192    0.176    0.163    0.183
## Balanced Accuracy       0.998    0.991    0.994    0.992    0.995
```


Looking at the Statistics by Class, the Sensitivity, Specificity, Positve Prediction Value and the Negative Prediction Value are all above 90%.



