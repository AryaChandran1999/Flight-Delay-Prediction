library(readr)

#1. Load the data
origData <- read.csv2("flightDelay.csv",sep=",",header=TRUE,stringsAsFactors = FALSE)
View(origData)

#2. Select only flights to & from large airports
airports<-c("ATL","LAX","ORD","DFW","JFK","SFO","CLT","LAS","PHX")
origData<-subset(origData,DEST %in% airports & ORIGIN %in% airports )

#3. Take a peak at the data
head(origData,2)
tail(origData,2)
str(origData)

#4. Realise that there's an useless column called "X" ath the end, so removing this column by setting it to `NULL`:
origData$X<-NULL

#5. Check if any of the numeric columns are correlated (which can thus be removed)
cor(origData[c("ORIGIN_AIRPORT_SEQ_ID","ORIGIN_AIRPORT_ID")])
cor(origData[c("DEST_AIRPORT_SEQ_ID","DEST_AIRPORT_ID")])

#Correlation is `1` for airport sequence ID and Airport ID for both origin and destination, so one of each pair can be moved.
#Correlation method would only work for numeric columns, it doesn't work for string columns.
#For <b>string</b> columns, find where the columns are different by using ``!=

origData$ORIGIN_AIRPORT_SEQ_ID<-NULL
origData$DEST_AIRPORT_SEQ_ID<-NULL

#6. The `OP_UNIQUE_CARRIER` and `OP_CARRIER` columns are text and seem identical. Let's see if any of them if not identical. The `,` at the end is important.
mismatched<-origData[origData$OP_CARRIER!=origData$OP_UNIQUE_CARRIER,]
nrow(mismatched)

#`nrow = 0` means that the two columns are identical, so we can drop one.

origData$OP_UNIQUE_CARRIER<-NULL

#7. Cleaning the data: for arrival delay column `ARR_DEL15` and departure delay column `DEP_DEL15`, remove rows with `NA` or ""
onTimeData<-origData[!is.na(origData$ARR_DEL15)&origData$ARR_DEL15!=""&!is.na(origData$DEP_DEL15)&origData$DEP_DEL15!="",]


#Checking for Multicollinearity
library(caTools)
library(car)
library(quantmod)
library(MASS)

featureCols<-c("ARR_DEL15","DAY_OF_WEEK","OP_CARRIER","DEST","ORIGIN","DEP_TIME_BLK")
train_df <- data.frame(onTimeData[,featureCols])
train_df
linear_model <- lm(ARR_DEL15~., data = train_df)
summary(linear_model)
vif(linear_model)

#Look at the difference between old and new data sets, old data set should includ `NA` and `""`:
nrow(origData)
nrow(onTimeData)

#8. The `DISTANCE` column is currently a string, we should convert it to integers. Do the same for `CANCELLED` and `DIVERTED`.
onTimeData$DISTANCE<-as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED<-as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED<-as.integer(onTimeData$DIVERTED)

#9. Convert other columns from strings to factors (so that they can work with random forest model)
onTimeData$ARR_DEL15<-as.factor(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15<-as.factor(onTimeData$DEP_DEL15)
onTimeData$DEST_AIRPORT_ID<-as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID<-as.factor(onTimeData$ORIGIN_AIRPORT_ID)
onTimeData$DAY_OF_WEEK<-as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$DEST<-as.factor(onTimeData$DEST)
onTimeData$ORIGIN<-as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK<-as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$OP_CARRIER<-as.factor(onTimeData$OP_CARRIER)

str(onTimeData)

#10. See how many delays vs. non-delays occur in the data - this will help us understand if we have enough data to build prediction. 
#`1` = delayed, `0` = not delayed, ~ 23% of flights are delayed.
tapply(onTimeData$ARR_DEL15,onTimeData$ARR_DEL15,length)


#11. Select algorithm for machine learning
#* Prediction -> Supervised Machine Learning
#* Regression vs. Classification --> Classification (i.e. predict if a flight would arrive 15+ minutes after the scheduled arrival time)
#* Eliminate "ensemble" algorithms which have multiple child algorithms, and are aimed to boost performance amount of data
#* There are three candidate algorithms:
#  + <b>Naives Bayes</b>: based on likelihood and probability, every feature(column) has the same weight, requires smaller amount of data
#+ <b>Logistic Regression</b>: gives binary result, relationships between features are weighted
#+ <b>Decision Tree</b>: binary tree, each node contains decision, requires enough data to determine nodes and splits, takes more time to run
#* I chose <b>Logistic Regression</b> for this model because:
#  + It is simple and easy to understand
#+ It is faster than some of the other algorithms 
#+ It is stable relative to data changes

#12. Split the data into two sets: `training` and `testing`, and select columns
#* Split the data into two sets: 70% for training set, 30% for testing set
#* Select minimum features(columns) based on hypothesis of what would impact the result:
#  + Origin and Destination ("ORIGIN","DEST")
#+ Day of the Week ("DAY_OF_WEEK")
#+ Carrier ("OP_CARRIER")
#+ Departure Time Block ("DEP_TIME_BLK")
#+ Arrival Delay 15 (required as part of supervised machine learning)

#13. Use the `caret` package to split the data 
#Caret = <u>C</u>lassification <u>A</u>nd <u>RE</u>gression <u>T</u>raining

#* Split the data
#* Pre-process the data
#* Feature selection
#* Model tuning

library(caret)
set.seed(122515)
featureCols<-c("ARR_DEL15","DAY_OF_WEEK","OP_CARRIER","DEST","ORIGIN","DEP_TIME_BLK")
onTimeDataFiltered<-onTimeData[,featureCols]

#Split the data into two sets: 70% training data, 30% testing data. Make sure that they are evenly distributed based on the value `ARR_DEL15` (i.e. the proportion of delays should be the same among the two sets)
inTrainRows<-createDataPartition(onTimeDataFiltered$ARR_DEL15,p=0.7, list= FALSE)

#Take a peek at this list, which will help create the dataframe for training data
head(inTrainRows,10)

#14. Create the 70% training and 30% testing sets
trainDataFiltered<-onTimeDataFiltered[inTrainRows,]
testDataFiltered<-onTimeDataFiltered[-inTrainRows,]


#Check that the training data is split at 70%, and that the testing data is split at 30%
nrow(trainDataFiltered)/(nrow(testDataFiltered)+nrow(trainDataFiltered))
nrow(testDataFiltered)/(nrow(testDataFiltered)+nrow(trainDataFiltered))


#15. Create the training model
#The `.` sign next to `~` means that all columns except the one before `~` is used to train the model
logisticRegModel<-train(ARR_DEL15~.,data=trainDataFiltered,method="glm",family="binomial")

class(testDataFiltered[,"ARR_DEL15"])

#16.  Test the logistic regression model's accuracy, using test data
logRegPrediction<-predict(logisticRegModel,testDataFiltered)
logRegPrediction
class(logRegPrediction)
logRegConfMat<-confusionMatrix(logRegPrediction,testDataFiltered[,"ARR_DEL15"])
logRegConfMat

#Interpretation:
#  * row 1 column 1 (value = `6812` , we call it `A`) is the number of flights in the test data which were not delayed, and when the model predicted it wouldn't be delayed
#* row 1 column 2 (value = `1116` , we call it `B`) is the number of flights in the test data which were delayed, and when the model predicted it wouldn't be delayed
#* row 2 column 1 (value = `3`, we call it `C`) is the number of flights in the test data which were not delayed, and when the model predicted it would be delayed
#* row 2 column 2 (value = `5`, we call it `D`) is the number of flights in the test data which were delayed, and when the model predicted it would be delayed

#Summary for logistic regression model:
#  * Accuracy is `0.859` 
#* Sensitivity is the measure of how the model predicts no delay when there is no delay
#+ `Sensitivity = 0.999
#* Specificity is the measure of model's ability to predict delay when there is a delay
#  + `Specificity = 0.004

#  Specificity is low, which means that the model is not very accurate at predicting when there is a delay.

#17. Improve model performance
#Options for improving model performance:
#  * Add additional columns
#+ e.g. add "DEP_DEL15"
#* Adjust training settings
#* Select a different algorithm
#+ Use emsemble algo: use Random Forest to see if different models can improve performance
#* Rethink the question - is there any other factor that may impact the model?
#  + e.g. add weather data. Where can we get weather data, and how to combine it into the model? To be continued in the next post...

#18. Use the random forest algorithm to see if it improves the model. Use `ConfusionMatrix` to check the result:
library(randomForest)
library(ggplot2)
library(cowplot)


trainDataFiltered
trainDataFiltered[-1]
rfModel<-randomForest(trainDataFiltered[-1],trainDataFiltered$ARR_DEL15,proximity=TRUE,important = TRUE)
rfValidation<-predict(rfModel,testDataFiltered)
rfValidation
rfConfMat<-confusionMatrix(rfValidation,testDataFiltered[,"ARR_DEL15"])
rfConfMat

#* `trainDataFiltered[-1]` is to create a new dataset that excludes the first column in `trainDataFiltered`, which is `ARR_DEL15`.
#* The second parameter `trainDataFiltered$ARR_DEL15` is the columnto predict for

#Summary for random forest model:
#  * <b>Accuracy</b> is `0.7447` which is > target (70%) for this model, but lower than the logistic regression model (at `0.7664`)  

#* <b>Sensitivity</b> is `0.9144`, which is lower than logistic regression model (at `0.98222`)   

#* <b>Specificity</b> is `0.1951`, which is a lot higher than logistic regression model (at `0.06708`)  

#Conclusion:
#  * Logistic regression model provides a fast and accurate prediction of flights that will not be delayed
#* Random forest model performs three times better at predicting when a flight would be delayed than the logistic model.  

