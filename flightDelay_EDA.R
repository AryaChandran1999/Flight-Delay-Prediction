##Packages Required:
library(readr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)

origData <- read.csv2("flightDelay.csv",sep=",",header=TRUE,stringsAsFactors = FALSE)
View(origData)

airports<-c("ATL","LAX","ORD","DFW","JFK","SFO","CLT","LAS","PHX")
origData<-subset(origData,DEST %in% airports & ORIGIN %in% airports )

origData$DEP_DEL15 <- as.numeric(origData$DEP_DEL15)
origData$ARR_DEL15 <- as.numeric(origData$ARR_DEL15)
origData$CANCELLED <- as.numeric(origData$CANCELLED)
origData$DIVERTED <- as.numeric(origData$DIVERTED)
origData$DISTANCE <- as.numeric(origData$DISTANCE)


#HEAT MAP
#install.packages("psych")
library(psych)
corPlot(origData[-c(3,4,5,6,8,9,10,12,13,14,17)],cex = 0.5,min.length=8)

ggplot(origData, aes(x=DEP_DEL15, y=ARR_DEL15)) + 
  geom_point(size=0.9)+geom_smooth(method=lm, col='darkred')+
  ggtitle("Relation between Departure Delay and Arrival Delay")

week_table <- table(origData$DAY_OF_WEEK,origData$DEP_DEL15)
spineplot(week_table, main = "Departure delay for each day of the week")

ggplot(origData, aes(x=DAY_OF_WEEK, col=ARR_DEL15)) + geom_density(fill = "lightblue")+
  ggtitle("Arrival Delay for each day of the week")

ggplot(origData, aes(x=DISTANCE, col=ARR_DEL15)) + 
  geom_density(fill = "lightblue")+
  xlim(0,5000)+
  ggtitle("Arrival Delay based on distance between airports")

origin_table <- table(origData$ORIGIN,origData$DEP_DEL15)
origin_table
spineplot(origin_table, main = "Arrival delay in each Airport")
