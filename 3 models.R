#Load Libraries
library(ggplot2); library(caret); library(scatterplot3d); library(RColorBrewer)
library(rpart); library(rattle)
setwd('~/Coursera/Machine Learning')
set.seed(1337)

#Load Data
if (!file.exists('./data/train.csv')) {
     trainURL<-'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
     dir.create('./data/')
     download.file(trainURL,method='auto',destfile='./data/train.csv')
}
allData  <- read.csv('./data/train.csv')
inTest   <- createDataPartition(y=allData$X,p=.2,list=FALSE)
testData <- allData[inTest,]
trainData<- allData[-inTest,]
inValid  <- createDataPartition(y=trainData$X,p=.15,list=FALSE)
validData<- trainData[inValid,]
trainData<- trainData[-inValid,]

#Clean Data
##Remove Columns where more than 75% are blank
trainData<-trainData[,sapply(trainData,function(x) (length(x[x != ""])>(0.75*nrow(trainData))))]
##Remove Columns where more than 75% are NA
trainData<-trainData[,sapply(trainData,function(x) (length(x[!is.na(x)])>(0.75*nrow(trainData))))]
trainInfo <- trainData[,(names(trainData) %in% c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window","num_window","classe"))]
trainData <- trainData[,!(names(trainData) %in% c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window","num_window"))]


col<-brewer.pal(5,"RdYlBu")
names(col)=c('A','B','C','D','E')
col2<-col[trainData$class] ; names(col2)<-NULL
accuracy <- function(values,prediction){sum((prediction == values))/length(values)}

#Exploratory Data Analysis
q1<-qplot(x=roll_belt,y=pitch_belt,data=trainData,colour=classe) ;q1
q2<-qplot(x=roll_belt,y=yaw_belt,data=trainData,colour=classe) ;q2

scatterplot3d(x=trainData$gyros_belt_x
             ,y=trainData$gyros_belt_y
             ,z=trainData$gyros_belt_z
             ,type='h'
             ,pch=19
             ,color=col2
             ,main='Class by Gyro Belt Postion')

model1 <- rpart(classe~.,data=trainData,method="class")

data.frame(training_set = accuracy(trainData$classe,predict(model1,newdata=trainData,type="class")),
           validation_set = accuracy(validData$classe,predict(model1,newdata=validData,type="class")))
table(trainData$classe,predict(model1,newdata=trainData,type="class"))
table(validData$classe,predict(model1,newdata=validData,type="class"))

xtable(table(trainData$classe,predict(model1,newdata=trainData,type="class")))
bestModel <- model1
xtable(table(trainData$classe,predict(bestModel,newdata=trainData,type="class")),caption="\\t Performance in Training Set")



##Model 2: Simple CTree
model2 <- train(classe~.,data=trainData,method="ctree")
data.frame(training_set = accuracy(trainData$classe,predict(model2,newdata=trainData)),
        validation_set = accuracy(validData$classe,predict(model2,newdata=validData)))

##Model 3: CTree with Principal Component Analysis
model3 <- train(classe~.,data=trainData,method="ctree", preProcess="pca")
data.frame(training_set = accuracy(trainData$classe,predict(model3,newdata=trainData)),
        validation_set = accuracy(validData$classe,predict(model3,newdata=validData)))

