#Load Libraries
library(ggplot2); library(caret); library(scatterplot3d)
setwd('~/Coursera/Machine Learning')
set.seed(1337)

#Load Data
if (!file.exists('./data/train.csv') or !file.exists('./data/test.csv')) {x<-1}
     trainURL<-'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
     testURL <-'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
     dir.create('./data/')
     download.file(trainURL,method='auto',destfile='./data/train.csv')
     download.file(trainURL,method='auto',destfile='./data/test.csv')
}
trainData<-read.csv('./data/train.csv')
testData <-read.csv('./data/test.csv')

#Clean Data
##Remove Columns where more than 75% are blank
trainData<-trainData[,sapply(trainData,function(x) (length(x[x != ""])>(0.75*nrow(trainData))))]
##Remove Columns where more than 75% are NA
trainData<-trainData[,sapply(trainData,function(x) (length(x[!is.na(x)])>(0.75*nrow(trainData))))]

#Exploratory Data Analysis
q1<-qplot(x=roll_belt,y=pitch_belt,data=trainData,colour=classe) ;q1
q2<-qplot(x=roll_belt,y=yaw_belt,data=trainData,colour=classe) ;q2

col<-brewer.pal(5,"RdYlBu")
names(col)=c('A','B','C','D','E')
col2<-col[trainData$class] ; names(col2)<-NULL


scatterplot3d(x=trainData$gyros_belt_x
             ,y=trainData$gyros_belt_y
             ,z=trainData$gyros_belt_z
             ,type='h'
             ,pch=19
             ,color=col2
             ,main='Class by Gyro Belt Postion')

                    
