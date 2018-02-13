
getwd();
setwd(choose.dir());

library(plyr)

##C:/Users/petou/Desktop/GETTING AND CLEANING DATA/DATA

# Clean up workspace

rm(list=ls())



# 1. Merge the training and the test sets to create one data set.

setwd('C:/Users/petou/Desktop/GETTING AND CLEANING DATA/DATA');
#READING OF THE TRAIN DATA
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt
#ASSIGNMENT OF THE COLUMN NAMES
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
#MERGE OF THE TRAIN DATA SETS
trainingData = cbind(yTrain,subjectTrain,xTrain);

#READING THE TEST DATA
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

#ASSIGNMENT OF THE COLUMN NAMES
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

#MERGE OF TEST
testData = cbind(yTest,subjectTest,xTest);

#MERGE OF TRAIN AND TEST
finalData = rbind(trainingData,testData);
#CREATION OF THE VECTOR THAT WILL CONTAIN THE DESIRED DATA
colNames  = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# CREATION OF THE LOGICAL VECTOR WITH TRUE VALUES FOR THE ID, MEAN AND STANDARD DEVIATION
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
#KEEPING ONLY DESIRED COLUMNS
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set
# MERGE OF THE FINAL DATASET WITH THE ACTIVITY TABLE FOR DESCRIPTIBE NAMES
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);
#NEW COLUMN NAMES 
colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 
#CLEANING OF THE VARIABLE NAMES
for (i in 1:length(colNames)) 
{
colNames[i] = gsub("\\()","",colNames[i])
colNames[i] = gsub("-std$","StdDev",colNames[i])
colNames[i] = gsub("-mean","Mean",colNames[i])
colNames[i] = gsub("^(t)","time",colNames[i])
colNames[i] = gsub("^(f)","freq",colNames[i])
colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
##REASSIGNMENT OF THE NEW COLUMN NAMES
colnames(finalData) = colNames;


# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
#NEW TABLE WITHOUT THE ACTIVITY TYPE
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];
#INCLUDE THE MEAN OF EACH VARIBLE
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);
##MERGE OF THE TIDY DATA WITH THE ATIVITY ONE TO INCLUDE THE ACTIVITY NAMES
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);
#EXPORT OF THE TIDY DATASET
write.table(tidyData, './data-tidy.txt',row.names=TRUE,sep='\t');

