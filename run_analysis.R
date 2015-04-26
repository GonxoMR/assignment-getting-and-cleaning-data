##Getting and cleaning data project assignement
##Coursera

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

---------------------
#Cleaning the workspace
rm(list=ls())

#Callign the data.table library
library("data.table")

#Create a new directory and download the data.
dir.create("dataD")
zipFileLoc = paste(getwd(), "dataD", sep="/")
zipPath = paste(zipFileLoc, "UCI HAR Dataset.zip", sep="/")

if (!file.exists(zipPath)) {
  zipFileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(zipFileUrl, zipPath, method "curl")
}

#Unzip the data
filesLoc = paste(zipFileLoc, "UCI HAR Dataset", sep="/")
unzip (zipPath, exdir= zipFileLoc)
list.files(filesLoc)

#Setting the working directory
setwd(filesLoc)

#Reading the data from the files
activityType = read.table("./activity_labels.txt", header=F, col.names = c("Activity.ID", "Activity.Label"))
features = read.table("./features.txt", header=F )
subjectTrain = read.table("./train/subject_train.txt", header=F, col.names ="Subject.ID" )
Xtrain = read.table("./train/X_train.txt", header=F)
Ytrain = read.table("./train/Y_train.txt", header=F, col.names = "Activity.ID")
subjectTest = read.table("./test/subject_test.txt", header=F, col.names ="Subject.ID")
Xtest = read.table("./test/X_test.txt", header=F)
Ytest = read.table("./test/Y_test.txt", header=F, col.names = "Activity.ID")

# 1. Merge the training and the test sets to create one data set.
#Merging the data by rows
subject = rbind(subjectTrain, subjectTest)
X = rbind(Xtrain, Xtest)
Y = rbind(Ytrain, Ytest)

#Merging in collumns
OneDataSet = cbind(Y,subject,X)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
#Creating a vector with the desired features
extractFeatures = grepl("mean", features[,2]) & !grepl("meanFreq", features[,2]) | grepl("std", features[,2])

#Getting the mean() and std() columns in the Data Set plus the Activity.ID and Subject.ID
OneDataSet = OneDataSet[,c(T,T, extractFeatures)]

# 3. Use descriptive activity names to name the activities in the data set
#Aplying apropiate lables to the dataset by merging
OneDataSet = merge(activityType, OneDataSet, by="Activity.ID")

# 4. Appropriately labels the data set with descriptive variable names.
colnames(OneDataSet) = c("Activity.ID","Activity.Label", "Subject.ID",as.character(features[extractFeatures,2]))

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#Creating a new Data Set and adding colum names
DataNoActivityLabel  = X[,extractFeatures]
colnames(DataNoActivityLabel) = as.character(features[extractFeatures,2])

#Calculating the mean by activity and subject
tidyData = aggregate(DataNoActivityLabel, by=list(Activity.ID=OneDataSet$Activity.ID, Subject.ID = OneDataSet$Subject.ID), mean)

#Merging with the Activity Descriptions and sorting by subject
tidyData = merge(activityType, tidyData, by="Activity.ID")

#Saving the new Data Set
write.table(tidyData,"./tidyData.txt", row.names=FALSE, sep="\t")
