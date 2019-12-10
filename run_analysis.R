#The data for this project is:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#Steps to download the data

if (!file.exists("getdata_projectfiles_UCI HAR Dataset")){
  download.file(fileURL,"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip("getdata_projectfiles_UCI HAR Dataset") 
}


#1. Merges the training and the test sets to create one data set.

##First let's load the data sets with the correct and respective column names
###Get the column names
featureNames<-read.table("features.txt", col.names = c("number","features")) 
activity_labels<-read.table("activity_labels.txt", col.names = c("number","activity"))

##Load Trainin Data
subject_train<-read.table("train/subject_train.txt", col.names = "subject")
x_train_data<- read.table("train/X_train.txt",col.names = as.character(featureNames$features))
y_train_data<-read.table("train/y_train.txt", col.names = "activity")

##Load Testing Data
subject_test<-read.table("test/subject_test.txt", col.names = "subject")
x_test_data<- read.table("test/X_test.txt",col.names =as.character(featureNames$features) )
y_test_data<-read.table("test/y_test.txt", col.names = "activity")



##let's merge the train and test data sets
merged_X_Data = rbind(x_train_data,x_test_data)
merged_Y_Data = rbind(y_train_data,y_test_data)
merged_Subjects=rbind(subject_train,subject_test)

##Now we will merge the data into one single data set
mergedDataSet = cbind(merged_X_Data, merged_Subjects, merged_Y_Data)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.

##We now extract the standard deviation and the mean from each measurement
###Please note that I did NOT include the meanFreq and gravityMean measurements.
###Also, please note that as a the check.names value was not set when I loaded the data sets the column name values changed according to R's correct syntax
extractedData<-(mergedDataSet[c(grep('std|\\.mean\\.',names(mergedDataSet),ignore.case = TRUE,value = TRUE),"subject","activity")])


#3. Uses descriptive activity names to name the activities in the data set
##I substituded the activity numberic values with the string readable values
library(plyr)
extractedData$activity <- mapvalues(extractedData$activity,1:6,as.character(activity_labels$activity))



#4. Appropriately labels the data set with descriptive variable names.

##I splitted the columns name values and substracted just the repeated meaningful descriptions of the features.
unique(sapply(strsplit(names(extractedData),"\\."),function(x){x[1]}))
##For your reference, the result of this request is the following list:
##[1] "tBodyAcc" "tGravityAcc" "tBodyAccJerk" "tBodyGyro" "tBodyGyroJerk" "tBodyAccMag" "tGravityAccMag" "tBodyAccJerkMag" "tBodyGyroMag" "tBodyGyroJerkMag"
##[11] "fBodyAcc" "fBodyAccJerk" "fBodyGyro" "fBodyAccMag" "fBodyBodyAccJerkMag" "fBodyBodyGyroMag" "fBodyBodyGyroJerkMag" "subject" "activity"     


##Then I proceeded to replace this shortened names with more self explanatory and readable name values.

names(extractedData)<-gsub("Acc", "Acceleration", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("tGravity", "totalGravity", names(extractedData))
names(extractedData)<-gsub("tBody", "totalBody", names(extractedData))
names(extractedData)<-gsub("fGravity", "frequencyGravity", names(extractedData))
names(extractedData)<-gsub("fBody", "frequencyBody", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("\\.\\.\\.", "\\()\\.", names(extractedData))
names(extractedData)<-gsub("\\.\\.", "\\().", names(extractedData))


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##I melted the entire data set into "activity" and "subject" ids. Then I created a new data set of 180*68 with the mean values with respect to activity and subject variables.
meltedDataSet<-melt(extractedData, id=c("activity","subject"))
independentTidyDataSet<-(dcast(meltedDataSet, activity+subject ~ variable,mean))

# Writing the resulted data set to a new txt file
write.csv(independentTidyDataSet, "independentTidyDataSet.txt", row.names=FALSE)

