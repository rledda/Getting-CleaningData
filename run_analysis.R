# This script  perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the "training" and the "test" datasets to create one dataset.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the dataset
# 4. Label the dataset with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Clean up workspace
rm(list=ls())

# 1. Merge the "training" and the "test2 datasets to create one dataset.

#set the working directory to the location where the UCI HAR Dataset are unzipped and archived
setwd("./Get&CleanData")

# Reading in the general datasets 
activity_type = read.table("./activity_labels.txt",header=FALSE) #importing activity_labels.txt
features = read.table("./features.txt",header=FALSE) #importing features.txt

# Read in the training dataset
subject_train = read.table("./train/subject_train.txt",header=FALSE) #importing subject_train.txt
x_train = read.table("./train/X_train.txt",header=FALSE) #importing x_train.txt
y_train = read.table("./train/y_train.txt",header=FALSE) #importing y_train.txt

# Assign column names to the training datasets imported
colnames(x_train) = features[,2]
colnames(y_train) = "activity_id"
colnames(activity_type) = c("activity_id","activity_type")
colnames(subject_train) = "subject_id"

# Create the training dataset merging x_train, y_train and subject_train datasets
training_data = cbind(x_train, y_train,subject_train)

# Read in the test dataset
subject_test = read.table("./test/subject_test.txt",header=FALSE) #importing subject_test.txt
x_test = read.table("./test/X_test.txt",header=FALSE) #importing x_test.txt
y_test = read.table("./test/y_test.txt",header=FALSE) #importing y_test.txt

# Assign column names to the test datasets imported
colnames(subject_test) = "subject_id"
colnames(x_test) = features[,2] 
colnames(y_test) = "activity_id"

# Create the test dataset merging the x_test, y_test and subject_test datasets
test_data = cbind(x_test, y_test,subject_test)

# Combine training and test datasets to create a new dataset
train_test_data = rbind(training_data, test_data)

# Create a vector for the column names from the train_test_data;
# this vector will be used to select the  mean() & stddev() columns
col_names  = colnames(train_test_data)

#Extract only the measurements on the mean and standard deviation for each measurement. 
# 1. Create a logical vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE value for others
logical_vector = (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names))

# 2. Subset train_test_data table based on the logical_vector to keep only the desired columns
train_test_data = train_test_data[logical_vector == TRUE]

# 3. Use descriptive activity names to name the activities in the dataset
# Merge the train_test_data dataset with the acitivity_type table to include the descriptive activity names
train_test_data = merge(train_test_data, activity_type, by='activity_id', all.x=TRUE)

# Updating the col_names vector to include the new column names after merge
col_names  = colnames(train_test_data)

# 4. Label the dataset with descriptive activity names. 
# Cleaning up the variable names
for (i in 1:length(col_names)) 
{
  col_names[i] = gsub("\\()","",col_names[i])
  col_names[i] = gsub("-std$","StdDev",col_names[i])
  col_names[i] = gsub("-mean","Mean",col_names[i])
  col_names[i] = gsub("^(t)","Time",col_names[i])
  col_names[i] = gsub("^(f)","Freq",col_names[i])
  col_names[i] = gsub("([Gg]ravity)","Gravity",col_names[i])
  col_names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",col_names[i])
  col_names[i] = gsub("[Gg]yro","Gyro",col_names[i])
  col_names[i] = gsub("AccMag","AccMagnitude",col_names[i])
  col_names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",col_names[i])
  col_names[i] = gsub("JerkMag","JerkMagnitude",col_names[i])
  col_names[i] = gsub("GyroMag","GyroMagnitude",col_names[i])
}

# Reassigning the new descriptive column names to the finalData set
colnames(train_test_data) = col_names

# 5. Create a tidy dataset with the average of each variable for each activity and each subject. 
# Create a new table, tidy_data1, without the activity_type column
tidy_data1 = train_test_data[,names(train_test_data) != 'activity_type']

# Summarizing the above table to include just the mean of each variable for each activity and each subject
tidy_data2 = aggregate(tidy_data1[,names(tidy_data1) != c('activity_id','subject_id')], by = list(activity_id = tidy_data1$activity_id, subject_id = tidy_data1$subject_id),mean)

# Merging the tidy_data2 dataset with activity_type to include descriptive activity names
tidy_data3 = merge(tidy_data2, activity_type, by='activity_id', all.x=TRUE)

# Export the tidy_data3 dataset 
write.table(tidy_data3, './tidy_data.txt',row.names=TRUE,sep='\t')