library(codebook)
library(plyr)
library(dplyr)

## 0. Preparation
## Download data.
filename <- "dataset"
if (!file.exists(filename)) {
    fileURL <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method = "curl")
}

# Unzip data.
if (!file.exists("UCI HAR Dataset")) {
    unzip(filename)
}

## 1. Merge the training and the test sets to create one data set.
## Import names of available features.
features <- data.table::fread(
    'UCI HAR Dataset/features.txt',
    select = c(1, 2),
    col.names = c("Id", "Name")
)

## Use features to import and name test data from X_test file
X_test <- data.table::fread('UCI HAR Dataset/test/X_test.txt',
                            select = features$Id,
                            col.names = features$Name)

## Use features to import and name train data from X_train file
X_train <- data.table::fread(
    'UCI HAR Dataset/train/X_train.txt',
    select = features$Id,
    col.names = features$Name
)

## Concatenate X_test and X_train data tables
X_data <- rbind(X_test, X_train)

## Import and cocatenate Y_test and Y_train activity data
Y_test <-
    read.csv("UCI HAR Dataset/test/Y_test.txt", header = FALSE)
Y_train <-
    read.csv("UCI HAR Dataset/train/Y_train.txt", header = FALSE)
Y_data <- rbind(Y_test, Y_train)

## Import and cocatenate subject_test and subject_train subject data
subject_test <-
    read.csv("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
subject_train <-
    read.csv("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
subject_data <- rbind(subject_test, subject_train)

## Rename column in activity data
names(Y_data) <- "activity"
## Rename column in subject data
names(subject_data) <- "subject"

Y_subject_data <- cbind(Y_data, subject_data)
class(Y_subject_data)
data <- cbind(Y_subject_data, as.data.frame(X_data))


## 2. Extract only the measurements on the mean and standard deviation for
## each measurement.
## Identify relevant column names
meanCloumns <- grep('mean()', features$Name)
stdColumns <- grep('std()', features$Name)

class(features[c(meanCloumns, stdColumns), 2])
relevantFeatures <-
    c(as.matrix(features[c(meanCloumns, stdColumns), 2]),
      c("activity", "subject"))
data <- data[, relevantFeatures]

## 3. Use descriptive activity names to name the activities in the data set

## Import activity labels
activityLabels <-
    read.csv("UCI HAR Dataset/activity_labels.txt",
             header = FALSE,
             sep = " ")
names(activityLabels) <- c("id", "label")

library(data.table)

## Add new column to store descriptive activity names
data$activityField = ""

## Insert descriptive activity names
for (row in 1:nrow(data)) {
    activityValue <- as.numeric(data[[row, 'activity']])
    tmpValue <-
        as.character(activityLabels[activityLabels$id == activityValue, ]$label)
    data[row, "activityField"] <- tmpValue
}

## 4. Appropriately label the data set with descriptive variable names.

names(data) <- gsub("^t", "time", names(data))
names(data) <- gsub("^f", "frequency", names(data))
names(data) <- gsub("Acc", "Accelerometer", names(data))
names(data) <- gsub("Gyro", "Gyroscope", names(data))
names(data) <- gsub("Mag", "Magnitude", names(data))
names(data) <- gsub("BodyBody", "Body", names(data))

## From the data set in step 4, create a second, independent tidy data set
## with the average of each variable for each activity and each subject.

## Remove column activity
data$activity <- NULL
tidyDataSet <- data %>% group_by(activityField, subject) %>% summarize_each(mean)
write.table(tidyDataSet, file = "tidyDataSet.txt", row.name = FALSE)
