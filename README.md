# Assignment Getting and Cleaning Data Course Project

This repository contains my submission for the final assignment.
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.

The underlying data originate from experiments that have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

## How It Works

### 0. Preparation phase.
* Consists of downloading and unzipping the data.

### 1. Merge the training and the test sets to create one data set.
* To do this, the script imports names of all the available features.
* Afterwards it uses those features to identify and import test data from a X_test and a X_train file.
* Now it concatenates the X_test and X_train data tables.
* Next step is to Import and cocatenate both the Y_test and Y_train activity data and the subject_test and subject_train subject data.
* For reasons of clarity, the columns are renamed.

### 2. Extract only the measurements on the mean and standard deviation for each measurement.
* This is done by identifying columns by name that contain this information.

### 3. Use descriptive activity names to name the activities in the data set.
* The activity labels are imported.
* Now a new column is added to store descriptive activity names.
* Insert descriptive activity names.

### 4. Appropriately label the data set with descriptive variable names.
* Find and replace the affected columns.

### 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

* To do this you must first remove the column activity.
* Then we can create a second, independent tidy data set.
* Last step is to export it.

## Dependencies
The script uses the following R libraries which need to be installed:

* codebook
* plyr
* dplyr