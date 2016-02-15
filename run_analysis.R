#1. Download data and store in Data_Cleaning_Proj

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "/Users/Karl/Documents/Data Science/Courses/Getting & Cleaning Data/Data_Cleaning_Proj.zip"
download.file(fileURL, destfile = destfile)
###############################################################################################################

#2. Unzip the folder

unzip(zipfile = "/Users/Karl/Documents/Data Science/Courses/Getting & Cleaning Data/Data_Cleaning_Proj.zip", 
      exdir = "~/Data Science/Courses/Getting & Cleaning Data")
###############################################################################################################

#3. Look at the files in the UCI HAR Dataset folder

path <- file.path("~/Data Science/Courses/Getting & Cleaning Data", "UCI HAR Dataset")
zip_files <- list.files(path, recursive = TRUE)
zip_files
###############################################################################################################

#4. Read data into variables
  
#Subject Files
SubjectTrain <- read.table(file.path(path, "train", "subject_train.txt"), header = FALSE)
SubjectTest <- read.table(file.path(path, "test", "subject_test.txt"), header = FALSE)

#Features files
FeaturesTrain <- read.table(file.path(path, "train", "X_train.txt"), header = FALSE)
FeaturesTest <- read.table(file.path(path, "test", "X_test.txt"), header = FALSE)

#Activity files
ActivityTrain <- read.table(file.path(path, "train", "y_train.txt"), header = FALSE)
ActivityTest <- read.table(file.path(path, "test", "y_test.txt"), header = FALSE)
###############################################################################################################

#5. Merge training and test sets to create one dataset
  
#i. Combine sets by row
Subject <- rbind(SubjectTrain, SubjectTest)
Features <- rbind(FeaturesTrain, FeaturesTest)
Activities <- rbind(ActivityTrain, ActivityTest)

#ii. Attribute names to columns
colnames(Subject) <- "subject"
colnames(Activities) <- "activity"
FeaturesNames <- read.table(file.path(path, "features.txt"),header =FALSE)
colnames(Features) <- FeaturesNames[,2]

#iii. Combine resulting sets by column to obtain a single dataset
FinalData <- cbind(Subject, Activities, Features)
###############################################################################################################

#6. Extract only the measurements on the mean and standard deviation for each measurement
  
library(dplyr)
sum(duplicated(colnames(FinalData))) ## check for duplicated column names
FinalData1 <- FinalData[ ,!duplicated(names(FinalData)) ] ## remove duplicate column names
FinalData2 <- select(FinalData1, matches("mean\\(\\)|std\\(\\)")) ## extract mean and std columns
FinalData3 <- cbind(FinalData1[,1:2], FinalData2) ## reattach columns "subject" and "activity"
###############################################################################################################

#7. Use descriptive activity names to name the activities in the data set
  
ActivityLabels <- read.table(file.path(path, "activity_labels.txt"),header = FALSE)
FinalData3$activity <- as.factor(FinalData3$activity)
levels(FinalData3$activity) <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

#8. Appropriately labels the data set with descriptive variable names
  
names(FinalData3)<-gsub("^t", "time", names(FinalData3))
names(FinalData3)<-gsub("Acc", "Accelerometer", names(FinalData3))
names(FinalData3)<-gsub("Gyro", "Gyroscope", names(FinalData3))
names(FinalData3)<-gsub("^f", "frequency", names(FinalData3))
names(FinalData3)<-gsub("Mag", "Magnitude", names(FinalData3))
names(FinalData3)<-gsub("BodyBody", "Body", names(FinalData3))

#9. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
  
library(plyr)
table <- ddply(FinalData3, c("subject", "activity"), function(x) colMeans(x[,3:68]))

