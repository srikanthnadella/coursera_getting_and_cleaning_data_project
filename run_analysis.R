# setwd("~/Developer/datasciencecourse/3_getting_and_cleaning_data/course project/coursera_getting_and_cleaning_data_project/")
# getwd()

## Step1. Merges the training and the test sets to create one data set.
X_train <- read.csv("./UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE)
y_train <- read.csv("./UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = "\n")
subject_train <- read.csv("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "\n")

X_test <- read.csv("./UCI HAR Dataset/test/X_test.txt", sep = "", header = FALSE)
y_test <- read.csv("./UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "\n")
subject_test <- read.csv("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "\n")

# Merge train and test data row-wise
X_df <- rbind(X_train, X_test)
activityLabels_df <- rbind(y_train, y_test)
names(activityLabels_df) <- "Activity"
subjects_df <- rbind(subject_train, subject_test)
names(subjects_df) <- "Subject"

## Step2. Extracts only the measurements on the mean and standard 
features <- read.csv("./UCI HAR Dataset/features.txt", header = FALSE, sep = " ")
activity_labels<- read.csv("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = " ")
meanIndices <- grep("mean", features[,2])
stdIndices <- grep("std", features[,2])
feature_names <- as.character(features[,2])
mean_std_indices <- c(meanIndices, stdIndices)
mean_std_merge_df <- X_df[, c(mean_std_indices)]
head(mean_std_merge_df)

## Step3. Uses descriptive activity names to name the activities in 
## the data set
activityLabels_df[,1] <- factor(activityLabels_df[,1], labels = as.character(activity_labels$V2))

## Step4. Appropriately labels the data set with descriptive activity 
## names. 
col_names <- c(feature_names)
col_names <- col_names[c(mean_std_indices)]
names(mean_std_merge_df)
names(mean_std_merge_df) <- col_names
names(mean_std_merge_df)
head(mean_std_merge_df)

## Step5. Creates a second, independent tidy data set with the average of 
## each variable for each activity and each subject. 
mean_std_merge_df <- cbind(subjects_df, activityLabels_df, mean_std_merge_df)
library(dplyr)
output <- data.frame()
i <- 0
for (i in 1:30) {
  new_df_by_subject <- mean_std_merge_df %>% filter(Subject == i) 
  output <- rbind(output, new_df_by_subject %>% group_by(Activity) %>% summarise_each(funs(mean(., na.rm=TRUE))))
}
write.table(output, "./outputFile.txt", row.names = FALSE)
