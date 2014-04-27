## The run_analysis function loads the Samsung data and returns a tidy dataset
## that contains the average of each variable for each activity and each subject. 
##
## Assumptions:
## 1) The working directory contains the following files from the Samsung data:
##      activity_labels.txt
##      X_train.txt
##      X_test.txt
##      y_train.txt
##      y_test.txt
##      subject_test.txt
##      subject_train.txt
##      features.txt
## 2) For this analysis, the dataset returned (and submitted as part of the class
##    project) is the dataset outlined in project instruction step 5

run_analysis <- function() {
  # Read in activity labels
  activities <- read.table("activity_labels.txt")
  
  # Name the columns of the activity labels data frame
  names(activities) <- c("activityid", "activity")
  
  # Read in the X training set
  x_train <- read.table("X_train.txt")
  
  # Read in the X testing set
  x_test  <- read.table("X_test.txt")
  
  # Read in the y training set
  y_train <- read.table("y_train.txt")
  
  # Read in the y testing set
  y_test  <- read.table("y_test.txt")
  
  # Read in the test subjects
  subject_test <- read.table("subject_test.txt")
  
  # Read in the training subjects
  subject_train <- read.table("subject_train.txt")
  
  # Read in the feature names
  features <- read.table("features.txt")
  
  # Set the column names for the training and test subjects
  names(subject_test) <- "subject"
  names(subject_train) <- "subject"
  
  # Extract the corresponding activity labels from the y_test data frame
  # Also convert activities to lower case
  activitylabelstest <- tolower(activities$activity[y_test[,1]])
  
  # Set the column name to activity
  names(activitylabelstest) <- "activity"
  
  # Extract the corresponding activity labels from the y_test data frame
  # Also convert activities to lower case
  activitylabelstrain <- tolower(activities$activity[y_train[,1]])

  # Set the column name to activity
  names(activitylabelstrain) <- "activity"
  
  # Find and extract all std() and mean() features
  x_test2 <- x_test[,grep("std|mean\\()",features[,2])]
  
  # Remove the () and set the feature names to lower case
  names(x_test2) <- tolower(sub("\\()","",features[,2][grep("std|mean\\()",features[,2])]))
  
  # Find and extract all std() and mean() features
  x_train2 <- x_train[,grep("std|mean\\()",features[,2])]
  
  # Remove the () and set the feature names to lower case
  names(x_train2) <- tolower(sub("\\()","",features[,2][grep("std|mean\\()",features[,2])]))
  
  # Combine the training data: subject, activity labels and feature variables
  x_train3 <- cbind(subject_train, activitylabelstrain, x_train2)
  
  # Set the 2nd column name to activity
  names(x_train3)[2] <- "activity"

  # Combine the test data: subject, activity labels and feature variables
  x_test3 <- cbind(subject_test, activitylabelstest, x_test2)

  # Set the 2nd column name to activity
  names(x_test3)[2] <- "activity"
  
  # Combine the training and test data
  tidydata <- rbind(x_train3,x_test3)
  
  # Melt the combined dataset for easier processing
  data <- melt(tidydata, id.vars=c("subject","activity"))
  
  # Apply the mean function to the feature variables and group by subject and activity
  data <- dcast(data, subject + activity ~ variable, mean)
  
  # Save tidy data set corresponding to step 4 in project instructions to disk
  write.table(tidydata,"tidydataset_step4.txt")
  
  # Save smaller aggregated tidy data set corresponding to step 5 in project
  # instructions to disk
  write.table(data,"tidydataset_step5.txt")
  
  # Return the data frame corresponding to step 5 in project instructions
  data
}