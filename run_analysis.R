##
##

run_analysis <- function() {
  activities <- read.table("activity_labels.txt")
  names(activities) <- c("activityid", "activity")
  
  x_train <- read.table("X_train.txt")
  x_test  <- read.table("X_test.txt")
  y_train <- read.table("y_train.txt")
  y_test  <- read.table("y_test.txt")
  
  subject_test <- read.table("subject_test.txt")
  subject_train <- read.table("subject_train.txt")
  features <- read.table("features.txt")
  
# Column Names
  names(subject_test) <- "subject"
  names(subject_train) <- "subject"
  
# Processing
  activitylabelstest <- tolower(activities$activity[y_test[,1]])
  names(activitylabelstest) <- "activity"
  
  activitylabelstrain <- tolower(activities$activity[y_train[,1]])
  names(activitylabelstrain) <- "activity"
  
  x_test2 <- x_test[,grep("std|mean\\()",features[,2])]
  
  names(x_test2) <- tolower(sub("\\()","",features[,2][grep("std|mean\\()",features[,2])]))
  
  x_train2 <- x_train[,grep("std|mean\\()",features[,2])]
  
  names(x_train2) <- tolower(sub("\\()","",features[,2][grep("std|mean\\()",features[,2])]))
  
  
# Merge Data
  x_train3 <- cbind(subject_train, activitylabelstrain, x_train2)
  names(x_train3)[2] <- "activity"
  
  x_test3 <- cbind(subject_test, activitylabelstest, x_test2)
  names(x_test3)[2] <- "activity"
  
  tidydata <- rbind(x_train3,x_test3)
  data <- melt(tidydata, id.vars=c("subject","activity"))
  data <- dcast(data, subject + activity ~ variable, mean)
  
  write.table(tidydata,"tidydataset_large.txt")
  write.table(data,"tidydataset_small.txt")
  data
}