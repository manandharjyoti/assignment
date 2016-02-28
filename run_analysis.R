# Merges the training and the test sets to create one data set.
activitylabels <-  read.table("./UCI HAR Dataset/activity_labels.txt")
features <-  read.table("./UCI HAR Dataset/features.txt")    
subjecttrain <-  read.table("./UCI HAR Dataset/train/subject_train.txt")
subjecttest <-  read.table("./UCI HAR Dataset/test/subject_test.txt")
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
xtest <-  read.table("./UCI HAR Dataset/test/X_test.txt")
ytrain <-  read.table("./UCI HAR Dataset/train/y_train.txt")
ytest <-  read.table("./UCI HAR Dataset/test/y_test.txt")

subject <- rbind(subjecttest, subjecttrain)
x <- rbind(xtest, xtrain)
y <- rbind(ytest, ytrain)

# Appropriately labels the data set with descriptive variable names. 
names(subject) <- c("subject")
names(x) <- features$V2
names(y) <- c("activity")

# Extract only the measurements on the mean and standard deviation for each measurement. 
x  <- x[,grepl("mean|std", names(x))]

# Uses descriptive activity names to name the activities in the data set
data <- cbind(subject, y, x)
data$activity <- factor(data$activity,activitylabels[[1]],activitylabels[[2]])

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidydata <- aggregate(data, by=list(subject=data$subject, activity=data$activity), FUN=mean)
tidydata <- subset(tidydata, select = -c(3,4) ) # remove cols 3 and 4 
write.table(tidydata, file="tidy.txt", row.names=FALSE)

run_analysis <- function(){
  # load test data  
  subject_test = read.table("UCI HAR Dataset/test/subject_test.txt")
  X_test = read.table("UCI HAR Dataset/test/X_test.txt")
  Y_test = read.table("UCI HAR Dataset/test/Y_test.txt")
  
  # load training data
  subject_train = read.table("UCI HAR Dataset/train/subject_train.txt")
  X_train = read.table("UCI HAR Dataset/train/X_train.txt")
  Y_train = read.table("UCI HAR Dataset/train/Y_train.txt")
  
  # load lookup information
  features <- read.table("UCI HAR Dataset/features.txt", col.names=c("featureId", "featureLabel"))
  activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("activityId", "activityLabel"))
  activities$activityLabel <- gsub("_", "", as.character(activities$activityLabel))
  includedFeatures <- grep("-mean\\(\\)|-std\\(\\)", features$featureLabel)
  
  # merge test and training data and then name them
  subject <- rbind(subject_test, subject_train)
  names(subject) <- "subjectId"
  X <- rbind(X_test, X_train)
  X <- X[, includedFeatures]
  names(X) <- gsub("\\(|\\)", "", features$featureLabel[includedFeatures])
  Y <- rbind(Y_test, Y_train)
  names(Y) = "activityId"
  activity <- merge(Y, activities, by="activityId")$activityLabel
  
  # merge data frames of different columns to form one data table
  data <- cbind(subject, X, activity)
  write.table(data, "merged_tidy_data.txt")
  
  # create a dataset grouped by subject and activity after applying standard deviation and average calculations
  library(data.table)
  dataDT <- data.table(data)
  calculatedData<- dataDT[, lapply(.SD, mean), by=c("subjectId", "activity")]
  write.table(calculatedData, "calculated_tidy_data.txt")
}

