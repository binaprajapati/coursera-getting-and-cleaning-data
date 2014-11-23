# Source of data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# This R script does the following:
# 1. Merges the training and the test sets to create one data set.

X <- rbind(X_train, X_test)

S <- rbind(subject_train,subject_test)

Y <- rbind(y_train,y_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))  # see last slide of the lecture Editing Text Variables (week 4)

# 3. Uses descriptive activity names to name the activities in the data set

activity_labels[, 2] = gsub("_", "", tolower(as.character(activity_labels[, 2])))
Y[,1] = activity_labels[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activity_labels[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activity_labels[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activity_labels[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt")

# res2 <- read.table("data_set_with_the_averages.txt")
# result[4,4]
# res2[4,4]
# res2[4,4]==result[4,4]
# result[6,4]
# res2[6,4]
# res2[6,4]==result[6,4]