###0. Download and prepare before doing the task
# I set the working directory to where the data files are located in my computer:
setwd("C:/Users/User/Desktop/RStudio/datascience")

# Load necessary packages (in this case just the dplyr)
library(dplyr)

# Read all the the datasets (read README.txt in case of doubt)
features <- read.table("features.txt")
activities <- read.table("activity_labels.txt")
subject_train <- read.table("train/subject_train.txt")
X_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_test <- read.table("test/subject_test.txt")
X_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

###1. Merge the training and test datasets
subject <- rbind(subject_train, subject_test)
X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
###4. Appropriately labels the data set with descriptive variable names
# Label the data set with descriptive variable names using features from features.txt
names(subject) <- "subject"
names(y) <- "activity"
names(X) <- features[,2]
# Merge subject, activity, and measurements into one data set
merged_data <- cbind(subject, y, X)

###2. Extract only the measurements on the mean and standard deviation
mean_std_columns <- grep("-(mean|std)\\(\\)", features[,2])
data_with_mean_std <- merged_data[, c(1,2,mean_std_columns+2)]

###3. Use descriptive activity names to name the activities from activity_labels.txt
data_with_mean_std$activity <- activities[data_with_mean_std$activity, 2]

###4. Create a second, independent tidy data set with the average of each variable
# for each activity and each subject
tidy_data <- data_with_mean_std %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean))

# Write the tidy data set to a text file
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)

