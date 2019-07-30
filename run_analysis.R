##getting and cleaning data - final project
library(dplyr)
library(foreign)

##load training and test sets
files <- c("X","y","subject")
training_file_names <- file.path("./train",  paste(files, "_train.txt", sep=""))
test_file_names <- file.path("./test", paste(files, "_test.txt", sep=""))
training_files <- lapply(training_file_names, read.table)
test_files <- lapply(test_file_names, read.table)

##merge the training and the test sets to create one data set
training_data <- bind_cols(training_files)
test_data <- bind_cols(test_files)
data <- rbind(training_data,test_data)

##load features and selects only those containing 'mean()' or 'std()'
features <- read.table("./features.txt")
features_filtered <- grep("mean()|std()",features$V2, value = FALSE)

#extract only the measurements on the mean and standard deviation for each measurement and the subject and activity columns
new_data <- cbind(data[,features_filtered], data$V1100, data$V1101)

#select labels names only for measurements on the mean and standard deviation
feature_names <- features[features_filtered,]

#substitute label names by more descriptive ones
feature_names <- gsub('^t', 'Time', feature_names$V2)
feature_names <- gsub('Freq', 'Frequency', feature_names)
feature_names <- gsub('^f', 'Frequency', feature_names)
feature_names <- gsub('-mean', 'Mean', feature_names)
feature_names <- gsub('-std', 'StandardDeviation', feature_names)
feature_names <- gsub('\\(|\\)', '', feature_names)

#appropriately label the data set with descriptive variable names.
colnames(new_data) <- c(feature_names,"Activity", "Subject")

#move activity and subject columns to the front
new_data <- new_data %>% select(Subject, Activity, everything())

#use descriptive activity names to name the activities in the data set
new_data$Activity <- ifelse(new_data$Activity==1, "WALKING", 
                         ifelse(new_data$Activity==2, "WALKING_UPSTAIRS", 
                                ifelse(new_data$Activity==3, "WALKING_DOWNSTAIRS", 
                                       ifelse(new_data$Activity==4, "SITTING", 
                                              ifelse(new_data$Activity==5, "STANDING", "LAYING")))))

#create a second, independent tidy data set with the average of each variable for each activity and each subject.
mean_subject_activity <- new_data %>% 
  group_by(Subject, Activity) %>%
  summarize_each(list(mean))

#output table
write.table(mean_subject_activity, "tidy_data.txt", row.names = FALSE, 
                      quote = FALSE)
