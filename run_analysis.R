library(tidyverse)

# script assumes that the zip file file has been downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# and unzipped in the directory in which this script exists
# This is of course possible to do with 
# download.file(the_url, name_of_file.zip) 
# unzip(name_of_file.zip)

BASE.FOLDER <- "UCI HAR Dataset" # base folder created when unzipping

features <- read_delim(file.path(BASE.FOLDER, "features.txt"), 
                       delim = " ",
                       col_names = c("num", "feature"),
                       col_types = cols(
                         num = col_integer(),
                         feature = col_character()))

# we only want "features" that are mean and/or std deviation
#indices_of_features_we_want <- grep("(tBodyAcc|tBodyGyro)-(mean|std)", features$feature)
indices_of_features_we_want <- grep("(mean|std)", features$feature)
features_we_want <- features$feature[indices_of_features_we_want]

# calling read_delim with string with _ in ith place ignores the ith column, if d is
# in the ith column it parses the coumn as a "double
col_X <- rep("_", nrow(features))
col_X[indices_of_features_we_want] <- "d"
col_X_str <- paste(col_X, collapse = "")

# read the sall data frame that maps activity numbers to activity names
activities <- read_delim(file.path(BASE.FOLDER, "activity_labels.txt"), 
                        delim=" ",
                        col_names = c("num", "activity"),
                        col_types = cols(
                          num = col_integer(),
                          activity = col_character()))

### 
# read the training data
TRAIN.FOLDER <- file.path(BASE.FOLDER, "train")
# read only the columns we want and name the features appropriately
X_train <- read_table(file.path(TRAIN.FOLDER, "X_train.txt"), 
                      col_types = col_X_str, 
                      col_names = features_we_want)
# read the labels associated with each observation
y_train <- read_table(file.path(TRAIN.FOLDER, "y_train.txt"), 
                      col_names = c("activity_number"),
                      col_types = cols(
                        activity_number = col_integer()
                      ))
# get the activity names
y_train$activity <- factor(activities$activity[y_train$activity_number], levels = activities$activity)

# read the subjects associated with each observation
train_subjects <- read_table(file.path(TRAIN.FOLDER, "subject_train.txt"),
                             col_names = "subject",
                             cols(
                               subject = col_integer()
                             ))
# bind together the columns we want
train_tidy <- bind_cols(train_subjects, select(y_train, activity), X_train)

###
# read the test data - same steps as for training data
TEST.FOLDER <- file.path(BASE.FOLDER, "test")
X_test <- read_table(file.path(TEST.FOLDER, "X_test.txt"), 
                     col_types = col_X_str, 
                     col_names = features_we_want)
y_test <- read_table(file.path(TEST.FOLDER, "y_test.txt"), 
                      col_names = c("activity_number"),
                      col_types = cols(
                        activity_number = col_integer()
                      ))
y_test$activity <- factor(activities$activity[y_test$activity_number], levels = activities$activity)
test_subjects <- read_table(file.path(TEST.FOLDER, "subject_test.txt"),
                             col_names = "subject",
                             cols(
                               subject = col_integer()
                             ))
test_tidy <- bind_cols(test_subjects, select(y_test, activity), X_test)

# bind the training and test data 
tidy_long <- bind_rows(train_tidy, test_tidy)

# now summarize
tidy_summary <- tidy_long %>% group_by(subject, activity) %>% summarise_all(funs(mean))



