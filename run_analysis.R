
library(data.table)
ls("package:data.table")

library(dplyr)
ls("package:dplyr")

# Set working directory to the root of the data set
setwd("./UCI HAR Dataset")
list.files()

# activity labels to link the activities to their activity names
activ.lab <- scan("activity_labels.txt", what = "character", sep = "\n")
activ.lab <- substr(activ.lab, 3, nchar(activ.lab))

# feature names
features.raw <- scan("features.txt", what = "character", sep = "\n")
length(features.raw)
# Modify the feature names to more descriptive variable names
features <- gsub(" ", "_", features.raw)
features <-gsub("_t", "_Time_", features)
features <-gsub("_f", "_Freq_", features)
features <-gsub("_angle", "_AngleBtn", features)
features <-gsub("\\(\\)", "", features)


# list files in the directories in the data set root directory
dir.names <- list.dirs()
dir.names
list.files(dir.names[2], pattern = ".txt")
list.files(dir.names[3], pattern = ".txt")
list.files(dir.names[4], pattern = ".txt")
list.files(dir.names[5], pattern = ".txt")

####
# training set

# training set data
X.train <- fread(paste(dir.names[4], "X_train.txt", sep = "/"))
dim(X.train) # 7352  561

# training set activities
Y.train <- scan(paste(dir.names[4], "Y_train.txt", sep = "/"),
                what = "character", sep = "\n")
unique(Y.train)
length(Y.train) # 7352
# Link the training set activity class labels to their activity names
train.activity.lab <- sapply(as.numeric(Y.train), function(X) activ.lab[X])

# training set subjects
subject_train <- scan(paste(dir.names[4], "subject_train.txt", sep = "/"),
                      what = "character", sep = "\n")
length(subject_train) # 7352
unique(subject_train)

# training set data frame
train.df <- cbind(subject_train, train.activity.lab, X.train)
names(train.df) <- c("subject", "activity", features)


####
# test set

# test set data
X.test <- fread(paste(dir.names[2], "X_test.txt", sep = "/"))
dim(X.test) # 2947  561

# test set activities
Y.test <- scan(paste(dir.names[2], "Y_test.txt", sep = "/"),
               what = "character", sep = "\n")
unique(Y.test)
length(Y.test)
# Link the test set activity class labels to their activity names
test.activity.lab <- sapply(as.numeric(Y.test), function(X) activ.lab[X])

# test set subjects
subject_test <- scan(paste(dir.names[2], "subject_test.txt", sep = "/"),
                     what = "character")
length(subject_test) # 2947
unique(subject_test)

# test set data frame
test.df <- cbind(subject_test, test.activity.lab, X.test)
names(test.df) <- c("subject", "activity", features)


####
# Combine the training and test data frames
comb.df <- rbind(train.df, test.df)

# sort the dataframe by subject, then activity
comb.df <- arrange(comb.df, as.numeric(subject), activity)

# extract only means and SDs for each measurement (plus the first two columns)
sel.col <- c(1:2,
             which(regexpr("mean", names(comb.df)) > 0),
             which(regexpr("std", names(comb.df)) > 0))
sel.comb.df <- comb.df[, sel.col]

subjects <- unique(sel.comb.df$subject)
activities <- unique(sel.comb.df$activity)
tidy.df <- character(0)
for(subj in subjects)
  for(activ in activities)
    tidy.df <-
      rbind(tidy.df, 
            c(subj, activ,
              apply(filter(sel.comb.df[, -(1:2)],
                           sel.comb.df$subject == subj,
                           sel.comb.df$activity == activ),
                    MARGIN = 2, mean)))
tidy.df <- as.data.frame(tidy.df)
names(tidy.df)[1:2] <- c("subject", "activity")

write.csv(tidy.df, "tidy-df.csv")

