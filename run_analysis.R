# _______________
# run_analysis.R
# _______________
# You should create one R script called run_analysis.R that does the following. 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# get the data set

#Features
feature_names <- read.table(file=".\\UCI HAR Dataset\\features.txt", sep=" ")

#Activity names
activity_names <- read.table(file=".\\UCI HAR Dataset\\activity_labels.txt", sep=" ") ; names(activity_names) =c("ID","Activity")

#Subjects
subject_train <- read.table(file=".\\UCI HAR Dataset\\train\\subject_train.txt", sep=" ")
subject_test  <- read.table(file=".\\UCI HAR Dataset\\test\\subject_test.txt", sep=" ")
subject_total = rbind(subject_test,subject_train)

# Data Sets
dset_test <- read.table(file=".\\UCI HAR Dataset\\test\\X_test.txt", sep="")
dset_train <- read.table(file=".\\UCI HAR Dataset\\train\\X_train.txt", sep="")
dset_total = rbind(dset_test,dset_train)
label_test <- read.table(file=".\\UCI HAR Dataset\\test\\y_test.txt", sep="")
label_train <- read.table(file=".\\UCI HAR Dataset\\train\\y_train.txt", sep="")
label_total = rbind(label_test,label_train)

# Apply the Label the columns with the feature
names(dset_total) = feature_names[,2]

# From this combined data set - we will filter down to only the mean and standard deviation columns

# Identify the mean and standard deviation for each measurement
var_idx <- c(grep("mean\\(\\)",feature_names[,2]),grep("std\\(\\)",feature_names[,2]))
var_names <- c(grep("mean\\(\\)",feature_names[,2],value=TRUE),grep("std\\(\\)",feature_names[,2],value=TRUE))

# Filter the data set
dset_mean_std <- dset_total[,var_idx]

#Add the activity ID to the data set
names(label_total) = "ID"
dset_mean_std = cbind(dset_mean_std, label_total)

# Merge with the Descriptive Activity Name
dset_mean_std_activity <- merge(dset_mean_std, activity_names, by = "ID")

# Add the subject to the data set
names(subject_total) = "Subject"
dset_mean_std_activity_subject = cbind(dset_mean_std_activity, subject_total)

# now we have a big data set...
# let's tidy the data set
library(reshape2)
melt_ds <- melt(dset_mean_std_activity_subject,id=c("Activity", "Subject"), measure.vars=var_names)
tidy_ds <- dcast(melt_ds,Activity + Subject ~ variable, mean)

# write the tidy data et out to a file
write.table(tidy_ds,file="tidy_ds.txt",row.name=FALSE)