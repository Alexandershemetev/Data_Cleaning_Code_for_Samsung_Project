###################################################################
#Stage 0. Downloading and unzipping dataset
###################################################################
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# Code book for variables: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
download.file(fileUrl1, destfile = "C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset.zip", method = "curl")
unzip(zipfile="C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset.zip",exdir="./getdata_projectfiles_UCI HAR Dataset")

# You should create one R script called run_analysis.R that does the following.
# Done - this is the name of our file

###################################################################
#Stage 1.Merges the training and the test sets to create one data set.
###################################################################

# 1.1 Reading files
# data tables names are the same as the names of the original files - in order not to mix the names 
# 1.1.1  Reading trainings tables:

x_train <- read.table("C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\X_train.txt")
y_train <- read.table("C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\y_train.txt")
subject_train <- read.table("C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\train\\subject_train.txt")

# 1.1.2 Reading testing tables:
x_test <- read.table("C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\X_test.txt")
y_test <- read.table("C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\y_test.txt")
subject_test <- read.table("C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\test\\subject_test.txt")

# 1.1.3 Reading feature vector:
features <- read.table('C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\features.txt')

# 1.1.4 Reading activity labels:
activity_labels = read.table('C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\activity_labels.txt')

# 1.1.5. Getting to know the data: 
str(x_train) #Good match with x_test - 99 variables outputted = 561 variable 
str(y_train) #Y train and Subject train consist of SAME Nobs but have 1 different variable each ++ Good to y_test 
str(subject_train) #* ++ good to subject test 
str(x_test) #*
str(y_test) #* ++ good for subject test 
str(subject_test) #* 
str(features) # 2 Variables: some output data from phone + human language explained  561 obs.
str(activity_labels) #6 types of activities in codes 1-6 + human language names of activities 
# Conclusion 561 = 561 => features contain himan readable column names - we will assign them 



# 1.2 Assigning column names:
# Starting with the train data tables: 
colnames(x_train) <- features[,2]
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"
# Same for the test data tables: 
colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"
# We discussed the activity_labels data table - renaming its column names: 
colnames(activity_labels) <- c('activityId','activityType')

#1.3 Merging all data in one set:

# We will have human-OK names of activities in the datatset: 
y_train_label <- left_join(y_train, activity_labels, by = "activityId")
str(y_train_label) #Looks great! 
# Now we need same number of columns for the test datatset with human redable labels in order to merge it later without any problems: 
y_test_label <- left_join(y_test, activity_labels, by = "activityId")
 str(y_test_label) #Looks great! 

# Now the main merging: 
mrg_train <- cbind(y_train_label, subject_train, x_train) #All trains - they have same Nobs 
mrg_test <- cbind(y_test_label, subject_test, x_test)           #All tests  - they have same Nobs 
setAllInOne <- rbind(mrg_train, mrg_test)
### Verifying the data is made OK: 
str(setAllInOne)
summary(setAllInOne)
dim(setAllInOne)
#[1] 10299   563
### Looks terrific! All the dataframes are used - no data is lost. 

###################################################################
#Stage 2.-Extracts only the measurements on the mean and standard deviation for each measurement.
###################################################################

#2.1 Reading column names:

colNames <- colnames(setAllInOne)
View(colNames) #This line may be omitted - but it is good to have column names in front of your eyes 
#2.2 Create vector for defining ID, mean and standard deviation:
# activityType - human readable replicator of activity IDs 
mean_and_std <- (grepl("activityId" , colNames) | #Preserve activity ID 
                   grepl("subjectId" , colNames) | #Preserve Subject ID 
				   grepl("activityType" , colNames) | #Preseve Human Readable Activity ID 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)
View(mean_and_std) #Returns a TRUE/FALSE vector for each column name - if it contains the mentioned in grepl word patterns in column names 
#2.3 Making nessesary subset from setAllInOne:

my_means_and_std_devs <- setAllInOne[ , mean_and_std == TRUE]
str(my_means_and_std_devs) #Terrific! Everything we may want to preserve - is preserved 
dim(my_means_and_std_devs)

###################################################################
#Stage 3. Uses descriptive activity names to name the activities in the data set
###################################################################
### This is already done in part 1.3 - the first half - str reveals we have it yet: 
#str(my_means_and_std_devs)
#'data.frame':   10299 obs. of  82 variables:
# $ activityId                     : int  5 5 5 5 5 5 5 5 5 5 ...
# $ activityType                   : chr  "STANDING" "STANDING" "STANDING" "STANDING" ... ### This is this column we are asked to do 
# $ subjectId                      : int  1 1 1 1 1 1 1 1 1 1 ...
# $ tBodyAcc-mean()-X              : num  0.289 0.278 0.28 0.279 0.277 ...
# $ tBodyAcc-mean()-Y              : num  -0.0203 -0.0164 -0.0195 -0.0262 -0.0166 ...

### But we can do it one more time: 
human_readable_DS <- merge(my_means_and_std_devs, activity_labels,
                              by='activityId',
                              all.x=TRUE)
str(human_readable_DS) #in this DF activityType column doesn't exist - now we have 2 same columns with the same data: activityType.x and activityType.y 
# This is made for test - in real life even without this this task would be performed before in advance. 
###################################################################
#Stage 4. Appropriately labels the data set with descriptive variable names.
###################################################################

#Done in previous Stages, see 1.3,2.2 and 2.3 and in other steps! We even have duplicated column for labels in one of the DF in order if in some experiment one column will be damaged - another one will be restored then. 

###################################################################
#Stage 5. From the data set in Stage 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
###################################################################

#5.1 Making a second tidy data set
# For this purpose we will use the function called aggregate: 
my_second_tidy_set <- aggregate(. ~subjectId + activityId, human_readable_DS, mean)
View(my_second_tidy_set) #Not good - all columns are NAs - we will use another method for aggregating - surrogate key 
human_readable_DS$pf <- paste(human_readable_DS$subjectId, human_readable_DS$activityId, human_readable_DS$activityType.x, sep = "_")
human_readable_DS$pf[1:100] #Test OK 
# Now Aggregate with the surrogate key: 
my_second_tidy_set = aggregate(human_readable_DS, list(human_readable_DS$pf), mean, na.action = na.omit, na.rm=TRUE)
View(my_second_tidy_set) #Done - all the variables are the means as is requested - no NAs in numerical cells. 
my_second_tidy_set_2 = my_second_tidy_set #copy of DS in case if reverse separation will go wrong: 
my_second_tidy_set_2 <- my_second_tidy_set_2 %>% separate(Group.1, c("Individual_ID", "Action_ID", "Action_Value"), "_", extra = "merge")
## Success!!! Our dataframe is splitted and made like it is supposed to be - the unified key cell (pf) is not anymore valid, but it preserved all the information we could ever want to be preserved in our dataset 
my_second_tidy_set_3 <- my_second_tidy_set_2[order(my_second_tidy_set_2$Individual_ID, my_second_tidy_set_2$Action_ID),]
View(my_second_tidy_set_3) #It nothing changes drammatically, but if our DF would be non-ordered - this would order it. 
#5.2 Writing second tidy data set in txt file (as an example - in RDS too)

write.table(my_second_tidy_set_2, "C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\my_second_tidy_set_2.txt", row.name=FALSE)
saveRDS(my_second_tidy_set_3, file = "C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\CLEANING_THE_DATA\\FINAL_PROJECT\\my_second_tidy_set_2.rds")







