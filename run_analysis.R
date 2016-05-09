library(dplyr)
library(data.table)
library(tidyr)

#1.Merges the training and the test sets to create one data set.
#set file path & read data
subject.train <- tbl_df(read.table("D:/Coursera/Clean data/Assignment/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt"))
x.train <- tbl_df(read.table("D:/Coursera/Clean data/Assignment/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/x_train.txt"))
y.train <- tbl_df(read.table("D:/Coursera/Clean data/Assignment/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt"))

subject.test <- tbl_df(read.table("D:/Coursera/Clean data/Assignment/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt"))
x.test <- tbl_df(read.table("D:/Coursera/Clean data/Assignment/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/x_test.txt"))
y.test <- tbl_df(read.table("D:/Coursera/Clean data/Assignment/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt"))

#merge datasets
merge.data1 <- rbind(subject.train, subject.test)
setnames(merge.data1, "V1", "subject")
merge.data2 <- rbind(y.train, y.test)
setnames(merge.data2, "V1", "activityNum")
merge.data3 <- rbind(x.train, x.test)


#names
Features <- tbl_df(read.table("D:/Coursera/Clean data/Assignment/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt"))
setnames(Features, names(Features), c("featureNum", "featureName"))
colnames(merge.data3) <- Features$featureName

ActivityLabels<- tbl_df(read.table("D:/Coursera/Clean data/Assignment/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt"))
setnames(ActivityLabels, names(ActivityLabels), c("activityNum","activityName"))



merge.data <- cbind(merge.data1, merge.data2, merge.data3)



#---------------------------------------------------------------------------------------------------------------------------------
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
FeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",Features$featureName,value=TRUE) 
FeaturesMeanStd <- union(c("subject","activityNum"), FeaturesMeanStd)
#sub.merge.datea <- merge.data()
sub.merge.data<- subset(merge.data, select = colnames(merge.data) %in% FeaturesMeanStd) 


#---------------------------------------------------------------------------------------------------------------------------------
#3. Uses descriptive activity names to name the activities in the data set
sub.merge.data<- merge(ActivityLabels, sub.merge.data , by = "activityNum", all.x=TRUE)
sub.merge.data$activityName <- as.character(sub.merge.data$activityName)
agg.data <- aggregate(. ~ subject - activityName, data = sub.merge.data, mean) 
agg.data2<- tbl_df(arrange(agg.data,subject,activityName))


#---------------------------------------------------------------------------------------------------------------------------------
#4. Appropriately labels the data set with descriptive variable names.
names(agg.data2)<-gsub("std()", "SD", names(agg.data2))
names(agg.data2)<-gsub("mean()", "MEAN", names(agg.data2))
names(agg.data2)<-gsub("^t", "time", names(agg.data2))
names(agg.data2)<-gsub("^f", "frequency", names(agg.data2))
names(agg.data2)<-gsub("Acc", "Accelerometer", names(agg.data2))
names(agg.data2)<-gsub("Gyro", "Gyroscope", names(agg.data2))
names(agg.data2)<-gsub("Mag", "Magnitude", names(agg.data2))
names(agg.data2)<-gsub("BodyBody", "Body", names(agg.data2))
# Names after
head(str(agg.data2),6)

#---------------------------------------------------------------------------------------------------------------------------------
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

write.table(agg.data2, "D:Coursera/Clean data/Assignment/TidyData.txt", row.name=FALSE)
