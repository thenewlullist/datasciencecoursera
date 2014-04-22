##This assumes that the UCI HAR Dataset has been downloaded and extracted to a local directory which this script is running in

##We start with retrieving the txt files in the "UCI HAR Dataset/train" directory

train.set<-read.table("UCI HAR Dataset/train/X_train.txt")
train.Y<-read.table("UCI HAR Dataset/train/y_train.txt",col.names="activity")
sub.train<-read.table("UCI HAR Dataset/train/subject_train.txt",col.names="subject")
##add the subject and train.Y columns to the training set
train.set$subject <- sub.train
train.set$activity<- train.Y

##this is repeated for the test set in "UCI HAR Dataset/test"

test.set<-read.table("UCI HAR Dataset/test/X_test.txt")
test.Y<-read.table("UCI HAR Dataset/test/y_test.txt",col.names="activity")
sub.test<-read.table("UCI HAR Dataset/test/subject_test.txt",col.names="subject")

##we'll be using some functions from the plyr package so make sure plyr is running
library("plyr")
##the two sets are then joined
har.set<-join(train.set,test.set)
har.set$subject<-sub.train
har.set$activity<-train.Y
##extract the columns measuring mean and std of the variables
colnums<-c(563,1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,
           122,123,124,125,126,161,162,163,164,165,166,201,202,214,
           215,227,228,240,241,253,254,266,267,268,269,270,271,294,
           295,296,345,346,347,348,349,350,373,374,375,424,425,426,
           427,428,429,452,453,454,503,504,513,516,517,526,529,530,
           539,542,543,552)
##retrieve the respective column titles in features.txt and extract into a character vector 
titles<-c("activity","tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z",
          "tBodyAcc-std()-X","tBodyAcc-std()-Y","tBodyAcc-std()-Z",
          "tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z",
          "tGravityAcc-std()-X","tGravityAcc-std()-Y","tGravityAcc-std()-Z",
          "tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z",
          "tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y","tBodyAccJerk-std()-Z",
          "tBodyGyro-mean()-X","tBodyGyro-mean()-Y","tBodyGyro-mean()-Z",
          "tBodyGyro-std()-X","tBodyGyro-std()-Y","tBodyGyro-std()-Z","tBodyGyroJerk-mean()-X",
          "tBodyGyroJerk-mean()-Y","tBodyGyroJerk-mean()-Z","tBodyGyroJerk-std()-X","tBodyGyroJerk-std()-Y",
          "tBodyGyroJerk-std()-Z","tBodyAccMag-mean()","tBodyAccMag-std()","tGravityAccMag-mean()",
          "tGravityAccMag-std()","tBodyAccJerkMag-mean()","tBodyAccJerkMag-std()","tBodyGyroMag-mean()",
          "tBodyGyroMag-std()","tBodyGyroJerkMag-mean()","tBodyGyroJerkMag-std()","fBodyAcc-mean()-X",
          "fBodyAcc-mean()-Y","fBodyAcc-mean()-Z","fBodyAcc-std()-X","fBodyAcc-std()-Y","fBodyAcc-std()-Z",
          "fBodyAcc-meanFreq()-X","fBodyAcc-meanFreq()-Y","fBodyAcc-meanFreq()-Z","fBodyAccJerk-mean()-X",
          "fBodyAccJerk-mean()-Y","fBodyAccJerk-mean()-Z","fBodyAccJerk-std()-X","fBodyAccJerk-std()-Y",
          "fBodyAccJerk-std()-Z","fBodyAccJerk-meanFreq()-X","fBodyAccJerk-meanFreq()-Y","fBodyAccJerk-meanFreq()-Z",
          "fBodyGyro-mean()-X","fBodyGyro-mean()-Y","fBodyGyro-mean()-Z","fBodyGyro-std()-X","fBodyGyro-std()-Y",
          "fBodyGyro-std()-Z","fBodyGyro-meanFreq()-X","fBodyGyro-meanFreq()-Y","fBodyGyro-meanFreq()-Z",
          "fBodyAccMag-mean()","fBodyAccMag-std()","fBodyAccMag-meanFreq()","fBodyBodyAccJerkMag-mean()",
          "fBodyBodyAccJerkMag-std()","fBodyBodyAccJerkMag-meanFreq()","fBodyBodyGyroMag-mean()","fBodyBodyGyroMag-std()",
          "fBodyBodyGyroMag-meanFreq()","fBodyBodyGyroJerkMag-mean()","fBodyBodyGyroJerkMag-std()","fBodyBodyGyroJerkMag-meanFreq()")
##feed in the extracted columns vector and assign it to a new variable
full.set<-har.set[colnums]
##assign the new column titles to the default V1,V2...Vn ones
colnames(full.set) <- titles
##create a column for activity. replace numeric levels with corresponding activity label
activity<-as.factor(train.Y$activity)
levels(activity) <- c("Walking","Walking Upstairs","Walking Downstairs","Sitting","Standing","Lying Down")
##then add it to the combined data set 
full.set$activity <- activity
##finally, a separate tidy data set is created
tidy.set<-har.set[562]
tidy.set$activity <- activity
##the closest we can come to a concise list for the global averages as stated in features_info.txt are the 
##signal averages used in the angle() variable so the tidy set will be made based on the angle of those variables
means<-556:561
tidy.names<-c("subject","activity","tBodyAccJerkMean.gravityMean","tBodyGyroMean.gravityMean",
              "tBodyGyroJerkMean.gravityMean","X.gravityMean","Y.gravityMean","Z.gravityMean")
tidy.set[3:8]<-har.set[means]
colnames(tidy.set) <- tidy.names
##combine the gravityMean X,Y,and Z columns into one
gravityMean<-tidy.set[[6]]+tidy.set[[7]]+tidy.set[[8]]
tidy<-tidy.set[1:5]      
tidy$XYZ.gravityMean <- gravityMean
##arrange by subject and activity 
tidier<-arrange(tidy,subject,activity)     
##some subjects are missing from the final data set create a vector of ones that remain        
genlevels<-c(1,3,5:8,11,14:17,19,21:23,25:30)

##arrainge a for loop so each subject gets a data frame with the six activies. then print to a list

for(n in genlevels) {
      x<-aggregate(tidier[(tidier$subject == n),3:6], list(Activity=as.factor(tidier[(tidier$subject == n),]$activity)), mean)

      print(list(paste("Subject",n),x))}

