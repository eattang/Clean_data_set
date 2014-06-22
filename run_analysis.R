require(plyr)

#create the data directory if it does not exist
if(!file.exists("./data")){
  dir.create("./data")
}

delete_white_space<-function(x){
  require(stringr)
  str_<-NULL
  for (i in seq_along(x)){
    str_<-paste0(str_,x[i])
    str_<-str_trim(str_)
  }
  return(str_)
}
#read the features table to replace the column names
#the features table is located in 'features.txt' file

features_<-read.table("./data/UCI HAR Dataset/features.txt")
#subset the second column just to obtain the features name
features_<-features_[,2]
#Read the activity labels
activity_<-read.table("./data/UCI HAR Dataset/activity_labels.txt")

activity_<-as.character(activity_[,2])
#Read subject_test

subject_test<-read.table("./data/UCI HAR Dataset/test/subject_test.txt")

#Read subject_train
subject_train<-read.table("./data/UCI HAR Dataset/train/subject_train.txt")
#read X_test
X_test<-read.table("./data/UCI HAR Dataset/test/X_test.txt")

#read X_train
X_train<-read.table("./data/UCI HAR Dataset/train/X_train.txt")

#read y_test as a data frame
y_test<-read.table("./data/UCI HAR Dataset/test/y_test.txt")

#read y_train as a data frame
y_train<-read.table("./data/UCI HAR Dataset/train/y_train.txt")


#replace the names of X_test with the features
names(X_test)<-features_

#we need to replace the y_test with the associated activity labels
y_test2<-y_test[,1]
for (i in seq_along(activity_)){
  y_test2[y_test2==i]<-activity_[i]
}
y_test2<-factor(y_test2)

y_train2<-y_train[,1]
for (i in seq_along(activity_)){
  y_train2[y_train2==i]<-activity_[i]
}
y_train2<-factor(y_train2)


#Now merge all the test data
test_df<-data.frame(y_test2,subject_test,X_test)
names(test_df)[1:2]<-c("activity","subject")



#merge all the train data
train_df<-data.frame(y_train2,subject_train,X_train)
names(train_df)[1:2]<-c("activity","subject")

#merging the test and training data set
new_dataframe<-join(test_df,train_df,type="full")


#get the names of the new data_frame
names_dataframe<-names(new_dataframe)

#perform some processing on the names_dataframe variable 

names_dataframe<-strsplit(names_dataframe,"\\.")

names_dataframe<-sapply(names_dataframe,delete_white_space)

names(new_dataframe)<-names_dataframe
#we have been able to delete the white-spaces and dots in the names

#we find the columns associated with the mean and std and subset them

#this is done using regular expressions
regex<-"mean|std"

#the indices variable gives the indices of the variables where a match
indices<-grep(regex,names_dataframe)
#1st and 2nd columns of new_dataframe are retained
sub_dataframe<-new_dataframe[,c(1,2,indices)]

#next filter out meanFreq() which is not a computative variable of interest
names_sub_dataframe<-names(sub_dataframe)
indices2<-grep("meanFreq()",names_sub_dataframe)
indices2<--1*indices2
sub_dataframe<-sub_dataframe[,indices2]

#to find the summary statistics, we use ddply function from plyr

require(plyr)

indices_mean<-grep("mean",sub_dataframe)
indices_std<-grep("std",sub_dataframe)

#For example to summarise data by any of the column indices say indices_mean[4]

specific_column<-names(sub_dataframe)[4]


res<-ddply(sub_dataframe,names(sub_dataframe)[1:2],mean_=mean(specific_column))









  