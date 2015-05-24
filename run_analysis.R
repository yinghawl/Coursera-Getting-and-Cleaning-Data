###Step1###
#Define the working directory 
setwd("C://Users//YingHaw//Documents//R//Coursera Getting and Cleaning Data//Project 1")
require (reshape2)
require (plyr)
require (dplyr)

#Read train data into R and add a new "Type" column 
classes<-c(rep("numeric",561))
Train<-cbind(read.table("./UCI HAR Dataset/train/subject_train.txt", colClasses = "character"),read.table("./UCI HAR Dataset/train/Y_train.txt", colClasses = "character"),read.table("./UCI HAR Dataset/train/X_train.txt", colClasses = classes))

#Read test data into R 
Test<-cbind(read.table("./UCI HAR Dataset/test/subject_test.txt", colClasses = "character"),read.table("./UCI HAR Dataset/test/Y_test.txt", colClasses = "character"),read.table("./UCI HAR Dataset/test/X_test.txt", colClasses = classes))

#Combine both the train and test data
rawdata<-rbind(Train,Test)

###Step2###
# Calculate the statistics (mean and standard deviation) for each measurement

mdata<-rawdata[3:563]
mdata<-as.matrix(mdata)
mdata<-t(mdata)
mdata<-as.data.frame(mdata)
feature<-read.table("./UCI HAR Dataset/features.txt",colClasses="character")
mdata<-cbind(mdata,feature[,2])

# Extract only the mean data. 
dmean<-mdata[grepl("mean" ,mdata[,ncol(mdata)]),];dmean<-dmean[!grepl("meanFreq" ,dmean[,ncol(dmean)]),]

# Extract only the standard deviation data
std<-mdata[grepl("std",mdata[,ncol(mdata)]),]

# Combine both data
cdata<-rbind(dmean,std)

# Tranpose the data back to original form
cdata<-as.matrix(cdata)
cdata<-t(cdata)
cdata<-as.data.frame(cdata)

# This is supposed to be part of the assignment from step 4; I did it here for simplicity
colnames(cdata)<-as.character(unlist(cdata[10300,]))
subcond<-rawdata[,1:2];colnames(subcond)<-c("Subject","Num.Cond")

#Combine the data with the subject and y column; I selected the first two columns of rawdata and the first 10299 rows of cdata(purpose exlude the last row which is the name for the variables)
cdata<-cbind(subcond,cdata[1:10299,])

###Step3###
# Create a new table with the activity name and the corresponding notation number
activitylabels<-read.table("./UCI HAR Dataset/activity_labels.txt",colClasses=c("numeric","character"),col.names=c("Num.Cond","Condition"))

# Merge the table with the original table
cdata<-merge(cdata,activitylabels,by="Num.Cond")

# Exclude the original Num.Cond column and order the data by Subject
cdata<-cdata %>% arrange(Subject) %>% select(-Num.Cond)

###Step 4###
# Name all the other variables; y-test=condition
# I believe it has been done. 

##Step5##
# Summarize the result per subject , condition and type; Use groupby() and summarize() 
result<-melt(cdata,id.vars=c("Subject","Condition"))
result$value<-as.numeric(result$value)
result<-dcast(result,Subject + Condition~variable,mean) # Wide format
result2<-melt(result,id.vars=c("Subject","Condition")) # Long format

# Make a new txt file of the tidy dataset using long format
write.table(result2,file="Getting and Cleaning Data Project 1 Tidy Data set.txt",row.names=FALSE)





