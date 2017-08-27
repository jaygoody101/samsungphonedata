# samsungphonedata
for Coursera Data Science Lesson 3; Week 4 Assignment
Objectives

1) Combine data from multiple source
2) Extract a subset of columns from the combined file
3) Transform into tidy format
4) Perform summarization as specified


-----

Data Description

The initial data consist of multiple files (all .txt) as folllows:

	activity_labels: contains encoding for ID to strings
	
	features: variable names

	Then for separate training and testing datasets:

	X_train and X_test: the raw data
	
	y_train and y_test: the activity IDs for rows in X_train and X_test

	subject_train and subject_test: the subject IDs for rows in X_train and X_test


-----

Preliminaries: Set working directory and load libraries

setwd("phonedata")

library(plyr)
library(dplyr)
library(stringr)
library(reshape2)
library(tidyverse)


-----

Read the data


alabels <- read.table("activity_labels.txt")
alabels$V2 <- c("walking", "walkingupstairs","walkingdownstairs","sitting","standing","laying")

features <- read.table("features.txt")

subject_test <- read.table("subject_test.txt")
subject_train <- read.table("subject_train.txt")

X_test <- read.table("X_test.txt")
X_train <- read.table("X_train.txt")

y_test <- read.table("y_test.txt")
y_train <- read.table("y_train.txt")

-----

Separately combine files for training and test data
I prepended "train" or "test" in case needed at a later time

new_train <- cbind("train",subject_train,y_train,X_train)
new_test <- cbind("test",subject_test,y_test,X_test)


-----

Apply column names

cname1 <- as.character(features$V2)
cnames <- c("dataset","subject","activity",cname1)
colnames(new_train) <- cnames
colnames(new_test) <- cnames

---

Merge the test and training datasets

new_both <- rbind(new_train,new_test)

-----

Extract the columns with "mean()" and "std()"

grep("keep", cname1)  ## checking to see whether "keep" is already being used
cnames[1:3] <- paste("keep", cnames[1:3],sep="")
good_cols <- grep("keep|mean\\(|std\\(", cnames)
subset_1 <- new_both[, good_cols]

-----

Tidy Dataset: Apply activity labels

tempvec  <- alabels[match(subset_1$activity, alabels$V1),2]
subset_1$activity <-tempvec

-----

Tidy Dataset: make column names same length for now for easier viewing and manipulation

gnames <- cnames[4:69]
gnames <- sub("BodyBody","Body",gnames)
gnames <- sub("\\(","",gnames)
gnames <- sub("\\)","",gnames)
gnames <- sub("Mag-mean","mean_m",gnames)
gnames <- sub("Mag-std","std_m",gnames)
gnames <- sub("std", "sdev",gnames)
gnames <- sub("t","time",gnames)
gnames <- sub("f","freq",gnames)
gnames <- sub("-","",gnames)
gnames <- sub("-","_",gnames)
gnames <- sub("Gravity","Grav",gnames)
gnames <- sub("Gyro","Gyr",gnames)
gnames <- sub("GyrJerk","GyJ",gnames)
gnames <- sub("AccJerk","AcJ",gnames)
gnames <- tolower(gnames)

gnames <- c("dataset","subject","activity",gnames)
colnames(subset_1) <- gnames

-----

Tidy Dataset: melt the dataset and separate XYZ directions; followed by unstacking by variable

nsubjects <- nrow(subset_1)
unique <- c(1:nsubjects)
subset_2 <- cbind(unique,subset_1)
subset_3 <- melt(subset_2,id=1:4)
subset_4 <- subset_3 %>% separate(variable, c("variable","direction"),"_")
subset_5 <- spread(subset_4,"variable","value")


Tidy Dataset: convert characters to factors and rename directions for meaningfulness 

subset_5$activity <- as.factor(subset_5$activity)
subset_5$direction <- as.factor(subset_5$direction)

subset_5$direction <- mapvalues(subset_5$direction, from = c("x", "y","z","m"), +
                                        to = c("xdirection", "ydirection","zdirection","magnitude"))

---

Tidy Dataset" Apply meaningful variable names

gnames <- colnames(subset_5)
gnames <- sub("sdev","stdev",gnames)
gnames <- sub("acj","accjerk",gnames)
gnames <- sub("gyj","gyrjerk",gnames)
gnames <- sub("acc","accelerometer",gnames)
gnames <- sub("gyr","gyroscope",gnames)
gnames <- sub("grav","gravity",gnames)
colnames(subset_5) <- gnames


---

Create objective average table

ave_data <- subset_5 %>% group_by_at(vars("subject","activity","direction")) %>% summarize_at(vars(freqbodyaccelerometermean:timegravityaccelerometerstdev),mean)

                    
                    
                    
                    
                    
