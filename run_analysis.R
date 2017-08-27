##setwd("phonedata")
##
##load libraries
##
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyverse)
##
## read the activity labels and rename them 
alabels <- read.table("activity_labels.txt")
alabels$V2 <- c("walking", "walkingupstairs","walkingdownstairs","sitting","standing","laying")
##
##alabels$V2
##
##
## read the features (original variable names)
##
features <- read.table("features.txt")
##
## read the ID for subjects in the Test and Train dataset
##
subject_test <- read.table("subject_test.txt")
subject_train <- read.table("subject_train.txt")
##
## read the Test and Train datasets
##
X_test <- read.table("X_test.txt")
X_train <- read.table("X_train.txt")
##
## read the Test and Train activity IDs
##
y_test <- read.table("y_test.txt")
y_train <- read.table("y_train.txt")
##
## combine the training data and prepend a column containing the string "train"
##
new_train <- cbind("train",subject_train,y_train,X_train)
##
## combine the test data and prepend a column containing the string "test"
##
new_test <- cbind("test",subject_test,y_test,X_test)
##
##
## prepend label strings to original variable names and apply as column names to new_test and new_train datasets
##
cname1 <- as.character(features$V2)
cnames <- c("dataset","subject","activity",cname1)
colnames(new_train) <- cnames
colnames(new_test) <- cnames
##
##  Merge the test and training datasets
##
new_both <- rbind(new_train,new_test)
##
## extract the columns with "mean()" and "std()"
## originally tried 'select' and 'match' functions but encountered error with duplicate column names 
## found discussion of this error on Stack Overflow (item 28549045)
## rather than fix ducplicates for columns that are to be removed, I grepped "mean\\(" and "std\\(" and kept them
##
grep("keep", cname1)  ## checking to see whether "keep" is already being used
cnames[1:3] <- paste("keep", cnames[1:3],sep="")
good_cols <- grep("keep|mean\\(|std\\(", cnames)
subset_1 <- new_both[, good_cols]
cnames <- colnames(subset_1)
##
##
## replace 1-6 with activity labels
## method based on stackoverflow.com; item 21712384
##
tempvec  <- alabels[match(subset_1$activity, alabels$V1),2]
subset_1$activity <-tempvec
##
## clean up column names prior to tidying
## fix "BodyBody" error caused by previous replacement
## change t and f to "time" and "freq" (for simplicity, remove "t" from "std")
## move "magnitude" to end (like X, Y, Z)
## change "Gravity" to "Grav" to be same length as "Body"
## change "Gyro" to "Gyr" to be same length as "Acc"
## reduce length of "Jerk" variables to "Acj" and "Grj"
## make all names lowercase
##
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

##
## apply names to dataset
gnames <- c("dataset","subject","activity",gnames)
colnames(subset_1) <- gnames
## 
## stack the data by variable
##
## add unique id or "separate" will not work properly
##
nsubjects <- nrow(subset_1)
unique <- c(1:nsubjects)
subset_2 <- cbind(unique,subset_1)
subset_3 <- melt(subset_2,id=1:4)
##
## separate "variable" column by direction (and magnitude)
##
subset_4 <- subset_3 %>% separate(variable, c("variable","direction"),"_")
##
## unstack columns by variable
##
subset_5 <- spread(subset_4,"variable","value")
##
## convert characters to factors
subset_5$activity <- as.factor(subset_5$activity)
subset_5$direction <- as.factor(subset_5$direction)
##
## rename directions from x,y,z,m to xdirection, ydirection, zdirection, and magnitude
##
subset_5$direction <- mapvalues(subset_5$direction, from = c("x", "y","z","m"), to = c("xdirection", "ydirection","zdirection","magnitude"))
##
##
## expand variable names
##
gnames <- colnames(subset_5)
gnames <- sub("sdev","stdev",gnames)
gnames <- sub("acj","accjerk",gnames)
gnames <- sub("gyj","gyrjerk",gnames)
gnames <- sub("acc","accelerometer",gnames)
gnames <- sub("gyr","gyroscope",gnames)
gnames <- sub("grav","gravity",gnames)
colnames(subset_5) <- gnames
##
## create average table
##
ave_data <- subset_5 %>% group_by_at(vars("subject","activity","direction")) %>% summarize_at(vars(freqbodyaccelerometermean:timegravityaccelerometerstdev),mean)

##                    
                    
                    
                    
                    
