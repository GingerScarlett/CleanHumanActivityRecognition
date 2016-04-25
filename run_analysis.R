# Script for the "Getting Data" Project - April 24, 2016
# Lenette Laurente

# Read packages
library(dplyr)

# Read the column headings
f <- readLines("./rawdata/features.txt")
fnumcols <- length(f)                               # keep the number of features (columns) for use later
ff <-strsplit (f, " ")                              # separate the feature number from the name
secondElement <- function(x){x[2]}
features <- data.frame(sapply(ff, secondElement))   # keep only the name

# Read the activity names
anfile <- "./rawdata/activity_labels.txt"
an <- readLines(anfile)
ansplit <- strsplit(an, " ")
activitynames <- data.frame(matrix(unlist(ansplit), nrow=length(ansplit), byrow=T))
names(activitynames) = c("activityid", "activityname")
activitynames$activityid = as.integer(activitynames$activityid)

# Read the training data's 561-feature vector
xtrfile <- "./rawdata/train/X_train.txt"
xtr1 <- readLines(xtrfile)
xtr2 <- strsplit(xtr1, " +")
xtr3 <- data.frame(xtr2)
xtr3 <- xtr3[2:562,]
xtr4 <- as.data.frame(t(xtr3))
names(xtr4) <- features[,1]
doconvert <- function(x){as.numeric(as.character(x))}
xtr5 <- as.data.frame(sapply(xtr4, doconvert))

# Read the training data's activities
ytrfile <- "./rawdata/train/Y_train.txt"
ytr0 <- readLines(ytrfile)
ytr1 <- data.frame(as.integer(ytr0))
names(ytr1) <- c("activityid")

# Read the training subject ids
strfile <- "./rawdata/train/subject_train.txt"
str0 <- readLines(strfile)
str1 <- data.frame(as.integer(str0))
names(str1) <- c("subjectid")

# Merge all the training data
training <- bind_cols(xtr5, ytr1, str1)



# Read the test data
xtsfile <- "./rawdata/test/X_test.txt"
xts1 <- readLines(xtsfile)
xts2 <- strsplit(xts1, " +")
xts3 <- data.frame(xts2)
xts3 <- xts3[2:562,]
xts4 <- as.data.frame(t(xts3))
names(xts4) <- features[,1]
#xts4 <- as.numeric(xts4)
xts5 <- as.data.frame(sapply(xts4, doconvert))

# Read the testing data's activities
ytsfile <- "./rawdata/test/Y_test.txt"
yts0 <- readLines(ytsfile)
yts1 <- data.frame(as.integer(yts0))
names(yts1) <- c("activityid")

# Read the testing subject ids
stsfile <- "./rawdata/test/subject_test.txt"
sts0 <- readLines(stsfile)
sts1 <- data.frame(as.integer(sts0))
names(sts1) <- c("subjectid")

# Merge all the testing data
testing <- bind_cols(xts5, yts1, sts1)




# Merge the training and test sets to create one data set (Item 1)
# Appropriately label the data set with descriptive variable names (Item 4)
combinedhar <- bind_rows(training, testing)
combinedharfile <- "./combinedhar.csv"
write.csv (combinedhar, file=combinedharfile)

#Extract only the measurements on the mean and standard deviation for each measurement (Item 2)
combinedmeanstd <-  combinedhar[, grep("(.*)mean(.*)|(.*)std(.*)",colnames(combinedhar))]
combinedmeanstdfile <- "./meanstdactivitynames.csv"
write.csv (combinedmeanstd, file=combinedmeanstdfile)

# Use descriptive activity names to name the activities in the data set (Item 3)
allactivitynames = merge(combinedhar, activitynames, by.x="activityid", by.y="activityid", all=TRUE)
allactivitynamesfile <- "./combinedactivitynames.csv"
write.csv (allactivitynames, file=allactivitynamesfile)

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. (Item 5)
byactivity <- group_by (combinedhar, activityid)
byactivitysum <- summarize_each(byactivity, funs(mean))
byactivitysumfile <- "./byactivitysummary.csv"
write.csv (byactivitysum, file=byactivitysumfile)

bysubject <- group_by (combinedhar, subjectid)
bysubjectsum <- summarize_each(bysubject, funs(mean))
bysubjectsumfile <- "./bysubjectsummary.csv"
write.csv (bysubjectsum, file=bysubjectsumfile)

byactivitysubject <- group_by (combinedhar, activityid, subjectid)
byactivitysubjectsum <- summarize_each(byactivitysubject, funs(mean))
byactivitysubjectsumfile <- "./byactivitysubjectsummary.txt"
write.table(byactivitysubjectsum, file=byactivitysubjectsumfile, row.names = FALSE)

