
#######################################################
### Getting and Cleaning Data --- COURSE PROJECT #####
#####################################################



run_analysis <- function() {
  # Download and extract data in the "data" directory
  FileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  data.directory <- "sourcedata"
  FileZIP <- paste(data.directory,"Dataset.zip",sep="/")
  if(!file.exists(data.directory)) { dir.create(data.directory) }
  
  #Download file if it had not been downloaded before
  if(!file.exists(FileZIP)) { download.file(FileURL, FileZIP, method = "curl") }
  
  #Extract data if it had not been extractec before
  data.directory <- "UCI HAR Dataset"
  if(!file.exists(data.directory)) { unzip(FileZIP, exdir = ".") }
  
  readData <- function(data.directory,path) {
    read.table(paste(data.directory, path, sep="/"))
  }
  # 1. Merges the training and the test sets to create one data set.
  trainingSet <- readData(data.directory,"train/X_train.txt")
  testSet <- readData(data.directory,"test/X_test.txt")
  mergeSet <- rbind (trainingSet, testSet)
  
  # read names of Features
  namesFeatures <- readData(data.directory,"features.txt")
  names(mergeSet) <- as.character(namesFeatures[,2])

  # 2. Extracts only the measurements on the mean and standard deviation
  #    for each measurement.
  # Select only the significative features (mean and standard deviation)
  # Column names which match mean() or std():
  
  selectedFeatures <- grep("(mean|std)\\(\\)", names(mergeSet))
  mergeSelection <- mergeSet[, selectedFeatures]
  
  # 3. Uses descriptive activity names to name the activities in the data set
  
  actTrain <- readData(data.directory,"train/y_train.txt")
  actTest  <- readData(data.directory,"test/y_test.txt")
  actMerged <- rbind(actTrain, actTest)[, 1]
  actNames <- readData(data.directory,"activity_labels.txt")
  activityNames <- as.character(actNames[, 2])
  activities <- activityNames[actMerged]
  
  # 4. Appropriately labels the data set with descriptive variable names. 
  
  # Changes in labels:
  #   t -> Time,
  #   f -> Freq,
  #   mean() -> Mean
  #   std() -> StdDev
  #   Extra dashes erased
  #   BodyBody -> Body
  names(mergeSelection) <- gsub("^t", "Time", names(mergeSelection))
  names(mergeSelection) <- gsub("^f", "Freq", names(mergeSelection))
  names(mergeSelection) <- gsub("-mean\\(\\)", "Mean", names(mergeSelection))
  names(mergeSelection) <- gsub("-std\\(\\)", "StdDev", names(mergeSelection))
  names(mergeSelection) <- gsub("-", "", names(mergeSelection))
  names(mergeSelection) <- gsub("BodyBody", "Body", names(mergeSelection))
  
  #Merge colums of subjects and activities to the mergeSelection set.
  #Both colums are correctly named (subject and activity)
  subjectTrain <- readData(data.directory,"train/subject_train.txt")
  subjectTest  <- readData(data.directory,"test/subject_test.txt")
  subjects <- rbind(subjectTrain, subjectTest)[, 1]
  
  tidySet <- cbind(Subject = subjects, Activity = activities, mergeSelection)
  
  
  # 5. From the data set in step 4, creates a second, independent tidy data set
  #   with the average of each variable for each activity and each subject.
  
  library("plyr")
  
  # Calculates mean for the colums with measures (all except subject and activity)
  
  tidySetMeans <- aggregate(. ~Subject + Activity, tidySet, mean)
  tidySetMeans <-tidySetMeans[order(tidySetMeans$Subject,
                                    tidySetMeans$Activity),]
  names(tidySetMeans)[-c(1,2)] <- paste0("Mean", names(tidySetMeans)[-c(1,2)])
  
  # Generates the file
  write.table(tidySetMeans, file = "tidydata.txt",row.name=FALSE)
  
  tidySetMeans  
}




