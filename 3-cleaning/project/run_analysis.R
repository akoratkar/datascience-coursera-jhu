##
##Main function for the project. This body takes care of all the five steps called out in the project
## description. It calls the various functions that have been written for both the test and training data
## and then creates the tidy data in tidy_mergedset.txt
##
run_analysis<-function(){
               
        ##Load the dplyr and reshape2 packages that we will use to merge/cast etc.
        library(dplyr)
        library(reshape2)
        
        print("Program executing ...")
        ##Download and unzip the data file
        zippeddataURL<-("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")
        
        ##Download the Zip File only if it is not already there. Unzip
        zippeddatafile <-"Dataset.zip"
        
        if (!file.exists(zippeddatafile)){
                
                download.file(zippeddataURL, zippeddatafile)
                dateDownloaded<-date()
                
                ##Unzip the downloaded file
                unzip(zippeddatafile)
                
        }
        
        ##Get the feature names (In a function for better readability. See function below)
        featurenames<-getfeaturenames("./UCI HAR Dataset/features.txt")
        
        ##Get the feature names (In a function for better readability. See function below)
        activitynames<-getactivitynames("./UCI HAR Dataset/activity_labels.txt")
         
        ##--Start of Test Data  ----
        ##Read the activity test set (In a function for better readability & reusability. See function below)
        activityset<-readactivityset("./UCI HAR Dataset/test/y_test.txt", activitynames)     
        
        ##Read subject test data for test (In a function for better readability & reusability. See function below)
        subjectset<-readsubjectset("./UCI HAR Dataset/test/subject_test.txt")
        
        ##Read the raw records data for test (In a function for better readability & reusability. See function below)
        measurementset<-readmeasurements("./UCI HAR Dataset/test/X_test.txt", featurenames)
        
        ##Merge subject, activity, measurements data for test.(In a function for better readability & reusability. See function below)
        subactmeasurestest<-mergesubactmeasures(subjectset, activityset, measurementset)
        ##--End of Test Data  ----
        
        ##--Start of Training Data  ----
        ##Read the activity train set (In a function for better readability & reusability. See function below) 
        activityset<-readactivityset("./UCI HAR Dataset/train/y_train.txt", activitynames)     
        
        ##Read subject test data for train (In a function for better readability & reusability. See function below)
        subjectset<-readsubjectset("./UCI HAR Dataset/train/subject_train.txt")
        
        ##Read the raw records data for train (In a function for better readability & reusability. See function below)
        measurementset<-readmeasurements("./UCI HAR Dataset/train/X_train.txt", featurenames)
        
        ##Merge subject, activity, measurements data for test. (In a function for better readability & reusability. See function below)
        subactmeasurestrain<-mergesubactmeasures(subjectset, activityset, measurementset)
        ##--End of Training Data  ----
        
        ##Final Merging of Test and Training Data. 
        ##notestrecords <-as.numeric(nrow(subactmeasurestest))
        ##subactmeasurestest <-mutate(subactmeasurestest, recordID=seq(notestrecords))
        ##subactmeasurestrain <-mutate(subactmeasurestrain, recordID=seq(notestrecords+1, notestrecords+nrow(subactmeasurestrain)))
        
        ## Now merge the test set and the training set.   
        ## This takes care of Step 1; Merges the training and the test sets to create one data set.
        mergedtesttraining<-merge(subactmeasurestest,subactmeasurestrain, all=TRUE, SORT=FALSE)
        ##mergedtesttraining<-arrange(mergedtesttraining, recordID)
        
        ##Following takes care of Step 5:#From the data set in step 4, 
        ##create a second, independent tidy data set with 
        ##the average of each variable for each activity and each subject.
        
        ##First melt the data set with SubjectID and ActivityLabel as IDs.
        meltedmergeddata <- melt(mergedtesttraining, id.vars = c("SubjectID", "ActivityLabel"))
          
        ##Now cast the data set with SubjectID and ActivityLabel as IDs.
        castedmergeddata <- dcast(meltedmergeddata, SubjectID+ActivityLabel~variable,mean)
        
        ##Clean the column names      
        colnames(castedmergeddata) <- cleancolumnnames(names(castedmergeddata))
        
        
        
        ##Save the output in tidy_mergedset.txt file
        write.table(castedmergeddata, file = "tidy_mergedset.txt", sep = " ", col.names=TRUE, row.names=FALSE)
        
        print("Program successfully completed. See tidy_mergedset.txt for the prescribed tidy data set")
                               
}

##-----------------------------------------------------------------------------------------------------
##--- Following are the list of all functions that are used in the run_analysis across test and training

##This function gets the feature names from the features file
getfeaturenames <-function (filename){
        
        ##Check if the features list file exists, 
        if (!file.exists(filename))
                error("Features list file not found!")
        
        ##Read the features and store it in a vector. This will be used to tag the column names in data sets
        featuredataset <-read.table(filename, header=FALSE, sep="", na.strings="-")
        featurenames <- as.vector(featuredataset[,2])
        return(featurenames)
        
}

##This function reads the activity names
getactivitynames<-function(filename){
        
        ##Check if the activity label  file exists. 
        if (!file.exists(filename))
                error("Activity Labels file not found!")
        
        ##Read the activity labels. This will be used to label the activities
        activitydataset <-read.table(filename, header=FALSE, sep="", na.strings="-")
        activitynames <- as.vector(activitydataset[,2])
        return(activitynames)
}

##This function reads activity file either for test or training
readactivityset<-function(filename, activitynames){
        
        ##Check if the activity file exists
        if (!file.exists(filename))
                error("Activity file not found!")
        activityset <-read.table(filename, col.names=c("ActivityLabel"),header=FALSE, sep="", na.strings="-")
        
        ##Replace Activity IDs with Activty Name
        ##This takes care of Step 3: Uses descriptive activity names to name the activities in the data set
        activityset <-mutate(activityset, ActivityLabel=factor(ActivityLabel, label=activitynames), tmpID=seq(nrow(activityset)))
              
        return (activityset)
}

##Function to read the subject file either for test or training
readsubjectset<-function(filename){
        
        ##Check if the subject file exists
        if (!file.exists(filename))
                error("Subject file not found!")
        subjectset <-read.table(filename, col.names=c("SubjectID"),header=FALSE, sep="", na.strings="-")
        subjectset <-mutate(subjectset, tmpID=seq(nrow(subjectset)))
                     
        return (subjectset)
}

##Function to read the measurements data either for test or training
readmeasurements<-function (filename, featurenames){
        
        ##Check if the meausurements data set exisits
        if (!file.exists(filename))
                error("Raw records file not found!")
        
        ##Read the test data set. Read columns as numeric with the column names obtained from feature list        
        ##This takes care of Step 4: Appropriately labels the data set with descriptive variable names. 
        measurementsset <-read.table(filename, col.names=featurenames, colClasses="numeric", header=FALSE, sep="", na.strings="-")
                
        ##Subset by only the mean() or the std() 
        ##This addresses Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
        measurementsset<-subset(measurementsset, select=grep("mean()|std()", names(measurementsset)))
        measurementsset <-mutate(measurementsset, tmpID=seq(nrow(measurementsset)))
                        
        return(measurementsset)

}

##Function to merge subject, activity and measurements
mergesubactmeasures<-function(subject, activity, measures){
     
        ##Merge the activity, subject and raw measurement records for 
        activityandsubject<-merge(subject, activity, by="tmpID", SORT=FALSE)
              
        activityandsubjectandmeasurements<-merge(activityandsubject, measures, by="tmpID", SORT=FALSE)
        
        ##Now remove the temporary tmpID
        activityandsubjectandmeasurements<-subset(activityandsubjectandmeasurements, select=-c(tmpID))
                       
        return(activityandsubjectandmeasurements)
        
}

##Function to clean column names
cleancolumnnames<-function (origcolnames){
        
        
        ##Replace . with empty
        origcolnames<-gsub("\\.", "", origcolnames)
        
        ##First replace f with mean_f and t_with mean_t to indicate that the columns are really means of
        origcolnames<-gsub("^f", "mean_f", origcolnames)
        origcolnames<-gsub("^t", "mean_t", origcolnames)
        
        ##Dump variable names. This is just a conveniece to write the book book
        write.table(origcolnames, file = "variablenames.txt", sep = " ", row.names=FALSE, col.names=FALSE, quote=FALSE)
        return (origcolnames)
        
        
        
}