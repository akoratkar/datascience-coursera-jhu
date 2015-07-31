# Getting and Cleaning Data: Course Project: README.md

##Description
This repository contains the R scripts (run_analysi.R), the output data file (tidy_mergedset.txt), the CodeBook.md and README.md for the Getting and Cleaning Data 030 Course Project. The steps realized by the code segments have been clearly identified in the code

##To execute
* > source("run_analysis.R")
* > run_analysis()
* [1] "Program executing ..."
* [1] "Program successfully completed. See tidy_mergedset.txt for the prescribed tidy data set"

##Pre-requisites libraries
* dplyr package
* reshape2 

##Organization of the code.
* run_analysis This is the main function. It downloads the data file and unzips it. 
* getfeaturenames This function gets the feature names from the features file
* getactivitynamesThis function reads the activity names
* readactivityset This function reads activity file either for test or training
* readsubjectset This function reads the subject file either for test or training
* readmeasurements This function reads the measurements data either for test or training
* mergesubactmeasures This Function to merge subject, activity and measurements
* cleancolumnnames This function cleans column names




 



