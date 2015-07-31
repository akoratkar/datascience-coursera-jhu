# Getting and Cleaning Data: Course Project: CodeBook.md to describe the variables in tidy_mergedset.txt

##Description
This is the code Book to describe the variables in tidy_mergedset.txt. Essentially, these are means of the mean() and std() columns from the original files

* SubjectID: Subject ID {1 to 30}
* ActivityLabel: Labels of the activities {WALKING
 WALKING UPSTAIRS
 WALKING_DOWNSTAIRS
 SITTING
 STANDING
6 LAYING
}
* mean_tBodyAccmeanX: Mean of the raw body acceleration along X-axis mean
* mean_tBodyAccmeanY: Mean of the raw body acceleration along Y-axis mean
* mean_tBodyAccmeanZ: Mean of the raw body acceleration along Z-axis mean
* mean_tBodyAccstdX: Mean of the FFT body acceleration along X-axis std
* mean_tBodyAccstdY:  Mean of the FFT body acceleration along Y-axis std
* mean_tBodyAccstdZ: Mean of the FFT body acceleration along Z-axis std
* mean_tGravityAccmeanX: Mean of the FFT gravity acceleration along X-axis mean
* mean_tGravityAccmeanY: Mean of the FFT gravity acceleration along Y-axis mean
* mean_tGravityAccmeanZ: Mean of the FFT gravity acceleration along Z-axis mean
* mean_tGravityAccstdX: Mean of the FFT gravity acceleration along X-axis std
* mean_tGravityAccstdY: Mean of the FFT gravity acceleration along Y-axis std
* mean_tGravityAccstdZ: Mean of the FFT gravity acceleration along Z-axis std
* mean_tBodyAccJerkmeanX: Mean of the FFT body acceleration jerk along X-axis mean
* mean_tBodyAccJerkmeanY: Mean of the FFT body acceleration jerk along Y-axis mean
* mean_tBodyAccJerkmeanZ: Mean of the FFT body acceleration jerk along Z-axis mean
* mean_tBodyAccJerkstdX: Mean of the FFT body acceleration jerk along X-axis std
* mean_tBodyAccJerkstdY: Mean of the FFT body acceleration jerk along Y-axis std
* mean_tBodyAccJerkstdZ: Mean of the FFT body acceleration jerk along Z-axis std
* mean_tBodyGyromeanX: Mean of the FFT body gyro along X-axis mean
* mean_tBodyGyromeanY: Mean of the FFT body gyro along Y-axis mean
* mean_tBodyGyromeanZ: Mean of the FFT body gyro along Z-axis mean
* mean_tBodyGyrostdX: Mean of the FFT body gyro along X-axis std
* mean_tBodyGyrostdY: Mean of the FFT body gyro along Y-axis std
* mean_tBodyGyrostdZ: Mean of the FFT body gyro along Z-axis std
* mean_tBodyGyroJerkmeanX: Mean of the FFT gyro jerk along X-axis mean
* mean_tBodyGyroJerkmeanY: Mean of the FFT gyro jerk along Y-axis mean
* mean_tBodyGyroJerkmeanZ: Mean of the FFT gyro jerk along Z-axis mean
* mean_tBodyGyroJerkstdX: Mean of the FFT gyro jerk along X-axis std
* mean_tBodyGyroJerkstdY: Mean of the FFT gyro jerk along Y-axis std
* mean_tBodyGyroJerkstdZ: Mean of the FFT gyro jerk along Z-axis std
* mean_tBodyAccMagmean: Mean of the FFT body acceneration mag with Euclidean norm mean
* mean_tBodyAccMagstd: Mean of the FFT body acceneration mag with Euclidean norm std
* mean_tGravityAccMagmean: Mean of the FFT gravity mag with Euclidean norm mean
* mean_tGravityAccMagstd: Mean of the FFT gravity mag with Euclidean norm std
* mean_tBodyAccJerkMagmean: Mean of the FFT body jerk mag with Euclidean norm mean
* mean_tBodyAccJerkMagstd: Mean of the FFT body jerk mag with Euclidean norm std
* mean_tBodyGyroMagmean: Mean of the FFT body gyro mag with Euclidean norm mean
* mean_tBodyGyroMagstd: Mean of the FFT body gyro y mag with Euclidean norm std
* mean_tBodyGyroJerkMagmean: Mean of the FFT body jerk mag with Euclidean norm mean
* mean_tBodyGyroJerkMagstd: Mean of the FFT body jerk mag with Euclidean norm std
* mean_fBodyAccmeanX: Mean of the raw body acceleration mean along X-axis
* mean_fBodyAccmeanY: Mean of the raw body acceleration mean along Y-axis
* mean_fBodyAccmeanZ: Mean of the raw body acceleration mean along Z-axis
* mean_fBodyAccstdX: Mean of the raw body acceleration std along X-axis
* mean_fBodyAccstdY: Mean of the raw body acceleration std along Y-axis
* mean_fBodyAccstdZ: Mean of the raw body acceleration std along Z-axis
* mean_fBodyAccmeanFreqX: Mean of the raw body acceleration mean frequency along X-axis
* mean_fBodyAccmeanFreqY: Mean of the raw body acceleration mean frequency along Y-axis
* mean_fBodyAccmeanFreqZ: Mean of the raw body acceleration mean frequency along Z-axis
* mean_fBodyAccJerkmeanX: Mean of the raw body acceleration jerk along X-axis mean
* mean_fBodyAccJerkmeanY: Mean of the raw body acceleration jerk along Y-axis mean
* mean_fBodyAccJerkmeanZ: Mean of the raw body acceleration jerk along Z-axis mean
* mean_fBodyAccJerkstdX: Mean of the raw body acceleration jerk along X-axis std
* mean_fBodyAccJerkstdY: Mean of the raw body acceleration jerk along Y-axis mean
* mean_fBodyAccJerkstdZ: Mean of the raw body acceleration jerk along Z-axis mean
* mean_fBodyAccJerkmeanFreqX: Mean of the raw body acceleration mean frequency along X-axis
* mean_fBodyAccJerkmeanFreqY: Mean of the raw body acceleration mean frequency along Y-axis
* mean_fBodyAccJerkmeanFreqZ: Mean of the raw body acceleration mean frequency along Z-axis
* mean_fBodyGyromeanX: Mean of the raw body gyro along X-axis mean
* mean_fBodyGyromeanY: Mean of the raw body gyro along Y-axis mean
* mean_fBodyGyromeanZ: Mean of the raw body gyro along Z-axis mean
* mean_fBodyGyrostdX: Mean of the raw body gyro along X-axis std
* mean_fBodyGyrostdY: Mean of the raw body gyro along Y-axis std
* mean_fBodyGyrostdZ: Mean of the raw body gyro along Z-axis std
* mean_fBodyGyromeanFreqX: Mean of the raw body gyro mean frequency along X-axis
* mean_fBodyGyromeanFreqY: Mean of the raw body gyro mean frequency along Y-axis
* mean_fBodyGyromeanFreqZ: Mean of the raw body gyro mean frequency along Z-axis
* mean_fBodyAccMagmean: Mean of the raw body acceleration mag with Euclidean norm mean
* mean_fBodyAccMagstd: Mean of the raw body acceleration mag with Euclidean norm std
* mean_fBodyAccMagmeanFreq: Mean of the raw body acceleration mean frequency 
* mean_fBodyBodyAccJerkMagmean: Mean of the raw body acceleration jerk mag with Euclidean norm mean
* mean_fBodyBodyAccJerkMagstd: Mean of the raw body acceleration jerk mag with Euclidean norm std
* mean_fBodyBodyAccJerkMagmeanFreq: Mean of the raw body acceleration jerk mag mean requency with Euclidean norm 
* mean_fBodyBodyGyroMagmean: Mean of the raw body gyro mag with Euclidean norm mean
* mean_fBodyBodyGyroMagstd: Mean of the raw body gyro mag with Euclidean norm std
* mean_fBodyBodyGyroMagmeanFreq: Mean of the raw body gyro mag mean frequncy with Euclidean norm 
* mean_fBodyBodyGyroJerkMagmean: Mean of the raw body gyro jerk mag with Euclidean norm mean
* mean_fBodyBodyGyroJerkMagstd: Mean of the raw body gyro jerk mag with Euclidean norm std
* mean_fBodyBodyGyroJerkMagmeanFreq: Mean of the raw body gyro jerk mag mean frequncy with Euclidean norm 
