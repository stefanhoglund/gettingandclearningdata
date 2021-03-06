Getting and Cleaning Data - Project Code Book
==============================================

## Project Purpose & Goal

The purpose of this project is to collect, work with and clean a data set.  The goal is to prepare a tidy data set that can be used for analysis at a later stage.

## Data

The data used for the project is from an experiment called "Human Activity Recongnition Using Smartphones"

Data Source: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

Full description and additional information: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

## Variables in tidy data set

 [1] subject - Identifies which subject (1:30) performed the activity                  
 [2] activity - Identifies activity (laying, sitting, standing, walking, walking_downstairs, walking_upstairs)
 
  The following contains the average of each variable:
  
 [3] tbodyacc-mean-x          
 [4] tbodyacc-mean-y          
 [5] tbodyacc-mean-z          
 [6] tbodyacc-std-x           
 [7] tbodyacc-std-y           
 [8] tbodyacc-std-z           
 [9] tgravityacc-mean-x       
[10] tgravityacc-mean-y       
[11] tgravityacc-mean-z       
[12] tgravityacc-std-x        
[13] tgravityacc-std-y        
[14] tgravityacc-std-z        
[15] tbodyaccjerk-mean-x      
[16] tbodyaccjerk-mean-y      
[17] tbodyaccjerk-mean-z      
[18] tbodyaccjerk-std-x       
[19] tbodyaccjerk-std-y       
[20] tbodyaccjerk-std-z       
[21] tbodygyro-mean-x         
[22] tbodygyro-mean-y         
[23] tbodygyro-mean-z         
[24] tbodygyro-std-x          
[25] tbodygyro-std-y          
[26] tbodygyro-std-z          
[27] tbodygyrojerk-mean-x     
[28] tbodygyrojerk-mean-y     
[29] tbodygyrojerk-mean-z     
[30] tbodygyrojerk-std-x      
[31] tbodygyrojerk-std-y      
[32] tbodygyrojerk-std-z      
[33] tbodyaccmag-mean         
[34] tbodyaccmag-std          
[35] tgravityaccmag-mean      
[36] tgravityaccmag-std       
[37] tbodyaccjerkmag-mean     
[38] tbodyaccjerkmag-std      
[39] tbodygyromag-mean        
[40] tbodygyromag-std         
[41] tbodygyrojerkmag-mean    
[42] tbodygyrojerkmag-std     
[43] fbodyacc-mean-x          
[44] fbodyacc-mean-y          
[45] fbodyacc-mean-z          
[46] fbodyacc-std-x           
[47] fbodyacc-std-y           
[48] fbodyacc-std-z           
[49] fbodyaccjerk-mean-x      
[50] fbodyaccjerk-mean-y      
[51] fbodyaccjerk-mean-z      
[52] fbodyaccjerk-std-x       
[53] fbodyaccjerk-std-y       
[54] fbodyaccjerk-std-z       
[55] fbodygyro-mean-x         
[56] fbodygyro-mean-y         
[57] fbodygyro-mean-z         
[58] fbodygyro-std-x          
[59] fbodygyro-std-y          
[60] fbodygyro-std-z          
[61] fbodyaccmag-mean         
[62] fbodyaccmag-std          
[63] fbodybodyaccjerkmag-mean
[64] fbodybodyaccjerkmag-std  
[65] fbodybodygyromag-mean    
[66] fbodybodygyromag-std     
[67] fbodybodygyrojerkmag-mean
[68] fbodybodygyrojerkmag-std 

## Transformations

* Converted all column headings to lower-case
* Filtered out to only include mean and standard deviation from the features
* Removed () from mean() and std() column headings
* Added activity labels
* Added subject id to identify subject performing the activity
* Converted activity labels (e.g. WALKING, STANDING) to lower-case
* Combined training and test data set
* Calculated the average feature value grouped by subject and activity
* Saved two tidy datasets to disk; one corresponding to step 4 of the project instructions and another one corresponding to step 5 of the project instructions.

## Assumptions

1) The working directory contains the following files from the Samsung data:

* activity_labels.txt
* X_train.txt
* X_test.txt
* y_train.txt
* y_test.txt
* subject_test.txt
* subject_train.txt
* features.txt

2) For this analysis, the dataset returned (and submitted as part of the class
   project) is the dataset outlined in project instruction step 5.  The larger
   dataset is also includede (tidydataset_step4.txt) in the repository for reference. 
