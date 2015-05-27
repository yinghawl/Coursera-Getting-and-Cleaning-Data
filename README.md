**Project 1 Coursera course: Getting and Cleaning Data**
The data used in the project was downloaded from UCI Machine Learning Repository. The folder name is "UCL HAR Dataset". The data that are used includes the following;
[Train dataset] ./train/X_train.txt, ./train/Y_train.txt, ./train/subject_train.txt,
[Test dataset]./test/X_test.txt, ./test/Y_test.txt, ./test/subject_test.txt,
[Label]./activity_labels.txt, ./features.txt 

Save the downloaded file on your working directory. Then you should be able to run it right away. 
**The Breakdown of the Project**
The whole project was executed in several steps;
Step 1: Read both train and test dataset and combine them
Step 2: Extract the measurements of the mean and standard deviation for each measurement 
Step 3: Rename the notation numbers in Y_train & Y_test data into readable words like WALKING,STANDING and etc by using activity_labels
Step 4: Give a descriptive names to each variable. (For the sake of simplicity, I did it on step 2)
Step 5: Compute one measurement for the average measurements for each activity and for each subject. 

Step 1: Read both train and test dataset and combine them
This step is pretty self-explanatory. It involves reading the table into the R by using read.table and combine imported tables (eg. from X_train.txt, subject_train.txt and Y_train.txt)
by using cbind function.
I specified the column class when reading to make reading process quicker.

Step 2: Extract the measurements of the mean and standard deviation for each measurement 
This step can be broken down into several substeps. 

First,extract all the variable columns(All except for subject and activity column); 

Due to the fact that each column of the extracted table(name is mdata ; dimension is 10299 561) represents a specific type of measurement. There are 561 of them. The description for the column is stored in the features.txt. 
I transposed the extracted table (now dimension is 561 10299) and cbind it with the table(name:feature, dimension is 561,1) I read from features.txt file into R. 

The following step is the most critical step. Basically, what I intend to do is to filter the mdata with specific words like "mean" or "std" (which stands for standard deviation) in order to achieve my goal to extract mean and standard deviation data
I filtered the mdata by "mean" with grepl functions (Note that I also explicitly excluded "meanFreq" which is not what we are looking for) and assigned it to dmean.

Then, I did the same things for standard deviation data (filtered by "std" with grepl function) and assigned it to std.

Then, I rbind dmean and std into one and assign it to cdata (dimension is 10300 33);I transposed cdata. Now the dimension is 10300 and 33;

For the sake of simplicity, I went ahead and renamed my columns. Note that the new table(cdata) has the dimension of 10300 and 33 The last row of "mdata" table is the name of each type of measurement; I therefore, renamed the column of cdata by using the last row of cdata; 
Also, I assigned the first two columns of rawdata into "subcond" and assigned the column name for "subcond" table.

As the last step for step 2, I cbind "subcond" with the first 10299 rows of cdata (Since I renamed using by the data from row 10300, I can exclude. This action also makes it possible to cbind these two tables.)

Step 3:Rename the notation numbers in Y_train & Y_test data into readable words like WALKING,STANDING and etc by using activity_labels
I read the activity labels.txt to R and assigned it to activitylabels;
Next, I merged activitylabels table with cdata by the key-"Num.Cond"

Then, I went ahead and excluded the column of Num.Cond and sorted table with Subject.

Step 4: Give a descriptive names to each variable. (For the sake of simplicity, I did it on step 2)
Since it has been done as part of the step 2, I will not write anymore here.

Step 5: Compute one measurement for the average measurements for each activity and for each subject. 
I managed to get the average measurement for each activity and for each subject with one command line by using dplyr (group by and summarise_each)
However, looking at the table I computed (wide format),I feel like it is a little bit messy though it is very concise as well. 
What I did next is to use melt function to melt down the table. It still gave the exact same data but in a much longer form.

The reason I prefer this over the wide form is that I believe the goal of tidy data, as Hadley Wickham stated, prepare data for the next step of processing.Making it easier to understand and process,therefore is the priority.
I think this also partially came down to personal preference. After all, it is just one more command line. 
 


