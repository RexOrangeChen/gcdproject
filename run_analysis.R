
library(dplyr)
run_analysis <- function(){
    #read datas, titles, labels, subjects
    activity_labels <- read.table(".\\UCI HAR Dataset\\activity_labels.txt", stringsAsFactors = FALSE)
    features <- read.table(".\\UCI HAR Dataset\\features.txt", stringsAsFactors = FALSE)
    data_train <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt", stringsAsFactors = FALSE)
    data_train_subject <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt", stringsAsFactors = FALSE)
    data_train_label <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt", stringsAsFactors = FALSE)
    data_test <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt", stringsAsFactors = FALSE)
    data_test_subject <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt", stringsAsFactors = FALSE)
    data_test_label <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt", stringsAsFactors = FALSE)
    
    #merge train and test
    train <- cbind(data_train_subject,data_train_label, data_train)
    test <- cbind(data_test_subject,data_test_label, data_test)
    all <- rbind(train, test)
    
    #rename the column
    names(all) <- c("subject", "labels", features$V2)
    #Extracts only the measurements on the mean and standard deviation for each measurement
    all_sub <- all[c(1,2,grep("mean|std", names(all)))]
    redefine <- function(){
        liu <- character()
        for(x in all_sub$labels){
            for(y in 1:6){
                if(activity_labels[y,1]==x){
                    liu <- c(liu,activity_labels[y,2])
                }
            }
        }
        return(liu)
    }
    all_sub$labels <-redefine() 
    #creates a second, independent tidy data set with the average of each variable for each activity and each subject
    all_sub_group <- group_by(all_sub, subject,labels)
    mydata <- summarize(all_sub_group, mean(all_sub_group[[3]]))
    for(i in 4:81){
        mydata1 <- summarize(all_sub_group, mean(all_sub_group[[i]]))
        mydata <- merge(mydata, mydata1, by =c("subject", "labels"),all = TRUE)
    }
    write.table(mydata,"output2.txt",row.name=FALSE)
    write.table(all,"output1.txt",row.name=FALSE)
}