runAnalysis <- function() {
    
    library(dplyr)
    
    setwd("Downloads/UCI HAR Dataset/")
    
    x_test <- read.table("test/X_test.txt")
    y_test <- read.table("test/y_test.txt")
    
    x_train <- read.table("train/X_train.txt")
    y_train <- read.table("train/y_train.txt")
    
    x_merged <- rbind(x_test, x_train)
    
    features <- read.table("features.txt")[, 2]
    names(x_merged) <- features
    
    pattern <- grep("(mean|std)\\(\\)", names(x_merged))
    filtered <- x_merged[, pattern]
    
    y_merged <- rbind(y_test, y_train)[, 1]
    
    actNames <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
    activities <- actNames[y_merged]
    
    names(filtered) <- gsub("-mean\\(\\)", "-Mean", names(filtered))
    names(filtered) <- gsub("-std\\(\\)", "-Std", names(filtered))
    names(filtered) <- gsub("^t", "Time-", names(filtered))
    names(filtered) <- gsub("^f", "Frequency-", names(filtered))
    
    subject_test <- read.table("test/subject_test.txt")
    subject_train <- read.table("train/subject_train.txt")
    subjects <- rbind(subject_test, subject_train)[, 1]
    
    df <- cbind(Activity = activities, Subject = subtidyjects, filtered)
    
    tidy <- aggregate(df, by = list(Activity = df$Activity, Subject = df$Subject), FUN = "mean")
    tidy[,3] = NULL
    tidy[,4] = NULL
    
    write.table(tidy, "tidy.txt")
}