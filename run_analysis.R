library(reshape2)

# The file function checks fopt the avalibility of file, downloads and unzip if missing
file <- function() {
  
  file_name <- "getdata_projectfiles_UCI HAR Dataset.zip"
  if (!file.exists(file_name)){fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
  download.file(fileURL, file_name, method="curl")}  
  if (!file.exists("UCI HAR Dataset")) { unzip(file_name) }
}
# the process function takes steps, Extraction-> mean and sd ->merging ->Resultant
Process  <- function() {
  # extraction of datasets
  
  features <- read.table("UCI HAR Dataset/features.txt")
  activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
  y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
  
  # Loading features and activity labels
  
  features[,2] <- as.character(features[,2])
  activity_labels[,2] <- as.character(activity_labels[,2])
  
 
  # Extracting the data on mean and standard deviation
  
  fea_req <- grep(".*mean.*|.*std.*", features[,2])
  fea_req.names <- features[fea_req,2]
  fea_req.names = gsub('-std', 'Std', fea_req.names)
  fea_req.names = gsub('-mean', 'Mean', fea_req.names)
  fea_req.names <- gsub('[-()]', '', fea_req.names)
  
      x_train <- read.table("UCI HAR Dataset/train/X_train.txt")[fea_req]
      x_test <- read.table("UCI HAR Dataset/test/X_test.txt")[fea_req]
 
  # merging 
  
  x_train <- cbind(subject_train, y_train, x_train)
  x_test <- cbind(subject_test, y_test, x_test)
  data <- rbind(x_train, x_test)
  colnames(data) <- c("subject", "activity", fea_req.names)

  # turn activities & subjects into factors
  data$activity <- factor(data$activity, levels = activity_labels[,1], labels = activity_labels[,2])
  data$subject <- as.factor(data$subject)
  
  data.melted <- melt(data, id = c("subject", "activity"))
  data.mean <- dcast(data.melted, subject + activity ~ variable, mean)
  
  write.table(data.mean, "Result.txt", row.names = FALSE, quote = FALSE) 
}


file()
Process()




