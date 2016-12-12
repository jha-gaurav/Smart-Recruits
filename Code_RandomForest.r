#Analytics Vidhya - Smart Recruits competition script
#A classification problem with one value to be predicted.
#Create Functions
#Normalize the fields. For this create a normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#Read the csv file as a dataframe
#Create Data Cleansing function
DataCleanse <- function(SR_Comp_DF) {
  
  #SR_Comp_DF$Manager_Business <- ifelse(is.na(SR_Comp_DF$Manager_Business), median(SR_Comp_DF$Manager_Business[which(!is.na(SR_Comp_DF$Manager_Business))]), SR_Comp_DF$Manager_Business)
  #SR_Comp_DF$Manager_Business2 <- ifelse(is.na(SR_Comp_DF$Manager_Business2), mean(SR_Comp_DF$Manager_Business2[which(!is.na(SR_Comp_DF$Manager_Business2))]), SR_Comp_DF$Manager_Business2)
  #SR_Comp_DF$Manager_Num_Products <- ifelse(is.na(SR_Comp_DF$Manager_Num_Products), median(SR_Comp_DF$Manager_Num_Products[which(!is.na(SR_Comp_DF$Manager_Num_Products))]), SR_Comp_DF$Manager_Num_Products)
  #SR_Comp_DF$Manager_Num_Products2 <- ifelse(is.na(SR_Comp_DF$Manager_Num_Products2), median(SR_Comp_DF$Manager_Num_Products2[which(!is.na(SR_Comp_DF$Manager_Num_Products2))]), SR_Comp_DF$Manager_Num_Products2)
  
  
  #nrow(SR_Comp_DF)
  #str(SR_Comp_DF)
  
  #Convert the required data to factors
  
  SR_Comp_DF$Applicant_Marital_Status <- as.numeric(as.factor(SR_Comp_DF$Applicant_Marital_Status))
  SR_Comp_DF$Applicant_Occupation <- as.numeric(as.factor(SR_Comp_DF$Applicant_Occupation))
  SR_Comp_DF$Applicant_Qualification <- as.numeric(as.factor(SR_Comp_DF$Applicant_Qualification))
  #str(SR_Comp_DF)
  
  SR_Comp_DF$Manager_Joining_Designation <- as.numeric(as.factor(SR_Comp_DF$Manager_Joining_Designation))
  SR_Comp_DF$Manager_Current_Designation <- as.numeric(as.factor(SR_Comp_DF$Manager_Current_Designation))
  SR_Comp_DF$Manager_Status <- as.numeric(as.factor(SR_Comp_DF$Manager_Status))
  SR_Comp_DF$Manager_Grade <- as.numeric(as.factor(SR_Comp_DF$Manager_Grade))
  
  #str(SR_Comp_DF)
  #Steps to cleanse the training data
  #1. Remove the applicant id
  SR_Clean_Data <- SR_Comp_DF[-1]
  #str(SR_Clean_Data)
  
  #2. Compare if Applicant city is same as office.
  #This can be done by comparing the first 3 characters of the applicant city pin and office pin
  #After this comarison, remove the fields from the dataframe
  SR_Clean_Data$SameCity <- ifelse ((substr(SR_Clean_Data$Office_PIN, 1, 3) == substr(SR_Clean_Data$Applicant_City_PIN, 1, 3)), 1 , 0)
  SR_Clean_Data$SameCity <- as.numeric(as.factor(SR_Clean_Data$SameCity))
  SR_Clean_Data$Office_PIN <- NULL
  SR_Clean_Data$Applicant_City_PIN <- NULL
  #str(SR_Clean_Data)
  
  #3. Check if Applicant and manager are of same gender.
  SR_Clean_Data$Same_Gender <- ifelse(SR_Clean_Data$Applicant_Gender == SR_Clean_Data$Manager_Gender, 1, 0)
  SR_Clean_Data$Same_Gender <- as.numeric(as.factor(SR_Clean_Data$Same_Gender))
  #SR_Clean_Data$Applicant_Gender <- as.numeric(as.factor(SR_Clean_Data$Applicant_Gender))
  #SR_Clean_Data$Manager_Gender <- as.numeric(as.factor(SR_Clean_Data$Manager_Gender))
  SR_Clean_Data$Applicant_Gender <- NULL
  SR_Clean_Data$Manager_Gender <- NULL
  #str(SR_Clean_Data)
  
  #Remove more columns which do not effect the final decision
  SR_Clean_Data$Manager_DOJ <- NULL
  SR_Clean_Data$Application_Receipt_Date <- NULL
  SR_Clean_Data$Applicant_BirthDate <- NULL
  SR_Clean_Data$Manager_DoB <- NULL
  #str(SR_Clean_Data)
  
  SR_Clean_Data$Manager_Business <- normalize(SR_Clean_Data$Manager_Business)
  SR_Clean_Data$Manager_Business2 <- normalize(SR_Clean_Data$Manager_Business2)
  SR_Clean_Data$Manager_Num_Products <- normalize(SR_Clean_Data$Manager_Num_Products)
  SR_Clean_Data$Manager_Num_Products2 <- normalize(SR_Clean_Data$Manager_Num_Products2)
  #str(SR_Clean_Data)
  return(SR_Clean_Data)
}

SR_Comp_DF_Training <- read.csv("Train_pjb2QcD.csv", header = TRUE, stringsAsFactors = FALSE)
SR_Comp_DF_Training <- SR_Comp_DF_Training[!(is.na(SR_Comp_DF_Training$Applicant_City_PIN)),]
SR_Comp_DF_Training <- SR_Comp_DF_Training[!(SR_Comp_DF_Training$Manager_DOJ == ""),]
SR_Train_Clean <- DataCleanse(SR_Comp_DF_Training)
str(SR_Train_Clean)
#Now the data has been cleansed. Implement RF now.
SR_Train_Clean$Business_Sourced <- as.factor(SR_Train_Clean$Business_Sourced)
set.seed(100)
SR_Sample_Ind <- sample(2, nrow(SR_Train_Clean), replace = TRUE, prob = c(0.75, 0.25))

Trn_Trn <- SR_Train_Clean[SR_Sample_Ind == 1,]
Trn_Test <- SR_Train_Clean[SR_Sample_Ind == 2,]

nrow(Trn_Trn)
nrow(Trn_Test)

library(e1071)
library(randomForest)
SR_Comp_NV <- randomForest(Business_Sourced ~., data = Trn_Trn, ntree = 700, mtry = 6)

#now test the modal with the test dataset
SR_Test_NV <- predict(SR_Comp_NV, Trn_Test)

#Create the confusiin matrix

nv_rf <- table (as.character(Trn_Test$Business_Sourced), as.character(SR_Test_NV))

#Find details from the confusion matrix

nv_tn <- nv_rf[1,1]
nv_tp <- nv_rf[2,2]
nv_fp <- nv_rf[2,1]
nv_fn <- nv_rf[1,2]
nv_rf

#compute accuracy, precision and recall
nv_accuracy <- (nv_tp + nv_tn) / nrow(Trn_Test)
nv_precision <- nv_tp / (nv_tp + nv_fp)
nv_recall <- nv_tp / (nv_tp + nv_fn)

nv_accuracy
nv_precision
nv_recall



#Going with this model. Efficiency = 82.6%
#Replicate all the data cleansing process for the test dataset as well.
SR_Comp_DF_Test <- read.csv("Test_wyCirpO.csv", header = TRUE, stringsAsFactors = FALSE)
SR_Comp_DF_Test[is.na(SR_Comp_DF_Test)] <- 0
SR_Test_Clean <- DataCleanse(SR_Comp_DF_Test)
SR_Test_Clean$Business_Sourced <- 0
str(SR_Test_Clean)

SR_Model_Test <- predict(SR_Comp_NV, SR_Test_Clean)
Output_Colnames <- c("ID", "Business_Sourced")
SR_Out <- cbind.data.frame(SR_Comp_DF_Test$ID, SR_Model_Test, stringsAsFactors = FALSE)
#SR_Write <- rbind.data.frame(Output_Colnames, SR_Out, stringsAsFactors = FALSE)
write.table(SR_Out, "Sample_Submission_NB.csv", quote = FALSE, col.names = Output_Colnames, sep = "," , row.names = FALSE)
