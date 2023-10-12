setwd("D:/MultiOmics/Code")
 
library(Matrix)
library(ggplot2)
library(dplyr) 


PatientInfo <- readxl::read_excel("D:/MultiOmics/Data/Supplementary Tables.xlsx")
# Specify the path to the folder containing your TSV files
folder_path <- "D:/MultiOmics/Data"

# List all TSV files in the folder
tsv_files <- list.files(folder_path, pattern = "\\.tsv$", full.names = TRUE)

# Loop through the TSV files and create separate data frames
for (file in tsv_files) {
  # Read the TSV file into a data frame
  tsv_data <- read.delim(file, header = TRUE, sep = "\t")
  
  # Remove the fixed part of the name (e.g., "transneo-diagnosis-")
  df_name <- gsub("^transneo-diagnosis-", "", basename(file))
  
  df_name <- gsub("\\.tsv$", "", df_name)  # Remove ".tsv" from the file name
  df_name <- gsub("-", "_", df_name)  # Remove ".tsv" from the file name
  
  # Create a separate data frame with the modified name
  assign(df_name, tsv_data)
}

colnames(`HRD`)

colnames(`immune_IPS_components`)[colnames(`immune_IPS_components`)=="X"]<-"Trial.ID"
colnames(`immune_MCPcounter`)[colnames(`immune_MCPcounter`)=="X"]<-"Trial.ID"
colnames(`mutational_signatures`)[colnames(`mutational_signatures`)=="X"]<-"Trial.ID"


colnames( immune_IPS_components )
library(dplyr)

merged_df <- merge(ASCAT_CIN, ASCAT_purity, by = "Trial.ID", all = TRUE)
merged_df <- merge(merged_df, DigPathology , by = "Trial.ID", all = TRUE)
merged_df <- merge(merged_df, HRD , by = "Trial.ID", all = TRUE)
merged_df <- merge(merged_df, immune_IPS_components,  by = "Trial.ID", all = TRUE)
merged_df <- merge(merged_df, immune_MCPcounter,   by = "Trial.ID", all = TRUE)
merged_df <- merge(merged_df, mutational_signatures, by = "Trial.ID", all = TRUE)

merged_df2 <- na.omit(merged_df) 
merged_df2$iC10 <- PatientInfo$iC10 [match(merged_df2$Trial.ID,PatientInfo$Donor.ID )] 
merged_df2$Stage <- PatientInfo$T.stage [match(merged_df2$Trial.ID,PatientInfo$Donor.ID )] 
merged_df2$Stage <- sub("T", "", merged_df2$Stage)
merged_df2$LNStatus <- PatientInfo$LN.status.at.diagnosis [match(merged_df2$Trial.ID,PatientInfo$Donor.ID )] 
merged_df2$ER  <- PatientInfo$ER.status [match(merged_df2$Trial.ID,PatientInfo$Donor.ID )] 
merged_df2$HER2  <- PatientInfo$ER.status [match(merged_df2$Trial.ID,PatientInfo$Donor.ID )] 
merged_df2$PAM50 <- PatientInfo$PAM50 [match(merged_df2$Trial.ID,PatientInfo$Donor.ID )] 
merged_df2$Response <- PatientInfo$pCR.RD [match(merged_df2$Trial.ID,PatientInfo$Donor.ID )] 

merged_df3 <- na.omit(merged_df2) 

merged_df3[merged_df3=="NA"] <- NA
merged_df3 <- na.omit(merged_df3) 

 write.table(merged_df3, file = "D:/MultiOmics/Data/AllData2.txt",sep="\t", row.names = FALSE, col.names = TRUE )
