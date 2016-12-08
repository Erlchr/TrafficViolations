library(kernlab)
library(caret)
library(dplyr)

#---------- START READING DATA ----------#

# File Names
dataCSVFileName = "traffic_violations.csv"
dataRDSFileName = "traffic_violations.rds"

# Save CSV as Binary
if(!file.exists(dataRDSFileName)) {
  dataCSV = read.csv(dataCSVFileName)
  saveRDS(dataCSV, dataRDSFileName)
}

# Read Data
data.raw = readRDS(dataRDSFileName)

#---------- END READING DATA ----------#
#---------- START DATA PREPARATION ----------#

# Remove redundant location information
data = select(data.raw, -Location, -Geolocation) 

# Remove columns which has always the same value like "Agency = MCP" in each row of data
data = select(data, -Agency, -Accident, -Commercial.Vehicle)

# Remove irrelevant/unnecessary columns for analysis
data = select(data, -Commercial.License, -Work.Zone, -State, -Model, -Color, -DL.State)
data = select(data, -Fatal, -HAZMAT, -Charge, -Article)

# Remove NA coordinates rows 
data = filter(data, !is.na(data$Latitude), !is.na(data$Longitude))

# Add a column "Local" to see if the driver is a local or a tourist
data$Local <- ifelse(data$Driver.State == "MD", TRUE, FALSE)
data = select(data, -Driver.State) # Remove Driver.State

# Update column "Violation.Type " to see if the driver is a local or a tourist
data$Local <- ifelse(data$Violation.Type == "Citation", TRUE, FALSE)

#---------- END DATA PREPARATION ----------#
#---------- START CREATE TEST AND TRAINING ----------#

data_train <- data[1:110000,]
data_test  <- data[110001:138000,]

#---------- END CREATE TEST AND TRAINING ----------#

letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")

