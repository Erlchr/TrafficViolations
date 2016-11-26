# Libraries
library(sp)
library(raster)
library(dplyr)

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

#---------- START DATA PREPARATION ----------#

# Remove redundant location information
data = select(data.raw, -Location, -Geolocation) 

# Remove columns which has always the same value like "Agency = MCP" in each row of data
data = select(data, -Agency, -Accident, -Commercial.Vehicle)

# Remove irrelevant/unnecessary columns for analysis
data = select(data, -Commercial.License, -Work.Zone, -State, -Model, -Color, -DL.State)
data = select(data, -Fatal, -HAZMAT, -Charge, -Article)

# Add a column "Local" to see if the driver is a local or a tourist
data$Local <- ifelse(data$Driver.State == "MD", TRUE, FALSE)
data = select(data, -Driver.State) # Remove Driver.State

# Add a column to see at which weekday the violation occured
data = mutate(data, weekDay = paste( wday(as.Date(data$Date.Of.Stop, format = "%m/%d/%Y"), label = TRUE)  ))

# Add column time intervall: 
# 1. Create column with converted time in seconds
convertedInSecTimeIntervall = minutes(times(data$Time.Of.Stop)) + hours(times(data$Time.Of.Stop)) * 60 * 60
# 2. Assign the seconds in intervall
cuts <- c(-Inf, 10800, 10800*2, 10800*3, 10800*4, 10800*5, 10800*6, 10800*7, Inf)
labs <- c("'00:00 - 02:59'", "03:00 - 05:59", "06:00 - 08:59", "09:00 - 11:59", "12:00 - 14:59", "15:00 - 17:59", "18:00 - 20:59", "21:00 - 23:59")
assignedInterval = labs[findInterval(convertedInSecTimeIntervall, cuts)]
# 3. Add column with useful values into data
data = mutate(data, timeIntervall = paste (assignedInterval))

#---------- END DATA PREPARATION ----------#