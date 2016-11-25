library(dplyr)

# File Names
dataCSVFileName = "traffic_violations.csv"
dataRDSFileName = "traffic_violations.rds"

# Save CSV as Binary
dataCSV = read.csv(file=dataCSVFileName)  #, nrows=50000)
saveRDS(dataCSV, dataRDSFileName)

# Read Data
dataRDS = readRDS(dataRDSFileName)

#------------------ DATA CLEANING BEGIN-----------
#Since we work with Latitude and Longitude we can remove redundant location data
dataRDS.cleaned = select(dataRDS, -Location, -Geolocation)

#Remove columns which has always the same value like "Agency = MCP" in each row of data
dataRDS.cleaned = select(dataRDS.cleaned, -Agency, -Accident, -Commercial.Vehicle)

#Remove irrelevant/unnecessary columns
dataRDS.cleaned = select(dataRDS.cleaned, -Fatal, -Commercial.License, -HAZMAT, -Work.Zone, -State, -Model, 
                        -Color, -Charge, -Article, -DL.State)

#Add a new column "Local" to see if the driver is a local or a tourist
dataRDS.cleaned$Local <- ifelse(dataRDS.cleaned$Driver.State == "MD", TRUE, FALSE)
# Delete the Driver State column
dataRDS.cleaned = select(dataRDS.cleaned, -Driver.State)
#------------------ DATA CLEANIN END-----------




#------------------ PIE CHARTS BEGIN-----------
# Transform the native americans to "OTHER" 
dataRDS.cleaned$Race <- as.character(dataRDS.cleaned$Race)
dataRDS.cleaned$Race[dataRDS.cleaned$Race == "NATIVE AMERICAN"] <- "OTHER"
dataRDS.cleaned$Race <- as.factor(dataRDS.cleaned$Race)

#Pie diagram about the RACE of the people 
#Calculate the percentage for each race, using one decimal place.
percentLabelRace <- round(100*(table(dataRDS.cleaned$Race))/sum((table(dataRDS.cleaned$Race))), 1)
#Add a new line after the names of each gender 
nameLabelRace <- paste(names(table(dataRDS.cleaned$Race)), "\n", sep="")
raceAddPercentage <- paste(nameLabelRace, percentLabelRace , sep="")
#Add a ‘%’ sign to each percentage value using the paste command.
pieLabelRace <- paste(raceAddPercentage, "%", sep="")
# Print the pie chart
pie((table(dataRDS.cleaned$Race)), labels=pieLabelRace, cex=0.8, radius = 1)

#Pie diagram about the GENDER of the people 
#Calculate the percentage for each gender, using one decimal place.
percentLabelGender <- round(100*(table(dataRDS.cleaned$Gender))/sum((table(dataRDS.cleaned$Gender))), 1)
#Add a new line after the name of each gender 
nameLabelGender <- paste(names(table(dataRDS.cleaned$Gender)), "\n", sep="")
genderAddPercentage <- paste(nameLabelGender, percentLabelGender , sep="")
#Add a ‘%’ sign to each percentage value using the paste command.
pieLabelGender <- paste(genderAddPercentage, "%", sep="")
# Print the pie chart
pie((table(dataRDS.cleaned$Gender)), labels=pieLabelGender, cex=1.2, radius = 1)

# http://www.census.gov/quickfacts/table/PST045215/24
# Maryland Population considering race
populationRace <- c(6.5, 30.5, 9.5, 1.5, 52)
pie(populationRace, labels=c("Asian\n 6.5%", "Black\n 30.5%", "Hispanic\n 9.5%",
                             "Other\n 1.5%", "White\n 52%"), cex=0.9)
#------------------ PIE CHARTS END-----------
















