# Libraries
library(devtools)
install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")

library(sp)
library(raster)
library(dplyr) 
select <- dplyr:: select
library(ggmap)
library(ggplot2)
library(utils)
library(lubridate)
library(chron) # Date
library(caret)
library(MASS)
library(rpart)
library(rpart.plot)
library(stringdist)
library(lattice)
library(caret)

#---------- START HELPER METHODS ----------#

#---------- END HELPER METHODS ----------#
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
data = data.raw

# Add Day & Interval
seconds = minutes(times(data$Time.Of.Stop)) * 60 + hours(times(data$Time.Of.Stop)) * 60 * 60
cuts <- c(-Inf, 6 * 60 * 60, 21600 * 2, 21600 * 3, Inf)
labs <- c(0,1,2,3)
assignedInterval = labs[findInterval(seconds, cuts)]
data = mutate(data, Interval = paste(assignedInterval))  

data$Interval = as.factor(data$Interval) # training_data <- training_data %>% mutate_if(is.character,as.factor)

# Add column with weekdays: 0=Sunday, 1=Monday, 2=Tuesday, 3=Wednesday, 4=Thursday, 5=Friday, 6=Saturday
data = mutate(data, Day = paste( wday(as.Date(data$Date.Of.Stop, format = "%m/%d/%Y"), label = FALSE)-1))
data$Day <- factor(data$Day, levels= c(0,1,2,3,4,5,6))

# Add Category
categories = c("Speed", "Red Lights", "License", "Registration", "Driving Failure", "Alcohol", "Electronic Device", "Unsafe Vehicle")
data = mutate(data, Category = as.character(data$Description))  

data$Category = gsub("LIC.", "LICENSE", data$Category)
data$Category = gsub("ZON", "ZONE", data$Category)
data$Category = gsub("(VEHICLE|VEHICL|VEH)", "VEHICLE", data$Category)
data$Category = gsub("HWY", "HIGHWAY", data$Category)
data$Category = gsub("(REGISTRATION|REGISTR|REG.)", "REGISTRATION", data$Category)
data$Category = gsub("REQUIR ", "REQUIRED", data$Category)
data$Category = gsub("MOT ", "MOTOR", data$Category)
data$Category = gsub("MSG.", "MESSAGE", data$Category)
data$Category = gsub("PLATES", "PLATE", data$Category)

data$Category = gsub(".*SPEED.*", "Speed", data$Category)
data$Category = gsub(".*(RED LIGHTS|RED SIGNAL|RED TRAFFIC SIGNAL|STOP LIGHTS|FAILURE TO STOP|STOPLIGHT|STOP AT  SIGN|REQUIRED STOP|STOP SIGN|STOP AT SIGN|RED ARROW).*", "Red Lights", data$Category)
data$Category = gsub(".*(LICENSE|PERMIT DRIVING|UNAUTHORIZED).*", "License", data$Category)
data$Category = gsub(".*(REGISTRATION|UNREGISTERED|PLATE).*", "Registration", data$Category)
data$Category = gsub(".*(AGGRESSIVE|NEGLIGENT|RECKLESS|TURN|BACKING|IMPRUDENT|PEDESTRIAN|SOUND|NOIS(E|Y)|FAILURE OF DRIVER|DRIVER (FAILING|FAILURE)|VEHICLE.*TOO CLOSELY|OVERTAKEN|OFF ROADWAY|ONE WAY|WRONG WAY|CARELESS|YIELD|LANE|IMPROPER ROAD POSITION|DRIVE RIGHT|YIELD RIGHT|RIGHT HALF|FOLLOWING VEHICLE CLOSER|PARKING|PARK).*", "Driving Failure", data$Category)
data$Category = gsub(".*(ALCOHOL|DRUG).*", "Alcohol", data$Category)
data$Category = gsub(".*(TELEPHONE|EARPHONES|EARPLUGS|PHONE|(READING|WRITING) .* MESSAGE|HEADSET).*", "Electronic Device", data$Category)
data$Category = gsub(".*(GLASS|LAMP|BELT|UNINSURED VEHICLE|LAMPS|HEADLIGHT|SUSPENSION|TAILLIGHT|DASH LIGHTS|EYE PROTECTION|SHOCKS|WHEEL|TURN SIGNALS|WINDSHIELD|UNSECURED LOADED VEHICLE|EXHAUST SYSTEM|WINDOW TINT|FAILURE OF VEHICLE|TIRES|MIRRORS|FENDERS|BUMPERS|HEADLIGHTS|BRAKING|REFLECTOR|UNSAFE VEHICLE|EQUIPMENT|TAG LIGHTS|TAILLIGHTS|EQUIP|BRAKE).*", "Unsafe Vehicle", data$Category)

data$Category = as.factor(ifelse(data$Category %in% categories, data$Category, "Other"))
summary(data$Category)

# Remove redundant location information
# Remove columns which has always the same value like "Agency = MCP" in each row of data
# Remove irrelevant/unnecessary columns for analysis
data = select(data, -Agency, -Accident, -Commercial.Vehicle)
data = select(data, -Location, -Geolocation) 
data = select(data, -Time.Of.Stop, -Date.Of.Stop)
data = select(data, -Commercial.License, -Work.Zone, -State, -Model, -DL.State)
data = select(data, -Fatal, -HAZMAT, -Charge, -Article)
data = select(data, -SubAgency, -Alcohol, -Driver.City, -Arrest.Type, -Driver.State)
data = select(data, -Belts)

# Update Vehicle Type
data$VehicleType = as.character(data$VehicleType)
data$VehicleType[data$VehicleType=="01 - Motorcycle"]           = "Motorcycle"
data$VehicleType[data$VehicleType=="02 - Automobile"]           = "Automobile"
data$VehicleType[data$VehicleType=="03 - Station Wagon"]        = "Automobile"
data$VehicleType[data$VehicleType=="04 - Limousine"]            = "Automobile"
data$VehicleType[data$VehicleType=="05 - Light Duty Truck"]     = "Truck"
data$VehicleType[data$VehicleType=="06 - Heavy Duty Truck"]     = "Truck"
data$VehicleType[data$VehicleType=="07 - Truck/Road Tractor"]   = "Truck"
data$VehicleType[data$VehicleType=="08 - Recreational Vehicle"] = "Truck"
data$VehicleType[data$VehicleType=="09 - Farm Vehicle"]         = "Truck"
data$VehicleType[data$VehicleType=="10 - Transit Bus"]          = "Truck"
data$VehicleType[data$VehicleType=="11 - Cross Country Bus"]    = "Truck"
data$VehicleType[data$VehicleType=="12 - School Bus"]           = "Truck"
data$VehicleType[data$VehicleType=="13 - Ambulance(Emerg)"]     = "Truck"
data$VehicleType[data$VehicleType=="14 - Ambulance(Non-Emerg)"] = "Truck"
data$VehicleType[data$VehicleType=="16 - Fire(Non-Emerg)"]      = "Truck"
data$VehicleType[data$VehicleType=="18 - Police(Non-Emerg)"]    = "Automobile"
data$VehicleType[data$VehicleType=="19 - Moped"]                = "Motorcycle"
data$VehicleType[data$VehicleType=="20 - Commercial Rig"]       = "Other"
data$VehicleType[data$VehicleType=="21 - Tandem Trailer"]       = "Other"
data$VehicleType[data$VehicleType=="22 - Mobile Home"]          = "Truck"
data$VehicleType[data$VehicleType=="23 - Travel/Home Trailer"]  = "Truck"
data$VehicleType[data$VehicleType=="24 - Camper"]               = "Truck"
data$VehicleType[data$VehicleType=="25 - Utility Trailer"]      = "Other"
data$VehicleType[data$VehicleType=="26 - Boat Trailer"]         = "Other"
data$VehicleType[data$VehicleType=="27 - Farm Equipment"]       = "Other"
data$VehicleType[data$VehicleType=="28 - Other"]                = "Other"
data$VehicleType[data$VehicleType=="29 - Unknown"]              = "Other"
data$VehicleType[is.na(data$VehicleType)]                       = "Other"

data$VehicleType = as.factor(data$VehicleType)

# Update Color
data$Color = as.character(data$Color)
data$Color[data$Color=="BEIGE"]       = "Color"
data$Color[data$Color=="BLACK"]       = "Black"
data$Color[data$Color=="BLUE"]        = "Color"
data$Color[data$Color=="BLUE, DARK"]  = "Color"
data$Color[data$Color=="BLUE, LIGHT"] = "Color"
data$Color[data$Color=="BRONZE"]      = "Color"
data$Color[data$Color=="BROWN"]       = "Color"
data$Color[data$Color=="CAMOUFLAGE"]  = "Color"
data$Color[data$Color=="CHROME"]      = "Gray"
data$Color[data$Color=="COPPER"]      = "Color"
data$Color[data$Color=="CREAM"]       = "Color"
data$Color[data$Color=="GOLD"]        = "Color"
data$Color[data$Color=="GRAY"]        = "Gray"
data$Color[data$Color=="GREEN"]       = "Color"
data$Color[data$Color=="GREEN, DK"]   = "Color"
data$Color[data$Color=="GREEN, LGT"]  = "Color"
data$Color[data$Color=="MAROON"]      = "Color"
data$Color[data$Color=="MULTICOLOR"]  = "Color"
data$Color[data$Color=="N/A"]     = "Other"
data$Color[data$Color=="ORANGE"]  = "Color"
data$Color[data$Color=="PINK"]    = "Color"
data$Color[data$Color=="PURPLE"]  = "Color"
data$Color[data$Color=="RED"]     = "Color"
data$Color[data$Color=="SILVER"]  = "Gray"
data$Color[data$Color=="TAN"]     = "Color"
data$Color[data$Color=="WHITE"]   = "White"
data$Color[data$Color=="YELLOW"]  = "Color"
data$Color[is.na(data$Color)]     = "Other"

data$Color = as.factor(data$Color)

# Update Race
data$Race = as.character(data$Race)
data$Race[data$Race=="NATIVE AMERICAN"]           = "OTHER"
data$Race = as.factor(data$Race)

# Update Make
makes <- c("ACURA", "ASTON MARTIN", "AUDI", "BENTLEY", "BENZ", "BMW", "CADILLAC", "BUICK", "CHEVROLET",
           "CHEVY", "CHRYSLER", "CORVETTE", "DAEWOO", "DODGE", "DUCATI", "FERRARI", "FORD", "FIAT",
           "GMC", "HONDA", "HUMMER", "HYUNDAI", "INFINITI", "ISUZU", "JAGUAR", "JEEP", "KAWASAKI",
           "KIA", "LAND ROVER", "LAMBORGHINI", "LEXUS", "LINCOLN", "LOTUS", "MAZDA", "MASERATI",
           "MINI", "MINI COOPER", "MITSUBISHI","NISSAN", "PEUGEOT", "PORSCHE",
           "PONTIAC", "RENAULT", "ROLLS ROYCE", "RANGE ROVER", "ROVER", "SAAB", "SEAT", "SKODA", "SMART",
           "SUBARU", "SUZUKI", "TESLA", "TOYOTA", "VOLKSWAGEN", "VOLVO", "YAMAHA")
data$Make = as.factor(makes[amatch(as.character(data$Make), makes, maxDist = 2)])




# Remove all rows where value of the "YEAR" is not between 1950-2017
data = filter(data, data$Year > 1950 & data$Year < 2017)
# Remove all rows where value of "Gender" is "U"
data = filter(data, data$Gender == "F" | data$Gender == "M")
# Remove all rows where value of "Color" is "Other"
data = filter(data, !(data$Color == "Other"))
# Reset factor levels to delete unused factors like "Other" in Color and "U" in Gender
data$Color <- factor(data$Color)
data$Gender <- factor(data$Gender)

# Remove NA coordinates rows 
data = filter(data, !is.na(data$Latitude), !is.na(data$Longitude))

# Update column "Violation.Type" to see if the driver is a local or a tourist
data$Violation.Type <- ifelse(data$Violation.Type == "Citation", "Yes", "No")
data$Personal.Injury <- ifelse(data$Personal.Injury == "Yes", 1, 0)
data$Property.Damage <- ifelse(data$Property.Damage == "Yes", 1, 0)
data$Contributed.To.Accident <- ifelse(data$Contributed.To.Accident == "Yes", 1, 0)
data$Contributed.To.Accident <- ifelse(data$Contributed.To.Accident == "Yes", 1, 0)
data$Gender_Num <- ifelse(data$Gender == "M", 0, 1)

data$Citation_Num <- ifelse(data$Violation.Type == "Yes", 1, 0)

data$Color_Num <- ifelse(data$Color == "White", 1,
                         ifelse(data$Color == "Gray", 2,
                                ifelse(data$Color == "Black", 3, 4)))

data$VehicleType_Num <- ifelse(data$VehicleType == "Motorcycle", 1,
                               ifelse(data$VehicleType == "Automobile", 2,
                                      ifelse(data$VehicleType == "Truck", 3, 4)))

data$Race_Num <- ifelse(data$Race == "ASIAN", 1,
                        ifelse(data$Race == "BLACK", 2,
                               ifelse(data$Race == "WHITE", 3,
                                      ifelse(data$Race == "HISPANIC", 4, 5))))


ranks <- rank(-table(data$Make), ties.method="first")
data$Make_Num <- ranks[as.character(data$Make)]



# Rename Columns
names(data)[names(data) == "Violation.Type"] = "Citation"
data$Citation = as.factor(data$Citation)

data$Interval_Num = as.numeric(data$Interval)
data$Day_Num = as.numeric(data$Day)

#------- START SONER --------#
#1: Split "data" into training_data and test_data:
set.seed(107)
inTrain <- createDataPartition(y = data$Citation, p = .9, list = FALSE)

training_data <- data[ inTrain,] ## 90% of original data
test_data  <- data[-inTrain,]

#2: 
#------------- START Bar Plots ---------------------#
# Create plot how often a traffic violation occurs depending on time interval
barplot(table(training_data$Interval),col="slategray2",border="black",
        xlab="Time Interval",
        ylab="Frequency of Traffic Violations",
        ylim=c(0,50000)
)
legend("top", legend = c("0 = 12am - 5.59am", "1 = 6am - 11.59am", "2 = 12pm - 5.59pm", "3 = 6pm - 11:59pm"))

# Create plot how often a traffic violation occurs depending on weekday
barplot(table(training_data$Day),col="slategray2",border="black",
        xlab="Weekday",
        ylab="Frequency of Traffic Violations",
        ylim=c(0,30000)
)
legend("top", legend = c("0 = Sun, 1 = Mon, 2 = Tues, 3 = Wed, 4 = Thu, 5 = Fri, 6 = Sat"))

# Create plot how often a traffic violation occurs depending on model year of the car
dataFiltered = filter(training_data, training_data$Year < 2017)
dataFiltered = filter(dataFiltered, dataFiltered$Year > 1995)
barplot(table(dataFiltered$Year),col="slategray2",border="black",
        xlab="Year of the car",
        ylab="Frequency of Traffic Violations",
        ylim=c(0,10000)
)
#------------- END Bar Plots ---------------------#

#------- END SONER --------#




#---------- END DATA PREPARATION ----------#
#---------- START DATA VISUALIZATION ----------#
#---------- END DATA VISUALIZATION ----------#

#---------- START DATA VISUALIZATION ----------#
#---------- END DATA VISUALIZATION ----------#


