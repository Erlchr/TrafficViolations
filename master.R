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

# File Names
dataCSVFileName = "traffic_violations.csv"
dataRDSFileName = "traffic_violations.rds"

# Save CSV as Binary
if(!file.exists(dataRDSFileName)) {
  dataCSV = read.csv(dataCSVFileName)
  saveRDS(dataCSV, dataRDSFileName)
}

# Read Data
data = readRDS(dataRDSFileName)

remove(dataCSVFileName)
remove(dataRDSFileName)

#---------- DATA OBSERVATION ----------#
observation.data = data

# Time Interval: 00:00 - 05:59 = 0, 06:00 - 11:59 = 0, 12:00 - 17:59 = 0, 18:00 - 23:59 = 0
observation.data.seconds  = minutes(chron::times(data$Time.Of.Stop)) * 60 + hours(chron::times(data$Time.Of.Stop)) * 60 * 60
observation.data.labs     = c(0,1,2,3)
observation.data.interval = observation.data.labs[findInterval(observation.data.seconds, c(-Inf, 6 * 60 * 60, 21600 * 2, 21600 * 3, Inf))]

observation.data = mutate(observation.data, Interval = as.factor(observation.data.interval))  

remove(observation.data.seconds)
remove(observation.data.labs)
remove(observation.data.interval)

# Time Interval: 0 = Sunday, 1 = Monday, 2 = Tuesday, 3 = Wednesday, 4 = Thursday, 5 = Friday, 6 = Saturday
observation.data.days = ordered(wday(as.Date(data$Date.Of.Stop, format = "%m/%d/%Y"), label = FALSE)-1, levels= c(0,1,2,3,4,5,6))
observation.data = mutate(observation.data, Day = observation.data.days)

remove(observation.data.days)

# Plot Interval & Weekdays
barplot(table(observation.data$Interval), col = "slategray2", xlab = "Interval", ylab = "Frequency of Traffic Violations", ylim = c(0, 50000))
legend("top", legend = c("0 = 12am - 5.59am", "1 = 6am - 11.59am", "2 = 12pm - 5.59pm", "3 = 6pm - 11:59pm"))

barplot(table(observation.data$Day), col = "slategray2", xlab = "Weekday", ylab = "Frequency of Traffic Violations", ylim = c(0,30000))
legend("top", legend = c("S - M - T - W - T - F - S"))

# Plot Year of the car
observation.data.years = filter(observation.data, observation.data$Year < 2017, observation.data$Year > 1995)
barplot(table(observation.data.years$Year), col = "slategray2", xlab = "Year", ylab = "Frequency of Traffic Violations", ylim=c(0,10000))

remove(observation.data.years)

# Plot Race
observation.data.race.percentage  = round(100 * (table(observation.data$Race)) / sum((table(observation.data$Race))), 1)
observation.data.race.name        = paste(names(table(observation.data$Race)), "\n", sep="")

observation.data.race.label = paste(observation.data.race.name, observation.data.race.percentage , sep="")
observation.data.race.label = paste(observation.data.race.label, "%", sep="")

pie((table(observation.data$Race)), labels = observation.data.race.label, cex = 1.7, radius = 0.8, family = "serif")

remove(observation.data.race.percentage)
remove(observation.data.race.name )
remove(observation.data.race.label)

# Plot Gender
observation.data.gender.percentage  = round(100 * (table(observation.data$Gender)) / sum((table(observation.data$Gender))), 1)
observation.data.gender.name        = paste(names(table(observation.data$Gender)), "\n", sep="")

observation.data.gender.label = paste(observation.data.gender.name, observation.data.gender.percentage , sep="")
observation.data.gender.label = paste(observation.data.gender.label, "%", sep="")

pie((table(observation.data$Gender)), labels = observation.data.gender.label, cex = 1.7, radius = 0.8, family = "serif")

remove(observation.data.gender.percentage)
remove(observation.data.gender.name )
remove(observation.data.gender.label)

# Plot Race Maryland Population
# http://www.census.gov/quickfacts/table/PST045215/24
observation.data.race.population.labels = c("ASIAN\n 6.5%", "BLACK\n 30.5%", "HISPANIC\n 9.5%", "OTHER\n 1.5%", "WHITE\n 52%")
pie(c(6.5, 30.5, 9.5, 1.5, 52), labels = observation.data.race.population.labels, cex = 1.7, radius = 0.8, family="serif")

remove(observation.data.race.population.labels)
remove(observation.data)

#---------- DATA PREPARATION ----------#

# Update Column Violation.Type to Citation
names(data)[names(data) == "Violation.Type"] = "Citation"
data$Citation = ifelse(data$Citation == "Citation", "Yes", "No")
data$Citation = as.factor(data$Citation)

# Remove Rows with Latitude & Longitude
data = filter(data, !is.na(data$Longitude), !is.na(data$Latitude))

preparePolygon <- function(x) {
  rtp <- rasterToPolygons(x)
  rtp@data$id <- 1:nrow(rtp@data) # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  return(rtpFortMer)
}

create_map <- function(location = "Montgomery County", zoom = 10) {
  montgomery = ggmap(get_map(location = location, maptype = "terrain", zoom = zoom), extent = "device")
  maryland = subset(getData("GADM", country = "USA", level = 2), NAME_1 == "Maryland")
  montgomery = montgomery + geom_polygon(data=maryland, aes(x=long, y=lat), alpha = 0.1, fill = NA, color = "black") 
  return(montgomery)
}

#-------UNIFYING DESCRIPTIONS BASED ON CHARGE-------

# store the different levels of charge
levelsCharge = unique(data$Charge)

# same charges have sometimes different descriptions
# we unify the descriptions, so that a charge has always the same description
for (charge in levelsCharge) {
  data$Description[data$Charge == charge] = names(summary(filter(data, data$Charge == charge)$Description))[1]
}

data$Description = droplevels(data$Description)

remove(levelsCharge)


# Cleaning the descriptions more 
data$Description = gsub(" LIC.", "LICENSE", data$Description)
data$Description = gsub("ZON ", "ZONE", data$Description)
data$Description = gsub("(VEHICLE|VEHICL|VEH)", "VEHICLE", data$Description)
data$Description = gsub("HWY", "HIGHWAY", data$Description)
data$Description = gsub("(REGISTRATION|REGISTR|REG)", "REGISTRATION", data$Description)
data$Description = gsub("REQUIR ", "REQUIRED", data$Description)
data$Description = gsub("MOT ", "MOTOR", data$Description)
data$Description = gsub("MSG.", "MESSAGE", data$Description)
data$Description = gsub("(MOTOR|MOT) ", "MOTOR", data$Description)
data$Description = gsub("PLATES", "PLATE", data$Description)
data$Description = gsub("(UNSAFE|UNSAF)", "UNSAFE", data$Description)
data$Description = gsub("(DRIVING|DRIVEN|DRIVER)", "DRIVE", data$Description)
data$Description = gsub("PROPERTI", "PROPERTY", data$Description)
data$Description = gsub("(DEVICE|DEVIC)", "DEVICE", data$Description)
data$Description = gsub("(LICENSEE|LICENSE|LICENS|LIC)", "LICENSE", data$Description)

#-------UNIFYING DESCRIPTIONS BASED ON CHARGE-------


#-------------- ANALYZING THE SIMILARITIES BETWEEN CHARGES AND DESCRIPTIONS --------------- 
descrData = select(data, Description, Personal.Injury, Property.Damage, Alcohol, Citation, Charge, Contributed.To.Accident)

descrCit = filter(descrData, descrData$Citation == "Yes")
descrNoCit = filter(descrData, descrData$Citation == "No")

# Descriptions of the most Charges - take the first 35 descriptions
chargeCit = head(names(summary(descrCit$Charge)), 35)
chargeNoCit = head(names(summary(descrNoCit$Charge)), 35)

# Common charges which appear when citation doesn't matter
commonCharge = intersect(chargeCit, chargeNoCit)

# only the charges which leads always to citation
bestChargeCit = chargeCit[!chargeCit %in% commonCharge]

#-------------- ANALYZING THE SIMILARITIES BETWEEN CHARGES AND DESCRIPTIONS --------------- 

#------------------- CORPUS CLEANING ----------------------------

corpusCit <- Corpus(VectorSource(descrCit$Description))
corpusNoCit <- Corpus(VectorSource(descrNoCit$Description))

corpusCit <- tm_map(corpusCit, tolower)
corpusCit <- tm_map(corpusCit, removePunctuation)
corpusCit <- tm_map(corpusCit, removeWords, stopwords("english"))
corpusCit <- tm_map(corpusCit, stripWhitespace)  
corpusCit <- tm_map(corpusCit, stemDocument)
corpusCit <- tm_map(corpusCit, removeNumbers)

corpusNoCit <- tm_map(corpusNoCit, tolower)
corpusNoCit <- tm_map(corpusNoCit, removePunctuation)
corpusNoCit <- tm_map(corpusNoCit, removeWords, stopwords("english"))
corpusNoCit <- tm_map(corpusNoCit, stripWhitespace)  
corpusNoCit <- tm_map(corpusNoCit, stemDocument)
corpusNoCit <- tm_map(corpusNoCit, removeNumbers)


#tell R to treat processed documents as text
corpusCit <- tm_map(corpusCit, PlainTextDocument)
corpusNoCit <- tm_map(corpusNoCit, PlainTextDocument)   

#create  document term matrix 
dtmCit = DocumentTermMatrix(corpusCit)
dtmNoCit = DocumentTermMatrix(corpusNoCit)

#lets trim down and remove terms
#remove words that are over 99% sparse (i.e., do not appear in 98% of documents)
dtmCit = removeSparseTerms(dtmCit, 0.997)
dtmNoCit = removeSparseTerms(dtmNoCit, 0.997)

dtm_df <- data.frame(as.matrix(dtm))
head(dtm_df)


cit = colSums(as.matrix(dtmCit))
cit = subset(cit, cit >= 1500)
v <- sort(cit, decreasing=TRUE)
word.freqCit <- data.frame(term = names(v),freq=v)
word.freqCit #lets see frequency of words


library(ggplot2)
ggplot(word.freqCit, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()


#lets make a bar chart of frequent words
barplot(word.freqCit[1:15,]$freq, las = 2, names.arg = word.freqCit[1:15,]$term,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
# plot word cloud
wordcloud(words = names(v), freq = v, min.freq = 3,
          random.order = F, colors = pal)

noCit = colSums(as.matrix(dtmNoCit))
noCit = subset(noCit, noCit >= 1500)
w <- sort(noCit, decreasing=TRUE)
word.freqNoCit <- data.frame(term = names(w),freq=w)
word.freqNoCit #lets see frequency of words


library(ggplot2)
ggplot(word.freqNoCit, aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Terms") + ylab("Count") + coord_flip()


#lets make a bar chart of frequent words
barplot(word.freqNoCit[1:15,]$freq, las = 2, names.arg = word.freqNoCit[1:15,]$term,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
# plot word cloud
wordcloud(words = names(w), freq = w, min.freq = 3,
          random.order = F, colors = pal)


remove(descrData, descrCit, descrNoCit, chargeCit, chargeNoCit, commonCharge, bestChargeCit)
remove(word.freqNoCit, word.freqCit, charge, cit, corpusNoCit, corpusCit, dtmNoCit, dtmCit, noCit, pal, v, w)

#------------------- CORPUS CLEANING ----------------------------

# Adding a new column which tells us the probability if a particular description leads to a citation
for (description in unique(data$Description)) {
  total =  filter(data, data$Description == description)
  citations = filter(total, total$Citation == "Yes")
  percentage = (nrow(citations) / nrow(total))
  
  valueProb <- ifelse(percentage < 0.25 , "very low",
                      ifelse(percentage < 0.5, "low",
                             ifelse(percentage < 0.75, "high", "very high")))
  data$DescriptionProb[data$Description == description] = valueProb 
}

data$DescriptionProb = ordered(data$DescriptionProb, levels=c("very low", "low", "high", "very high"))

# Change the Description probabilities if other columns imply a citation - very low -> high;  low -> very high
for (rowIndex in 1:nrow(data)) {
  row = data[rowIndex, ]
  if (row$Property.Damage == "Yes" | row$Personal.Injury == "Yes" | row$Fatal == "Yes" | row$Alcohol == "Yes" | row$Contributed.To.Accident == "Yes") {
    if (row$DescriptionProb == "very low") {
      data$DescriptionProb[rowIndex] = "high"
    } else if (row$DescriptionProb == "low") {
      data$DescriptionProb[rowIndex] = "very high"
    }
  }
}

remove(charge, description, rowIndex, total, citations, percentage, valueProb, row)



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

remove(makes)

# Remove all rows where value of the "YEAR" is not between 1950-2017
data = filter(data, data$Year > 1950 & data$Year < 2017)
data = filter(data, data$Gender == "F" | data$Gender == "M")
data = filter(data, !(data$Color == "Other"))
data$Color <- factor(data$Color)
data$Gender <- factor(data$Gender)

# Update column "Violation.Type" to see if the driver is a local or a tourist
data$Personal.Injury_Num <- ifelse(data$Personal.Injury == "Yes", 1, 0)
data$Property.Damage_Num <- ifelse(data$Property.Damage == "Yes", 1, 0)
data$Contributed.To.Accident_Num <- ifelse(data$Contributed.To.Accident == "Yes", 1, 0)
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

remove(ranks)

###
# Traffic Violation Blackspot Analysis
data.coords = select(data, Latitude, Longitude); data.coords = data.coords[,2:1]

# Traffic Violation Hotspots Analysis using kmeans clustering
# 1) Find optimal k
# 2) Execute kmeans algorithm
# 3) Create lookup table to convert cluster numbers to locality names, and override afterwards
# 4) Visualize & save Montgomery map, violation points and clusters
# 5) Add cluster to frame 
hotspot.wss <- numeric(15) 
for (k in 1:15) hotspot.wss[k] <- sum(kmeans(data.coords, centers=k)$withinss)
plot(hotspot.wss)

hotspot.kmeans          = kmeans(data.coords[,1:2], 6)
hotspot.lookup          = mutate(as.data.frame(hotspot.kmeans$centers), Location = as.factor(apply(hotspot.kmeans$centers, 1, function(v) revgeocode(v, output = "more")$locality)))
hotspot.kmeans$cluster  = sapply(hotspot.kmeans$cluster, function(v) hotspot.lookup[v,]$Location)
hotspot.centers         = as.data.frame(hotspot.kmeans$centers)

hotspots.map = create_map() +
  geom_point(data.coords, mapping=aes(x=Longitude, y=Latitude, color=hotspot.kmeans$cluster), size=1) + 
  geom_point(hotspot.centers, mapping=aes(x=Longitude, y=Latitude), size=2) +
  geom_point(hotspot.centers, mapping=aes(x=Longitude, y=Latitude), size=10, alpha=0.3) + 
  labs(colour="Hotspots")
ggsave(hotspots.map, file = "figures/map_hotspots.png", width = 10, height = 8)

data.coords$Cluster   = hotspot.kmeans$cluster
data.coords$Citation  = data$Citation

remove(hotspot.wss)
remove(hotspots.map)
remove(hotspot.kmeans)
remove(hotspot.lookup)
remove(hotspot.centers)
remove(k)

# Traffic Violation Hotspots Analysis using raster package
# 1) Create Frame with Latitude & Longitude & Count (=1, needed for sum up)
# 2) Create Raster and rasterize the created Frame
# 3) Add Maryland Border Layer 
# 4) Plot
rastered = raster(ncol=100, nrow=100, xmn=-77.52, xmx=-76.9, ymn=38.93, ymx=39.35)

rasterized = rasterize(data.coords[,1:2], rastered, 1, fun = sum)

rasterized.prob = rasterize(data.coords[,1:2], rastered, ifelse(data.coords$Citation == "Yes", 1, 0) , fun = sum)
values(rasterized.prob) = values(rasterized.prob) / values(rasterized)

rasterized.highProb = rasterize(data.coords[,1:2], rastered, 1, fun = sum)
values(rasterized.highProb) = ifelse(values(rasterized.prob) > 0.45, TRUE, FALSE)

data$prob = values(rasterized.prob)[cellFromXY(rasterized.prob, data.coords[,1:2])]
data$prob[is.na(data$prob)] = 0
data$HighProb = ifelse(data$prob > 0.45, TRUE, FALSE)
data$HighProb_Num = ifelse(data$prob > 0.45, 1, 0)

# Map
rasterized.map = create_map() 
rasterized.map = rasterized.map + geom_polygon(data=preparePolygon(rasterized), aes(x = long, y = lat, group = group, fill = layer)) 
rasterized.map = rasterized.map + geom_line(data=preparePolygon(rastered), aes(x = long, y = lat, group = group), color = "gray47", size = 0.2) 
rasterized.map = rasterized.map + scale_fill_gradient(trans = "log", low = "red1", high = "red4")
ggsave(rasterized.map, file = "figures/map_rasterized.png", width = 10, height = 8)

# Map
rasterized.prob.map = create_map() +
  scale_fill_continuous(guide = "legend", limits = c(min(values(rasterized.prob), na.rm = T), max(values(rasterized.prob), na.rm = T)), low = "red1", high = "red4")
rasterized.prob.map = rasterized.prob.map + geom_polygon(data=preparePolygon(rasterized.prob), aes(x = long, y = lat, group = group, fill = layer)) 
rasterized.prob.map = rasterized.prob.map + geom_line(data=preparePolygon(rastered), aes(x = long, y = lat, group = group), color = "gray47", size = 0.2) 
ggsave(rasterized.prob.map, file = "figures/map_rasterized_prob.png", width = 10, height = 8)

rasterized.highprob.map = create_map() +
  scale_fill_continuous(guide = "legend", low = "red1", high = "red4")
rasterized.highprob.map = rasterized.highprob.map + geom_polygon(data=preparePolygon(rasterized.highProb), aes(x = long, y = lat, group = group, fill = layer)) 
rasterized.highprob.map = rasterized.highprob.map + geom_line(data=preparePolygon(rastered), aes(x = long, y = lat, group = group), color = "gray47", size = 0.2) 
ggsave(rasterized.highprob.map, file = "figures/map_rasterized_highprob.png", width = 10, height = 8)

# Top Adresse
topvalues = xyFromCell(rasterized, cell = match(tail(sort(values(rasterized)), 10), values(rasterized)))
topvalues.addresses = apply(topvalues, 1, function(c) revgeocode(c))

map.topvalues = create_map() +
  geom_point(as.data.frame(topvalues), mapping=aes(x=x, y=y, color="1"), size=3) +
  scale_colour_manual(name="", values = c("1"="#0070C0"))
ggsave(map.topvalues, file = "figures/map_topvalues.png", width = 10, height = 8)

write(topvalues.addresses, file = "addresses.txt", sep = "\n")

remove(rastered)
remove(rasterized)
remove(rasterized.prob)
remove(rasterized.highProb)
remove(rasterized.map)
remove(rasterized.prob.map)
remove(rasterized.highprob.map)
remove(topvalues)
remove(topvalues.addresses)
remove(map.topvalues)
remove(data.coords)


#--------------------------------------#
#----------- MODEL PLANNING -----------#

#--------------------------------------#
#----------- MODEL BUILDING -----------#

set.seed(107)
inTrain <- createDataPartition(y = data$Citation, p = .9, list = FALSE)

training_data <- data[ inTrain,] ## 90% of original data
test_data  <- data[-inTrain,]

library("doParallel")
library(pROC)

cl <- makeCluster(4)
registerDoParallel(cl)

ctrl = trainControl(method="repeatedcv", number=5, repeats=2, summaryFunction = twoClassSummary, classProbs = TRUE)

model = as.formula(Citation ~ prob) 
model_glm = train(model, data=training_data, method="glm", trControl=ctrl, metric="Accuracy")
model_glm <- train (model, data = training_data, method = "glm", metric="Accuracy", trControl=ctrl)
model_rf <- train (model, data = training_data, method = "rf")
model_svm <- train (model, data = training_data, method = "svmRadial")
model_tree <- train (model, data = training_data, method = "class")

saveRDS(model_glm, "model_glm.rds")
saveRDS(model_rf, "model_rf.rds")
saveRDS(model_svm, "model_svm.rds")
saveRDS(model_tree, "model_tree.rds")

model_glm <- readRDS("model_glm.rds")
model_rf <- readRDS("model_rf.rds")
model_svm <- readRDS("model_svm.rds")
model_tree <- readRDS("model_tree.rds")

prediction_glm = predict(model_glm, test_data)
prediction_rf = predict(model_rf, test_data)
prediction_svm = predict(model_svm, test_data)
prediction_tree = predict(model_tree, test_data)

confusionMatrix(prediction_glm, test_data$Citation)
confusionMatrix(prediction_rf, test_data$Citation)
confusionMatrix(prediction_svm, test_data$Citation)
confusionMatrix(prediction_tree, test_data$Citation)

stopCluster(cl)

remove(cl)
remove(model)
remove(ctrl)
remove(model_glm)
remove(model_rf)
remove(model_svm)
remove(model_tree)
remove(prediction_glm)
remove(prediction_rf)
remove(prediction_svm)
remove(prediction_tree)

#--------------------------------------#