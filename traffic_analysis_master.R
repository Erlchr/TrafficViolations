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

# Remove NA coordinates rows 
data = filter(data, !is.na(data$Latitude), !is.na(data$Longitude))

# Add a column "Local" to see if the driver is a local or a tourist
data$Local <- ifelse(data$Driver.State == "MD", TRUE, FALSE)
data = select(data, -Driver.State) # Remove Driver.State

<<<<<<< HEAD
#---------- END DATA PREPARATION ----------#
#---------- START DATA VISUALIZATION ----------#

# Traffic Violation Hotspots Analysis using raster package
# 1) Create Frame with Latitude & Longitude & Count (=1, needed for sum up)
# 2) Create Raster and rasterize the created Frame
# 3) Add Maryland Border Layer 
# 4) Plot
coords = select(data, Latitude, Longitude)
coords = mutate(coords, count = 1)

r.xmn = quantile(coords[,2], probs=0.05)
r.xmx = quantile(coords[,2], probs=0.95)
r.ymn = quantile(coords[,1], probs=0.05)
r.ymx = quantile(coords[,1], probs=0.95)

r = raster(ncol=100, nrow=100, xmn=-77.4, xmx=-77.0, ymn=38.9, ymx=39.4) # xmn=r.xmn, xmx=r.xmx, ymn=r.ymn, ymx=r.ymx)
r.rasterized = rasterize(coords[,2:1], r, coords[,3], fun = sum)

us = getData("GADM", country="USA", level=2)
maryland = subset(us, NAME_1 == "Maryland")

r.values = values(r.rasterized); r.values = r.values[!is.na(r.values)]
r.quantiles = c(.2,.4,.6,.8)
breakpoints <- c(quantile(r.values, probs=0.2), quantile(r.values, probs=0.5), quantile(r.values, probs=1))
colors <- c("transparent", "red")

png(filename="maryland_traffic.png", height=1000, width=1000, bg="NA")

values(r.rasterized)

xyFromCell(r.rasterized, cell = 2)

plot(r.rasterized) # breaks = breakpoints, col = colors)
plot(maryland, add = TRUE)
dev.off()

#---------- END DATA VISUALIZATION ----------#
=======
# Add a column to see at which weekday the violation occured
data = mutate(data, weekDay = paste( wday(as.Date(data$Date.Of.Stop, format = "%m/%d/%Y"), label = TRUE)  ))

# Add column time interval: 
# 1. Create column with converted time in seconds
convertedInSecTimeIntervall = minutes(times(data$Time.Of.Stop)) + hours(times(data$Time.Of.Stop)) * 60 * 60
# 2. Assign the seconds in intervall
cuts <- c(-Inf, 10800, 10800*2, 10800*3, 10800*4, 10800*5, 10800*6, 10800*7, Inf)
labs <- c("'00:00 - 02:59'", "03:00 - 05:59", "06:00 - 08:59", "09:00 - 11:59", "12:00 - 14:59", "15:00 - 17:59", "18:00 - 20:59", "21:00 - 23:59")
assignedInterval = labs[findInterval(convertedInSecTimeIntervall, cuts)]
# 3. Add column with useful values into data
data = mutate(data, timeInterval = paste (assignedInterval))

# Create plot how often a traffic violation occurs depending on time interval
freq_timeInteravl <- data.frame(table(data$timeInterval))
plot(freq_timeInteravl, type="l", xlab="Time Interval", ylab="Frequency")

#---------- END DATA PREPARATION ----------#
>>>>>>> e634bce1055faddbb0b1c2cc3bc0be7dbb7c1ce2
