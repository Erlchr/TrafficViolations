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