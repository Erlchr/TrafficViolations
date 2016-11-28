# Libraries
library(sp)
library(raster)
library(dplyr)
library(ggmap)

library(devtools)
install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")

#---------- START HELPER METHODS ----------#

preparePolygon <- function(x) {
  rtp <- rasterToPolygons(x)
  rtp@data$id <- 1:nrow(rtp@data) # add id column for join
  
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  return(rtpFortMer)
}

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

r = raster(ncol=25, nrow=25, xmn=-77.5, xmx=-76.9, ymn=38.95, ymx=39.35)
r.rasterized = rasterize(coords[,2:1], r, coords[,3], fun = sum)
r.treshold = 0
values(r.rasterized) = ifelse(values(r.rasterized) < r.treshold, NA, values(r.rasterized))

r.values = values(r.rasterized); r.values = ifelse(r.values < r.treshold, NA, r.values)
r.topvalues = tail(sort(r.values), 10)
r.topvalues.indexes = match(r.topvalues, r.values)
r.topvalues.coordinates = xyFromCell(r.rasterized, cell = r.topvalues.indexes)
#r.topvalues.adresses = apply(r.topvalues.coordinates, 1, function(v) revgeocode(v))

map.maryland    = subset(getData("GADM", country="USA", level=2), NAME_1 == "Maryland")

polygon.montgomery  = ggmap(get_map(location = "Montgomery County", maptype = "roadmap", zoom = 10), extent = "device")
polygon.maryland    = geom_polygon(data=maryland, aes(x=long, y=lat), alpha = 0.1, fill = NA, color = "black") 
polygon.raster      = geom_polygon(data=preparePolygon(r.rasterized), aes(x = long, y = lat, group = group, fill = layer), size = 0, alpha=0.3) 
polygon.grid        = geom_line(data=preparePolygon(r), aes(x = long, y = lat, group = group), color = "gray47", size = 0.2) 
polygon.legend      = scale_fill_gradientn(limits = c(r.treshold, max(r.values, na.rm = T)), colours = topo.colors(255))

map.default = polygon.montgomery + polygon.maryland 
map.grid = polygon.montgomery + polygon.maryland + polygon.grid
map.violations = polygon.montgomery + polygon.maryland + polygon.grid + polygon.raster + polygon.legend

ggsave(map.default, file = "figures/map_default.png", width = 5, height = 4, type = "cairo-png")
ggsave(map.grid, file = "figures/map_grid.png", width = 5, height = 4, type = "cairo-png")
ggsave(map.violations, file = "figures/map_violations.png", width = 5, height = 4, type = "cairo-png")

png(filename="maryland_traffic.png", height=1000, width=1000, bg="NA")
apply(r.topvalues.coordinates, 1, function(v) points(v[1], v[2], col = "black", cex = .6))
dev.off()

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

#---------- END DATA VISUALIZATION ----------#








