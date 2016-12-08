# Libraries
library(devtools)
install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")

library(sp)
library(raster)
library(dplyr)
library(ggmap)
library(ggplot2)
library(utils)

#---------- START HELPER METHODS ----------#

preparePolygon <- function(x) {
  rtp <- rasterToPolygons(x)
  rtp@data$id <- 1:nrow(rtp@data) # add id column for join
  
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  return(rtpFortMer)
}

drawCityMaps <- function(c) {
  x = c[1]; y = c[2]; span = 0.04
  
  r.city = raster(ncol=25, nrow=25, xmn=x-span, xmx=x+span, ymn=y-span, ymx=y+span)
  r.city.rasterized = rasterize(coords[,2:1], r.city, coords[,3], fun = sum)
  r.city.treshold = 100
  values(r.city.rasterized) = ifelse(values(r.city.rasterized) < r.city.treshold, NA, values(r.city.rasterized))
  
  r.city.values = values(r.city.rasterized);
  r.city.topvalues = tail(sort(r.city.values), 5)
  r.city.topvalues.indexes = match(r.city.topvalues, r.city.values)
  r.city.topvalues.coordinates = xyFromCell(r.city.rasterized, cell = r.city.topvalues.indexes)
  
  polygon.city        = ggmap(get_map(location = c(lon = x, lat = y), maptype = "roadmap", zoom = 13), extent = "device")
  polygon.city.raster = geom_polygon(data=preparePolygon(r.city.rasterized), aes(x = long, y = lat, group = group, fill = layer), size = 0, alpha=0.6) 
  polygon.city.grid   = geom_line(data=preparePolygon(r.city), aes(x = long, y = lat, group = group), color = "gray47", size = 0.2) 
  polygon.city.legend = scale_fill_continuous(guide = "legend", limits = c(r.city.treshold, max(r.city.values, na.rm = T)), low = "red1", high = "red4")

  map = polygon.city + polygon.maryland + polygon.city.grid + polygon.city.raster + polygon.city.legend
  
  ggsave(map, file = sprintf("figures/map_city_%.3f_%.3f.png",c[1], c[2]), width = 5, height = 4, type = "cairo-png")
  
  apply(r.city.topvalues.coordinates, 1, function(v) revgeocode(v))
}

map <- function(location = "Montgomery County", zoom = 10, show.border = TRUE) {
  montgomery = ggmap(get_map(location = location, maptype = "terrain", zoom = zoom), extent = "device")
    
  if (show.border) { 
    maryland = subset(getData("GADM", country="USA", level=2), NAME_1 == "Maryland")
    montgomery = montgomery + geom_polygon(data=maryland, aes(x=long, y=lat), alpha = 0.1, fill = NA, color = "black") 
  }
      
  return(montgomery)
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

### Traffic Violation Hotspots Analysis
# Create Frame with Latitude & Longitude
coords = select(data, Latitude, Longitude); coords = coords[,2:1]

# Traffic Violation Hotspots Analysis using kmeans clustering
# 1) Find optimal k
# 2) Execute kmeans algorithm
# 3) Create lookup table to convert cluster numbers to locality names, and override afterwards
# 4) Visualize & save Montgomery map, violation points and clusters
# 5) Add cluster to frame 
hotspot.wss <- numeric(15) 
for (k in 1:15) hotspot.wss[k] <- sum(kmeans(coords, centers=k)$withinss)
plot(hotspot.wss)

hotspot.kmeans          = kmeans(coords[,1:2], 6)
hotspot.lookup          = mutate(as.data.frame(hotspot.kmeans$centers), Location = as.factor(apply(hotspot.kmeans$centers, 1, function(v) revgeocode(v, output = "more")$locality)))
hotspot.kmeans$cluster  = sapply(hotspot.kmeans$cluster, function(v) hotspot.lookup[v,]$Location)
hotspot.centers         = as.data.frame(hotspot.kmeans$centers)

hotspots.map = map() +
  geom_point(coords, mapping=aes(x=Longitude, y=Latitude, color=hotspot.kmeans$cluster), size=1) + 
  geom_point(hotspot.centers, mapping=aes(x=Longitude, y=Latitude), size=2) +
  geom_point(hotspot.centers, mapping=aes(x=Longitude, y=Latitude), size=10, alpha=0.3) + 
  labs(colour="Hotspots")

ggsave(hotspots.map, file = "figures/map_hotspots.png", width = 10, height = 8)

coords = mutate(coords, Cluster = hotspot.kmeans$cluster)

# Traffic Violation Hotspots Analysis using raster package
# 1) Create Frame with Latitude & Longitude & Count (=1, needed for sum up)
# 2) Create Raster and rasterize the created Frame
# 3) Add Maryland Border Layer 
# 4) Plot
# coords = mutate(coords, count = 1)

analyseLocation <- function(data, location, show.grid=TRUE, show.raster=TRUE) {
  cs = as.matrix(location[1:2]); x = cs[1]; y = cs[2]; span = 0.04; data = data[,1:2];

  raster = raster(ncol=50, nrow=50, xmn=x-span, xmx=x+span, ymn=y-span, ymx=y+span)
  rasterized = rasterize(data, raster, 1, fun = sum)
  treshold = mean(values(rasterized), na.rm=TRUE)
  values(rasterized) = ifelse(values(rasterized) < treshold, NA, values(rasterized))
  
  # Map
  map = map(location=cs, zoom=13) 
   scale_fill_continuous(guide = "legend", limits = c(treshold, max(values(rasterized), na.rm = T)), low = "red1", high = "red4")
  
  if(show.grid)   { map = map + geom_polygon(data=preparePolygon(rasterized), aes(x = long, y = lat, group = group, fill = layer)) }
  if(show.raster) { map = map + geom_line(data=preparePolygon(raster), aes(x = long, y = lat, group = group), color = "gray47", size = 0.2) }
   
  # Top Adresse
  topvalues = xyFromCell(rasterized, cell = match(tail(sort(values(rasterized)), 5), values(rasterized)))
  return(list(location[3], map, apply(topvalues, 1, function(v) revgeocode(v))))
}

result = lapply(1:nrow(hotspot.lookup), function(v) analyseLocation(data=coords, location = hotspot.lookup[v,]))

for(i in 1:length(result)) {
  ggsave(result[[i]][[2]], file = sprintf("figures/map_%s.png", result[[i]][[1]]$Location), width = 10, height = 8)
}

# Traffic Violation Hotspots Analysis using raster package
# 1) Create Frame with Latitude & Longitude & Count (=1, needed for sum up)
# 2) Create Raster and rasterize the created Frame
# 3) Add Maryland Border Layer 
# 4) Plot


r = raster(ncol=25, nrow=25, xmn=-77.5, xmx=-76.9, ymn=38.95, ymx=39.35)
r.rasterized = rasterize(coords[,2:1], r, coords[,3], fun = sum)
r.treshold = 500
values(r.rasterized) = ifelse(values(r.rasterized) < r.treshold, NA, values(r.rasterized))

r.values = values(r.rasterized); r.values = ifelse(r.values < r.treshold, NA, r.values)
r.topvalues = tail(sort(r.values), 10)
r.topvalues.indexes = match(r.topvalues, r.values)
r.topvalues.coordinates = xyFromCell(r.rasterized, cell = r.topvalues.indexes)
# r.topvalues.adresses = apply(r.topvalues.coordinates, 1, function(v) revgeocode(v))

map.maryland    = subset(getData("GADM", country="USA", level=2), NAME_1 == "Maryland")

polygon.montgomery  = ggmap(get_map(location = "Montgomery County", maptype = "roadmap", zoom = 10), extent = "device")
polygon.maryland    = geom_polygon(data=map.maryland, aes(x=long, y=lat), alpha = 0.1, fill = NA, color = "black") 
polygon.raster      = geom_polygon(data=preparePolygon(r.rasterized), aes(x = long, y = lat, group = group, fill = layer), size = 0, alpha=0.3) 
polygon.grid        = geom_line(data=preparePolygon(r), aes(x = long, y = lat, group = group), color = "gray47", size = 0.2) 
polygon.legend      = scale_fill_continuous(guide = "legend", limits = c(r.treshold, max(r.values, na.rm = T)), low = "red1", high = "red4")
polygon.points      = geom_point(data = as.data.frame(r.topvalues.coordinates), aes(x=x, y=y), color="gray47", size = 2)

map.default = polygon.montgomery + polygon.maryland 
map.grid = polygon.montgomery + polygon.maryland + polygon.grid
map.violations = polygon.montgomery + polygon.maryland + polygon.grid + polygon.raster + polygon.legend
map.violations2 = polygon.montgomery + polygon.maryland + polygon.points

ggsave(map.default, file = "figures/map_default.png", width = 5, height = 4, type = "cairo-png")
ggsave(map.grid, file = "figures/map_grid.png", width = 5, height = 4, type = "cairo-png")
ggsave(map.violations, file = "figures/map_violations.png", width = 5, height = 4, type = "cairo-png")
ggsave(map.violations2, file = "figures/map_violations2.png", width = 5, height = 4, type = "cairo-png")

apply(r.topvalues.coordinates, 1, function(v) drawCityMaps(v))

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
