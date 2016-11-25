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
data = readRDS(dataRDSFileName)
  
data.train <- as.data.frame(data[1:200,])
data.test <- as.data.frame(data[15,])

# Remove other states than Maryland (MD)
data.cleaned = select(data.train, -Geolocation, -Location)
data.cleaned = filter(data.cleaned, data.cleaned$State == "MD")

summary(data.cleaned)

coords = select(data.cleaned, Latitude, Longitude)
coords = mutate(coords, count = 1)
coords = filter(coords, !is.na(coords$Latitude) & !is.na(coords$Longitude))

r.xmn = quantile(coords[,2], probs=0.05)
r.xmx = quantile(coords[,2], probs=0.95)
r.ymn = quantile(coords[,1], probs=0.05)
r.ymx = quantile(coords[,1], probs=0.95)

r = raster(ncol=100, nrow=100, xmn=-77.4, xmx=-76.8, ymn=38.9, ymx=39.4) # xmn=r.xmn, xmx=r.xmx, ymn=r.ymn, ymx=r.ymx)
r.rasterized = rasterize(coords[,2:1], r, coords[,3], fun=sum)

us <- getData("GADM", country="USA", level=1)
# extract states (need to uppercase everything)
ne = us[match(c("MARYLAND"), us$NAME_1),]

# now use the mask function
r.values = values(r.rasterized); r.values = r.values[!is.na(r.values)]
r.quantiles = c(.2,.4,.6,.8)
breakpoints <- c(quantile(r.values, probs=.6), quantile(r.values, probs=.8), quantile(r.values, probs=1))
colors <- c("green", "blue", "red")

plot(r.rasterized, breaks = breakpoints, col = colors)
plot(ne, add = TRUE)

# saveRDS, data.table instead of data.frame