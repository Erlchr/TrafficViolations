trafficData <- read.csv("/Users/sonerercelik/Desktop/E-Business/Traffic_Violations.csv", header=T)
trafficData

data.cleaned = select(trafficData, -Geolocation, -Location)


library(dplyr)
library(lubridate)
library(chron)

test <- trafficData

res = mutate(test, timeIntervall = paste(seconds(trafficData$Date.Of.Stop)))

# Add column with weekdays
res = mutate(test, weekDay = paste( wday(as.Date(trafficData$Date.Of.Stop, format = "%m/%d/%Y"), label = TRUE)  ))

# TESTING:
# convert time in seconds
convertedInSecTimeIntervall = minutes(times(trafficData$Time.Of.Stop)) + hours(times(trafficData$Time.Of.Stop)) * 60 * 60
convertedInSecTimeIntervall

# assign seconds in intervall
  
  for(timeInSeconds in convertedInSecTimeIntervall){
    switch(timeInSeconds, 
         10800 <= {
           # case 'foo' here...
             timeInSeconds = '00:00 - 03:00'
             print(timeInSeconds)
           },
           10800*2 <= {
             # case 'bar' here...
             timeInSeconds = '03:01 - 06:00' 
             print(timeInSeconds)
           },
           10800*3 <= {
             # case 'bar' here...
             timeInSeconds = '06:01 - 09:00' 
             print(timeInSeconds)
           },
           10800*4 <= {
             # case 'bar' here...
             timeInSeconds = '09:01 - 12:00'
             print(timeInSeconds)
           },
           10800*5 <= {
             # case 'bar' here...
             timeInSeconds = '12:01 - 15:00' 
             print(timeInSeconds)
           },
           10800*6 <= {
             # case 'bar' here...
             timeInSeconds = '15:01 - 18:00' 
             print(timeInSeconds)
           },
           10800*7 <= {
             # case 'bar' here...
             timeInSeconds = '18:01 - 21:00'  
             print(timeInSeconds)
           },
           {
             timeInSeconds = '21:01 - 23:59'  
             print(timeInSeconds)
           }
    )
    sink()
  } 


convertedInSecTimeIntervall

# Archiv
MyDatesTable <- table(cut(as.Date(trafficData$Date.Of.Stop, breaks="day")))
data.frame(MyDatesTable)
plot(MyDatesTable, type="l", xlab="Time", ylab="Freq")


