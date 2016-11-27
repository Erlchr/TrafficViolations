library(dplyr)
library(lubridate)
library(chron)

data <- read.csv("/Users/sonerercelik/Desktop/E-Business/Traffic_Violations.csv", header=T)

########################### Start - Von Master eingefügt:

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

############################# End

############################# Start - Own Work
# Add column with weekdays
data = mutate(data, weekDay = paste( wday(as.Date(data$Date.Of.Stop, format = "%m/%d/%Y"), label = TRUE)  ))

# column with converted time in seconds
convertedInSecTimeIntervall = minutes(times(data$Time.Of.Stop)) + hours(times(data$Time.Of.Stop)) * 60 * 60

# assign the seconds in intervall
cuts <- c(-Inf, 10800, 10800*2, 10800*3, 10800*4, 10800*5, 10800*6, 10800*7, Inf)
labs <- c("00:00 - 02:59", "03:00 - 05:59", "06:00 - 08:59", "09:00 - 11:59", "12:00 - 14:59", "15:00 - 17:59", "18:00 - 20:59", "21:00 - 23:59")
assignedInterval = labs[findInterval(convertedInSecTimeIntervall, cuts)]

# Add column time intervall 
data = mutate(data, timeInterval = paste (assignedInterval))

# Create plot how often a traffic violation occurs depending on time interval
barplot(table(data$timeInterval),col="slategray2",border="black",
        xlab="Time Interval",
        ylab="Frequency of Traffic Violations",
        ylim=c(0,35000)
)

# Create plot how often a traffic violation occurs depending on weekday
data$weekDay <- factor(data$weekDay, levels= c("Sun", "Mon","Tues", "Wed", "Thurs", "Fri", "Sat"))
barplot(table(data$weekDay),col="slategray2",border="black",
        xlab="Weekday",
        ylab="Frequency of Traffic Violations",
        ylim=c(0,30000)
        )



# TESTING not important:

# Archiv
MyDatesTable <- table(cut(as.Date(trafficData$Date.Of.Stop)))
data.frame(MyDatesTable)
plot(MyDatesTable, type="l", xlab="Time", ylab="Freq")

# dont work: plot(data$timeInterval)
# x muss numerisch sein: b <- hist(data$timeInterval, plot=FALSE)

# plot(freq_timeInterval, type="l", xlab="Time Interval", ylab="Frequency of Traffic Violations")

freq_dayInterval$Var1 <- factor(freq_dayInterval$Var1, levels= c("Sun", "Mon","Tues", "Wed", "Thurs", "Fri", "Sat"))
freq_dayInterval[order(freq_dayInterval$Var1), ]
str(freq_dayInterval)

# plot(freq_dayInterval, type="l", xlab="Weekday", ylab="Frequency of Traffic Violations")
# hist(freq_dayInterval)

freq_dayInterval$Var1 <- as.numeric(freq_dayInterval$Var1)
freq_dayInterval
str(freq_dayInterval)


freq_timeInterval <- data.frame(table(data$timeInterval))
freq_timeInterval
str(freq_timeInterval)

#################################
# TO INSERT IN MASTER



