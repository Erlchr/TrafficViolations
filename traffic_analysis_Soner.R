library(dplyr)
library(lubridate)
library(chron)

data <- read.csv("/Users/sonerercelik/Desktop/E-Business/Traffic_Violations.csv", header=T)

########################### Start - Von Master eingefügt:

# Remove redundant location information
data = select(data, -Location, -Geolocation) 

# Remove columns which has always the same value like "Agency = MCP" in each row of data
data = select(data, -Agency, -Accident, -Commercial.Vehicle)

# Remove irrelevant/unnecessary columns for analysis
data = select(data, -Commercial.License, -Work.Zone, -State, -Model, -Color, -DL.State)
data = select(data, -Fatal, -HAZMAT, -Charge, -Article)

# Add a column "Local" to see if the driver is a local or a tourist
data$Local <- ifelse(data$Driver.State == "MD", TRUE, FALSE)
data = select(data, -Driver.State) # Remove Driver.State

############################# End

#------------- Start - Own Work -----------------#
# Add column with weekdays
data = mutate(data, weekDay = paste( wday(as.Date(data$Date.Of.Stop, format = "%m/%d/%Y"), label = TRUE)  ))

# column with converted time in seconds
convertedInSecTimeIntervall = minutes(times(data$Time.Of.Stop)) + hours(times(data$Time.Of.Stop)) * 60 * 60

# assign the seconds in intervall
cuts <- c(-Inf, 10800, 10800*2, 10800*3, 10800*4, 10800*5, 10800*6, 10800*7, Inf)
labs <- c("00:00 - 02:59", "03:00 - 05:59", "06:00 - 08:59", "09:00 - 11:59", "12:00 - 14:59", "15:00 - 17:59", "18:00 - 20:59", "21:00 - 23:59")
assignedInterval = labs[findInterval(convertedInSecTimeIntervall, cuts)]

# Add column time intervall 
data = mutate(data, TimeInterval = paste (assignedInterval))


#------------- Bar Plots - Start ---------------------#
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

# Create plot how often a traffic violation occurs depending on model year of the car
dataFiltered = filter(data, Year < 2017)
dataFiltered = filter(dataFiltered, Year > 1995)

barplot(table(dataFiltered$Year),col="slategray2",border="black",
        xlab="Year of the car",
        ylab="Frequency of Traffic Violations",
        ylim=c(0,10000)
)

table(data$Belts)
table(data$Personal.Injury)
table(data$Contributed.To.Accident)

# Gurtverstoß und Körperverletzung und daraus resultierender Unfall -> 183 times
new_data <- filter(data, Belts == 'Yes', Personal.Injury=='Yes', Contributed.To.Accident=='Yes') 

# Gurtverstoß und keine Körperverletzung und daraus kein resultierender Unfall -> 4335 times
new_data <- filter(data, Belts == 'Yes', Personal.Injury=='No', Contributed.To.Accident=='No') 

# Gurtverstoß und keine Körperverletzung und daraus resultierender Unfall -> 248 times
new_data <- filter(data, Belts == 'Yes', Personal.Injury=='No', Contributed.To.Accident=='Yes') 

# Kein Gurtverstoß und Körperverletzung und daraus resultierender Unfall -> 816 times
new_data <- filter(data, Belts == 'No', Personal.Injury=='Yes', Contributed.To.Accident=='Yes') 

# Kein Gurtverstoß und keine Körperverletzung aber daraus resultierender Unfall -> 2498 times
new_data <- filter(data, Belts == 'No', Personal.Injury=='No', Contributed.To.Accident=='Yes' )

# Gurtverstoß und Körperverletzung und kein Unfall -> 192 times
new_data <- filter(data, Belts == 'Yes', Personal.Injury=='Yes', Contributed.To.Accident=='No')

# Gurtverstoß und Körperverletzung -> 375 times
new_data <- filter(data, Belts == 'Yes', Personal.Injury=='Yes')

# Gurtverstoß und keine Körperverletzung -> 4583 times
new_data <- filter(data, Belts == 'Yes', Personal.Injury=='No') 

# kein Gurtverstoß und keine Körperverletzung -> 134548 times
new_data <- filter(data, Belts == 'No', Personal.Injury =='No')

# kein Gurtverstoß und Körperverletzung -> 1461 times
new_data <- filter(data, Belts == 'No', Personal.Injury =='Yes') #


# Summary:
# Kein Gurtverstoß, Unfall, keine Körperverletzung  -> 2498 times
# Kein Gurtverstoß, Unfall, Körperverletzung        ->  816 times
# Gurtverstoß, Unfall, keine Körperverletzung       ->  248 times
# Gurtverstoß, Unfall, Körperverletzung             ->  183 times
# Gurtverstoß, kein Unfall, keine Körperverletzung  -> 4335 times
# Gurtverstoß, kein Unfall, Körperverletzung        ->  192 times

# Gurtverstoß und Körperverletzung                  ->  375 times = T-TEST
# kein Gurtverstoß und Körperverletzung             -> 1461 times

# Gurtverstoß und keine Körperverletzung            ->   4583 times
# kein Gurtverstoß und keine Körperverletzung       -> 134548 times

# Add column 'Citation' if the car driver get a citation: 
data = mutate(data, Citation = paste (ifelse(Violation.Type=='Citation', TRUE, FALSE)))

data = filter(data, Year < 2017)
data = filter(data, Year > 1930)
mean(data$Year)

# Add column 'OldCar': <= 2007
data = mutate(data, OldCar = paste (ifelse(Year <= 2007, TRUE, FALSE)))

#------------- End - Own Work -----------------#

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



