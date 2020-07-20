## Read Data ###
household <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

## Check Data## 
summary(household)

## change date format##
household$Date <- as.Date(household$Date, "%d/%m/%Y")

## Subset date between the given dates ##
household <- subset(household,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## Data clean up Remove missing variables ##
household <- household[complete.cases(household),]
complete.cases(household)

## Sturcture and format Date and time ##
dateTime <- paste(household$Date, household$Time)
dateTime <- setNames(dateTime, "DateTime")
household <- household[ ,!(names(household) %in% c("Date","Time"))]
household <- cbind(dateTime, household)
household$dateTime <- as.POSIXct(dateTime)

## Check Datat##
summary(household)

## 1st plot ##
hist(household$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
dev.copy(png,"plot1.png", width=480, height=480)

## 2nd plot ##
plot(household$Global_active_power~household$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
dev.copy(png,"plot2.png", width=480, height=480)

## 3rd plot##
with(household, {
       plot(Sub_metering_1~dateTime, type="l",
                      ylab="Global Active Power (kilowatts)", xlab="")
      lines(Sub_metering_2~dateTime,col='Red')
     lines(Sub_metering_3~dateTime,col='Blue')  })
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
                c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.copy(png, file="plot3.png", height=480, width=480)

## Plot 4 ## 
with(household, {
      plot(Global_active_power~dateTime, type="l", 
                      ylab="Global Active Power (kilowatts)", xlab="")
      plot(Voltage~dateTime, type="l", 
                      ylab="Voltage (volt)", xlab="")
       plot(Sub_metering_1~dateTime, type="l", 
                       ylab="Global Active Power (kilowatts)", xlab="")
     lines(Sub_metering_2~dateTime,col='Red')
      lines(Sub_metering_3~dateTime,col='Blue')
      legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
                          legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
      plot(Global_reactive_power~dateTime, type="l", 
                      ylab="Global Rective Power (kilowatts)",xlab="")
   })
