library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)

fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
UCIHPCDataset <- "./data/exdata_data_household_power_consumption.zip"

if (!file.exists("data")) {
  dir.create("data")
}

if (!file.exists(UCIHPCDataset)){
  download.file(fileURL, destfile = UCIHPCDataset, mode = "wb")
  unzip(UCIHPCDataset, exdir = "./data")
  dateDownloaded <- date()
}

household_power_consumption <- read.table("./data/household_power_consumption.txt", header = TRUE, sep = ";", na.strings = c("?"), stringsAsFactors = FALSE)

household_power_consumption_df <- tbl_df(household_power_consumption)
rm(household_power_consumption)

household_power_consumption_df$Date <- as.Date(household_power_consumption_df$Date, format = "%d/%m/%Y")

household_power_consumption_ss <- household_power_consumption_df[household_power_consumption_df$Date == "2007-02-01" | household_power_consumption_df$Date == "2007-02-02", ]
rm(household_power_consumption_df)

household_power_consumption_ss$datetime <- ymd_hms(paste(household_power_consumption_ss$Date, household_power_consumption_ss$Time))

# household_power_consumption_ss$Global_active_power <- as.numeric(as.character(household_power_consumption_ss$Global_active_power))
# household_power_consumption_ss$Global_reactive_power <- as.numeric(as.character(household_power_consumption_ss$Global_reactive_power))
# household_power_consumption_ss$Voltage <- as.numeric(as.character(household_power_consumption_ss$Voltage))
# household_power_consumption_ss$Sub_metering_1 <- as.numeric(as.character(household_power_consumption_ss$Sub_metering_1))
# household_power_consumption_ss$Sub_metering_2 <- as.numeric(as.character(household_power_consumption_ss$Sub_metering_2))
# household_power_consumption_ss$Sub_metering_3 <- as.numeric(as.character(household_power_consumption_ss$Sub_metering_3))
household_power_consumption_ss[ , 3:9] <- apply(household_power_consumption_ss[ , 3:9], 2, function(x) as.numeric(as.character(x)))

plot4 <- function() {
  
  par(mfrow = c(2, 2))
  
  plot(household_power_consumption_ss$datetime, household_power_consumption_ss$Global_active_power,
       type = "l", 
       xlab = "",
       ylab = "Global Active Power (kilowatts)")
  
  plot(household_power_consumption_ss$datetime, household_power_consumption_ss$Voltage,
       type = "l", 
       xlab = names(household_power_consumption_ss[ , 10]),
       ylab = names(household_power_consumption_ss[ , 5]))
  
  plot(household_power_consumption_ss$datetime, household_power_consumption_ss$Sub_metering_1,
       type = "l", 
       xlab = "",
       ylab = "Energy sub metering")
  lines(household_power_consumption_ss$datetime, household_power_consumption_ss$Sub_metering_2, 
        col="red")
  lines(household_power_consumption_ss$datetime, household_power_consumption_ss$Sub_metering_3, 
        col="blue")
  legend("topright", 
         col = c("black","red","blue"), 
         names(household_power_consumption_ss[ , 7:9]),
         lty = c(1, 1), 
         lwd = c(1, 1),
         bty = "n",
         inset = c(0.01, 0.01))
  
  plot(household_power_consumption_ss$datetime, household_power_consumption_ss$Global_reactive_power,
       type = "l", 
       xlab = names(household_power_consumption_ss[ , 10]),
       ylab = names(household_power_consumption_ss[ , 4]))
  
  dev.copy(png, file = "plot4.png", width = 480, height = 480)
  
  dev.off()
  
}

plot4()