plot3 <- function(){
        library(dplyr)
        library(lubridate)
        
        temp <- tempfile()
        
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
        
        data <- read.table(unz(temp, "household_power_consumption.txt"), 
                           sep=";", header=T, colClasses = "character")
        unlink(temp)
        
        tbl <- tbl_df(data)
        rm(data)
        
        tbl<-tbl %>%
                transmute(datetime=dmy_hms(paste(Date,Time)), 
                          Global_active_power = as.numeric(Global_active_power),
                          Global_reactive_power = as.numeric(Global_reactive_power),
                          Voltage = as.numeric(Voltage),
                          Sub_metering_1 = as.numeric(Sub_metering_1),
                          Sub_metering_2 = as.numeric(Sub_metering_2),
                          Sub_metering_3 = as.numeric(Sub_metering_3)
                ) %>%   
                filter(date(datetime) %in% ymd(c("2007-02-01","2007-02-02")))
        
        png(file="plot3.png")
                with(tbl, plot(datetime, Sub_metering_1, xlab="", 
                                ylab="Energy sub metering",type="n"))
                with(tbl, lines(datetime, Sub_metering_1))
                with(tbl, lines(datetime, Sub_metering_2, col="red"))
                with(tbl, lines(datetime, Sub_metering_3, col="blue"))
        dev.off()
}