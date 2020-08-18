##################################################################################################################
###########################  Exploratory Data Analysis week 1 assignment  ########################################
library(tidyverse)
library(lubridate)
setwd("G:/Kostnadsanalyse og prising/R kurs/Coursera Data Science/exploratorydataw1assignment")

hpc <- read.table("household_power_consumption.txt",header=TRUE,sep=";")
head(hpc)
str(hpc)
hpcdata <- hpc %>% mutate(Date=dmy(Date),Time=parse_time(Time))         
hpcdata <- hpcdata %>% filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>% mutate(Global_active_power=as.numeric(Global_active_power),
                                                                                      Global_reactive_power=as.numeric(Global_reactive_power),Voltage=as.numeric(Voltage),
                                                                                      Global_intensity=as.numeric(Global_intensity), Sub_metering_1=as.numeric(Sub_metering_1),Sub_metering_2=as.numeric(Sub_metering_2))

hpcdata <- hpcdata %>% mutate(weekday = weekdays(Date))

plot1 <- hpcdata %>%  ggplot(aes(x=Global_active_power)) + 
  geom_histogram(binwidth = 0.5, fill="red", colour="black", boundary=0) + 
  scale_y_continuous(breaks=seq(0,1200,200)) +
  labs(title="Global active power", x="Global Active Power (kilowat)",y="Frequency") 


png("Plot_1.png", width=480, height=480)
plot1
dev.off()
