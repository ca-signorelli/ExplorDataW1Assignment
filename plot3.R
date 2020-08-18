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

hpcdata2 <- hpcdata %>% gather(Sub_metering_1, Sub_metering_2, Sub_metering_3, key="Meetering", value="Energy") %>% arrange(Date, Time)
plot3 <- hpcdata2 %>% ggplot(aes(x=order(Date,Time),y=Energy, colour=Meetering)) + geom_line() + scale_x_continuous(breaks=c(0,4320), labels=c("Thursday","Friday")) +
  labs(x="", y="Energy/Sub meetering", colour="") + theme(legend.position=c(1,1), legend.justification=c(1,1)) + guides(colour=guide_legend(title=NULL))

png("Plot_3.png", width=480, height=480)
plot3
dev.off()
