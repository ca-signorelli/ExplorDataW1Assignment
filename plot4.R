##################################################################################################################
###########################  Exploratory Data Analysis week 1 assignment  ########################################
library(tidyverse)
library(lubridate)
library(gridExtra)
setwd("G:/Kostnadsanalyse og prising/R kurs/Coursera Data Science/exploratorydataw1assignment")

hpc <- read.table("household_power_consumption.txt",header=TRUE,sep=";")
head(hpc)
str(hpc)
hpcdata <- hpc %>% mutate(Date=dmy(Date),Time=parse_time(Time))         #  "%d/%m/%Y"
hpcdata <- hpcdata %>% filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>% 
  mutate(Global_active_power=as.numeric(Global_active_power), 
         Global_reactive_power=as.numeric(Global_reactive_power),Voltage=as.numeric(Voltage),
         Global_intensity=as.numeric(Global_intensity), Sub_metering_1=as.numeric(Sub_metering_1),Sub_metering_2=as.numeric(Sub_metering_2))

hpcdata <- hpcdata %>% mutate(weekday = weekdays(Date))

plot1 <- hpcdata %>%  ggplot(aes(x=Global_active_power)) + 
  geom_histogram(binwidth = 0.5, fill="red", colour="black", boundary=0) + 
  scale_y_continuous(breaks=seq(0,1200,200)) +
  labs(title="Global active power", x="Global Active Power (kilowat)",y="Frequency") 


plot2 <- hpcdata %>% ggplot(aes(x=order(Date,Time), y=Global_active_power)) + geom_line() + 
  scale_x_continuous(breaks=c(0,1441,2882), labels=c("Thursday","Friday","Saturday")) +
  labs(x="", y="Global Active Power (kilowatts)") 

hpcdata2 <- hpcdata %>% gather(Sub_metering_1, Sub_metering_2, Sub_metering_3, key="Meetering", value="Energy") %>% arrange(Date, Time)
plot3 <- hpcdata2 %>% ggplot(aes(x=order(Date,Time),y=Energy, colour=Meetering)) + geom_line() + 
  scale_x_continuous(breaks=c(0,4320), labels=c("Thursday","Friday")) +
  labs(x="", y="Energy/Sub meetering", colour="") + theme(legend.position=c(1,1), legend.justification=c(1,1)) + guides(colour=guide_legend(title=NULL))

plot412 <- hpcdata %>% ggplot(aes(x=order(Date,Time), y=Voltage)) + geom_line() + 
  scale_x_continuous(breaks=c(0,1441,2882), labels=c("Thursday","Friday","Saturday")) +
  labs(x="", y="Voltage")

plot422 <- hpcdata %>% ggplot(aes(x=order(Date,Time), y=Global_reactive_power)) + geom_line() + 
  scale_x_continuous(breaks=c(0,1441,2882), labels=c("Thursday","Friday","Saturday")) +
  labs(x="", y="Global Reactive Power (kilowatts)")

ggsave("Plot_4.png",arrangeGrob(plot2,plot412,plot3,plot422),height=4.80,width=4.80, dpi=100)
