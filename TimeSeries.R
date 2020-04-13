#Load packages
library(dplyr)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(scales)
library(cowplot)
#Load data
cuyama<-read.csv("Cuyama.csv")

#Format data - keep columns shrub ID, Volume, microsite, treatment, soil moisture, temperature, latitude and longitude, month, day, hour
str(cuyama)#Will also need to convert date formate and calculate volume
cuyama<-cuyama %>% mutate(Volume=(2/3)*pi*(x/2)*(y/2)*z,
                         Date=as.Date(paste(year, month, days,sep="-"), "%Y-%m-%d"),
                         Month=month.abb[month]) %>% select(shrub.ID,Volume,
                                                           lat,long,measure,
                                                           microsite,
                                                           treatment,sensor,
                                                           Date, hour,Month,site)
TempDat<-cuyama %>% filter(sensor%in%'temp') %>% rename(Temp=measure)
MoistDat<-cuyama %>% filter(sensor%in%'soil moisture') %>% rename(Moisture=measure)
cuyama<-full_join(TempDat,MoistDat) %>% select(-sensor)

#Timeseries with every data point
MonthMicrositeTemp <- ggplot(cuyama,aes(x=Date, y= Temp, color=microsite)) +
  geom_point(na.rm=TRUE, size=1,alpha=I(0.2)) +
  #(aes(colour=microsite))+
  xlab("Date") + ylab("Air Temperature (C)")+
  scale_colour_manual(values=c("steelblue","lightgreen","navy","forestgreen"))+
  scale_x_date(labels=date_format("%b %y"))+
  theme_classic()
MonthMicrositeTemp
MonthMicrositeMoist <- ggplot(cuyama,aes(x=Date, y= Moisture, color=microsite)) +
  geom_point(na.rm=TRUE, size=1,alpha=I(0.2)) +
  xlab("Date") + ylab("Moisture")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  scale_x_date(labels=date_format("%b %y"))+
  theme_classic()
MonthMicrositeMoist
MonthTreatTemp <- ggplot(cuyama,aes(x=Date, y= Temp, color=treatment)) +
  geom_point(na.rm=TRUE, size=1,alpha=I(0.2)) +
  xlab("Date") + ylab("Air Temperature (C)")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  scale_x_date(labels=date_format("%b %y"))+
  theme_classic()
MonthTreatTemp
MonthTreatMoist <- ggplot(cuyama,aes(x=Date, y= Moisture, color=treatment)) +
  geom_point(na.rm=TRUE, size=1,alpha=I(0.2)) +
  xlab("Month") + ylab("Moisture)")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  scale_x_date(labels=date_format("%b %y"))+
  theme_classic()
MonthTreatMoist
plot_grid(MonthMicrositeTemp, MonthMicrositeTemp,MonthTreatTemp, MonthTreatMoist,ncol=2,nrow=2, align=c("h"))
#Monthly averages for treatments
AveMonthTreat<-cuyama %>% group_by(Month,treatment) %>% summarize(TempMean=mean(na.omit(Temp)),
                                                                           MoistMean=mean(na.omit(Moisture)))
AveMonthTreat$Month<-factor(AveMonthTreat$Month,
                           levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
AveMonthTreatTemp <- ggplot(AveMonthTreat,aes(x=Month, y=TempMean, color=treatment)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Month") + ylab("Air Temperature (C)")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveMonthTreatTemp
AveMonthTreatMoisture <- ggplot(AveMonthTreat,aes(x=Month, y=MoistMean, color=treatment)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Month") + ylab("Soil Moisture")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveMonthTreatMoisture
plot_grid(AveMonthTreatTemp, AveMonthTreatMoisture,ncol=1,nrow=2, align=c("h"))


#Monthly averages for microsite
AveMonthMicro<-cuyama %>% group_by(Month,microsite) %>% summarize(TempMean=mean(na.omit(Temp)),
                                                                  MoistMean=mean(na.omit(Moisture)))
AveMonthMicro$Month<-factor(AveMonthMicro$Month,
                            levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
AveMonthMicroTemp <- ggplot(AveMonthMicro,aes(x=Month, y=TempMean, color=microsite)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Month") + ylab("Air Temperature (C)")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveMonthMicroTemp
AveMonthMicroMoisture <- ggplot(AveMonthMicro,aes(x=Month, y=MoistMean, color=microsite)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Month") + ylab("Soil Moisture")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveMonthMicroMoisture
plot_grid(AveMonthMicroTemp, AveMonthMicroMoisture,ncol=1,nrow=2, align=c("h"))

#Hourly averages for treatments
AveHourTreat<-cuyama %>% group_by(hour,treatment) %>% summarize(TempMean=mean(na.omit(Temp)),
                                                                  MoistMean=mean(na.omit(Moisture)))
AveHourTreat$hour<-as.factor(AveHourTreat$hour)
AveHourTreatTemp <- ggplot(AveHourTreat,aes(x=hour, y=TempMean, color=treatment)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Time (hour)") + ylab("Air Temperature (C)")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveHourTreatTemp
AveHourTreatMoisture <- ggplot(AveHourTreat,aes(x=hour, y=MoistMean, color=treatment)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Time (hour)") + ylab("Soil Moisture")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveHourTreatMoisture
plot_grid(AveHourTreatTemp, AveHourTreatMoisture,ncol=1,nrow=2, align=c("h"))

#Monthly averages for microsite
AveHourMicro<-cuyama %>% group_by(hour,microsite) %>% summarize(TempMean=mean(na.omit(Temp)),
                                                                  MoistMean=mean(na.omit(Moisture)))
AveHourMicroTemp <- ggplot(AveHourMicro,aes(x=hour, y=TempMean, color=microsite)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Time (hours)") + ylab("Air Temperature (C)")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveHourMicroTemp
AveHourMicroMoisture <- ggplot(AveHourMicro,aes(x=hour, y=MoistMean, color=microsite)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Time (hour)") + ylab("Soil Moisture")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveHourMicroMoisture
plot_grid(AveHourMicroTemp, AveHourMicroMoisture,ncol=1,nrow=2, align=c("h"))

#Timeseries with daily averages
AveDateData<-cuyama %>% group_by(Date,microsite,treatment) %>%summarize(TempMean=mean(na.omit(Temp)),
                                                                MoistMean=mean(na.omit(Moisture)))

AveDateTreatTemp <- ggplot(AveDateData,aes(x=Date, y=TempMean, color=treatment)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Air Temperature (C)")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveDateTreatTemp
AveDateTreatMoisture <- ggplot(AveDateData,aes(x=Date, y=MoistMean, color=treatment)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Soil Moisture")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveDateTreatMoisture
AveDateMicroTemp <- ggplot(AveDateData,aes(x=Date, y=TempMean, color=microsite)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Air Temperature (C)")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveDateMicroTemp
AveDateMicroMoisture <- ggplot(AveDateData,aes(x=Date, y=MoistMean, color=microsite)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Soil Moisture")+
  scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
AveDateMicroMoisture

#By site for curiosity

SiteDateData<-cuyama %>% group_by(Date,site,microsite,treatment) %>%summarize(TempMean=mean(na.omit(Temp)),
                                                                        MoistMean=mean(na.omit(Moisture)))
SiteDateTreatTemp <- ggplot(SiteDateData,aes(x=Date, y=TempMean, color=site)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Air Temperature (C)")+
  #scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
SiteDateTreatTemp
SiteDateTreatMoisture <- ggplot(SiteDateData,aes(x=Date, y=MoistMean, color=site)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Soil Moisture")+
  #scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
SiteDateTreatMoisture
SiteDateMicroTemp <- ggplot(SiteDateData,aes(x=Date, y=TempMean, color=site)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Air Temperature (C)")+
  #scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
SiteDateMicroTemp
SiteDateMicroMoisture <- ggplot(SiteDateData,aes(x=Date, y=MoistMean, color=as.factor(site))) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Soil Moisture")+
  #scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
SiteDateMicroMoisture

#Without site 6...and 5
SiteDateData<-cuyama %>% group_by(Date,site,microsite,treatment) %>%summarize(TempMean=mean(na.omit(Temp)),
                                                                              MoistMean=mean(na.omit(Moisture))) %>% filter(!site%in%6,!site%in%5)
SiteDateTreatTemp <- ggplot(SiteDateData,aes(x=Date, y=TempMean, color=site)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Air Temperature (C)")+
  #scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
SiteDateTreatTemp
SiteDateTreatMoisture <- ggplot(SiteDateData,aes(x=Date, y=MoistMean, color=site)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Soil Moisture")+
  #scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
SiteDateTreatMoisture
SiteDateMicroTemp <- ggplot(SiteDateData,aes(x=Date, y=TempMean, color=site)) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Air Temperature (C)")+
  #scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
SiteDateMicroTemp
SiteDateMicroMoisture <- ggplot(SiteDateData,aes(x=Date, y=MoistMean, color=as.factor(site))) +
  geom_point(na.rm=TRUE, size=2) +
  xlab("Date") + ylab("Soil Moisture")+
  #scale_colour_manual(values=c("steelblue","lightgreen"))+
  theme_classic()
SiteDateMicroMoisture


#Add SE bars or CI shading to ave plots