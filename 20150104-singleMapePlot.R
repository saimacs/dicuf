# plot historical MAPE values for each <building,strategy> pair 

#rm(list = ls())  
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)

vectorLength = 199
beginDR = 54 # 1:15 PM
endDR = 69 # 5:00 PM
DRst = c("GTR", "VFD", "DUTY", "GTR & VFD",
         "GTR & DUTY", "VFD & DUTY", "GTR & VFD & DUTY")

# read names of buildings with DR events
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/")
schedule = read.csv("DR-Schedule-all.csv",header=TRUE,as.is=TRUE)
buildings = unique(schedule$Building)
numBuildings = length(buildings)

# read DR event data for each building
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/DRdataset")
eventDates = list()
for(i in 1:numBuildings){
  bldng = buildings[i]
  DRdays = list.files(pattern=paste(bldng,"-*",sep=""))
  DRdays = substr(DRdays,5,14)
  eventDates[[length(eventDates)+1]] = as.Date(DRdays)
}

# determine number of DR event days for each building
countDRdays = numeric(length(eventDates))
for (i in 1:length(eventDates)){
  countDRdays[i] =  length(eventDates[[i]]) 
}
missingBuildings = which(countDRdays == 0)

#---------------------
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/")
# read data
data.dicuf = read.csv("Aligned/DiCuF-Wtd-TimeWise-MA-aligned.csv",header=TRUE,as.is=TRUE)
data.avon = read.csv("Aligned/AvoN-Wtd-TimeWise-MA-aligned.csv",header=TRUE,as.is=TRUE)
data.obs = read.csv("Aligned/obs-Wtd-TimeWise-MA-aligned.csv",header=TRUE,as.is=TRUE)  

# errors
nzRows = apply(data.obs[,4:19], 1, function(x) any(x ==0 ))
indices = which(nzRows == 0)  

# calculate mape
ape = abs(data.dicuf[indices,6:21] - data.obs[indices,4:19])/data.obs[indices,4:19]
mape = apply(ape,1,mean)  
myDF = data.frame(Date = as.Date(data.dicuf$Date[indices], "%m/%d/%y"),
                  Building = data.dicuf$Building[indices],
                  Strategy = data.dicuf$Strategy[indices],
                  MAPE = mape)

# save event-wise mape info
write.csv(myDF,"../gcode/dicuf/eventwiseMape.csv",row.names=FALSE)

#------------------------
# 1. plot event-wise (individually for buildings)
p1 = ggplot(myDF,aes(Date,MAPE)) +  
  geom_point() +
  labs(y = "MAPE")

p2 = p1 + stat_smooth(method="lm") +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0.1,1.0,0.1)) +
  scale_x_date(breaks = "1 month", minor_breaks = "1 week", 
               labels=date_format("%m-%Y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = rel(1.3), colour = "black"),
        axis.title = element_text(size = rel(1.3), colour = "black"))

nMonths = month(as.Date(data.dicuf$Date[indices], "%m/%d/%y"))
if(length(unique(nMonths))<2){
  p2 = p2 + scale_x_date(labels=date_format("%d-%m-%Y"))        
}

#------------------------
# 2. plot event-wise average 
eventsList = unique(myDF$Date)
numEvents = length(eventsList)
avgMape = numeric(numEvents)
for (i in 1:numEvents){
  data.slice = myDF[which(myDF$Date == eventsList[i]),]
  avgMape[i] = mean(data.slice$MAPE) 
}
myDF2 = data.frame(Date = eventsList, MAPE = avgMape)
  
p1 = ggplot(myDF2,aes(Date,MAPE)) +  
  geom_point() +
  labs(y = "MAPE")

p2 = p1 + stat_smooth(method="lm") +
  scale_y_continuous(limits = c(0,0.7),
                     breaks = seq(0.1,0.7,0.1)) +
  scale_x_date(breaks = "1 month", minor_breaks = "1 week", 
               labels=date_format("%m-%Y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = rel(1.3), colour = "black"),
        axis.title = element_text(size = rel(1.3), colour = "black"))

nMonths = month(as.Date(data.dicuf$Date[indices], "%m/%d/%y"))
if(length(unique(nMonths))<2){
  p2 = p2 + scale_x_date(labels=date_format("%d-%m-%Y"))        
}

#------------------------
# 3. plot colored for event-strategy

p1 = ggplot(myDF,aes(Date,MAPE,color = Building)) +  
  geom_point() +
  labs(y = "MAPE")

p2 = p1 + 
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0.1,1.0,0.1)) +
  scale_x_date(breaks = "1 month", minor_breaks = "1 week", 
               labels=date_format("%m-%Y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = rel(1.3), colour = "black"),
        axis.title = element_text(size = rel(1.3), colour = "black")) + 
  guides(fill=guide_legend(ncol=2))

nMonths = month(as.Date(data.dicuf$Date[indices], "%m/%d/%y"))
if(length(unique(nMonths))<2){
  p2 = p2 + scale_x_date(labels=date_format("%d-%m-%Y"))        
}
