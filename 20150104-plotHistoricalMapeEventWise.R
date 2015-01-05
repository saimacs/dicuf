# plot historical MAPE values for each <building,strategy> pair 

rm(list = ls())  
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
eventDays = unique(schedule$EventDate)
numEventDays = length(eventDays)
  
# read DR event data for each day
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/DRdataset")
eventBuildings = list()
for(i in 1:numEventDays){
  event = as.Date(eventDays[i],format="%m/%d/%y")
  DRbuildings = substr(list.files(pattern=paste("*-",event,"-*",sep="")),1,3)
  eventBuildings[[length(eventBuildings)+1]] = DRbuildings
}

# determine number of buildings for each DR event day
countDRbuildings = numeric(length(eventBuildings))
for (i in 1:length(eventBuildings)){
  countDRbuildings[i] =  length(eventBuildings[[i]])
}
missingDays = which(countDRbuildings == 0)

#---------------------
setwd("/Users/saima/Desktop/Energy Experiments/2014 New Curtailment/")
# read data
data.dicuf = read.csv("Aligned/DiCuF-Wtd-TimeWise-MA-aligned.csv",header=TRUE,as.is=TRUE)
data.avon = read.csv("Aligned/AvoN-Wtd-TimeWise-MA-aligned.csv",header=TRUE,as.is=TRUE)
data.obs = read.csv("Aligned/obs-Wtd-TimeWise-MA-aligned.csv",header=TRUE,as.is=TRUE)  

#dicuf: error per <day, building>
days = unique(data.dicuf$Date)
for (i in 1:length(days)){
    
    indices = which(data.dicuf$Date == days[i])
    data.dicuf.slice = data.dicuf[indices,]
    data.obs.slice = data.obs[indices,]
    buildings = unique(data.dicuf.slice$Building)
    nzRows = apply(data.obs.slice[,4:19], 1, function(x) any(x ==0 ))    
    new.indices2 = which(nzRows == 0)  
    
    # include only if data is non-empty
    if(length(new.indices2)!=0){
      ape = abs(data.dicuf.slice[new.indices2,6:21] - data.obs.slice[new.indices2,4:19])/data.obs.slice[new.indices2,4:19]
      mape = apply(ape,1,mean)
      
      # make dataframe for plots
      myDF = data.frame(MAPE = mape,
                        Building = data.dicuf.slice$Building[new.indices2])
      p1 = ggplot(myDF,aes(Building,MAPE)) +  
          geom_bar(stat="identity",width=0.5) +
          ggtitle(as.Date(data.dicuf.slice$Date,"%m/%d/%y")) +
          labs(y = "MAPE") +
          #theme(axis.text.x = element_text(angle = 45, hjust = 1),
          theme(axis.text = element_text(size = rel(1.3), colour = "black"),
              axis.title = element_text(size = rel(1.3), colour = "black"))
      p1
    }
    readline("press return to continue")
      
} # done for each DR event day



