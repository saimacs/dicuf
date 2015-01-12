# read event data 
setwd("/Users/saima/Desktop/Energy Experiments/gcode/")
eventData = read.csv("dicuf/eventwiseMape.csv")
buildings = unique(eventData$Building)
numBuildings = length(buildings)

stdev = numeric(numBuildings)
avgMape = numeric(numBuildings)
for (i in 1:numBuildings){
  indices = which(eventData$Building == buildings[i])
  data.slice = eventData[indices,]
  stdev[i] = sd(data.slice$MAPE)
  avgMape[i] = mean(data.slice$MAPE)
}

#--------
#sorted by std dev
stdevSorted = sort(stdev,index.return = TRUE)
sortedIndices = stdevSorted$ix 
sortedBuilding = buildings[sortedIndices]

# SAL HAR SCX THH DRB BHE JKP LAW ASC TCC OHE JHH BRI KAP MHP
# SLH GFS WPH BKS SCB LVL STU EEB LRC ALM VKC SOS SWC HOH HSH
# MRF WAH SCE

#--------
#sorted by low mape
mapeSorted = sort(avgMape,index.return = TRUE)
sortedIndices = mapeSorted$ix 
sortedBuilding = buildings[sortedIndices]

# SAL HAR LAW TCC THH WPH ASC OHE MHP JKP DRB BHE JHH EEB SOS
# SCB KAP SLH GFS SWC BRI STU LVL BKS SCX HOH VKC ALM LRC MRF
# WAH HSH SCE
