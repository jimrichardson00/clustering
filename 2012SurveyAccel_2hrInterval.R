# Analyze accelerometer data from 2012 sablefish survey

rm(list=ls())

setwd("/Users/beaudoherty/Documents/SFU/REM 699/sablefish data/DohertyCoxData")

x1= read.csv("Accelerometer_Survey_2012.csv", sep=",")	#Read-in csv
y1= read.csv("Sensor_Fishing_Events.csv", sep=",")

uniqueSets = unique(x1$Set)
vals = vector ('numeric')

y2 = subset(y1, YEAR==2012)

for(i in min(uniqueSets):max(uniqueSets))	
{
	
if (i %in% uniqueSets)
{
	x2 = subset(x1,x1$Set==i)
	uniqueTraps = unique(x2$Trap)
	y3 = subset(y2,y2$SETNO==i)
	slope = y3$BEGIN_DEPTH - y3$END_DEPTH
		
	for(n in min(uniqueTraps): max(uniqueTraps))
	{
		x3 = subset(x2,x2$Trap==n)
		
	if (n %in% uniqueTraps)
	{
		
		x3$AccVectorSum	= (sqrt((x3$XAccel^2)+(x3$YAccel^2)+(x3$ZAccel^2)))
		
		# Separate into 3 time periods (1-deployment, 2-soak, 3-retrieval)
		
		x3rows = nrow(x3)
		
		x3.1 = x3[1:1440,]
		x3.2 = x3[1441:(x3rows-1440),]
		x3.3 = x3[(x3rows-1439):x3rows,]
		
		ac.var1	 = var(x3.1$AccVectorSum)	
		ac.mean1 = mean(x3.1$AccVectorSum)
		ac.var2	 = var(x3.2$AccVectorSum)	
		ac.mean2 = mean(x3.2$AccVectorSum)
		ac.var3	 = var(x3.3$AccVectorSum)	
		ac.mean3 = mean(x3.3$AccVectorSum)
		
		x3.2rows = nrow(x3.2)
	
		vals1 = c(i, n, 1, ac.mean1, ac.var1, y3$MEAN_DEPTH, slope, y3$TRAPS_FISHED)
		vals2 = c(i, n, 2, ac.mean2, ac.var2, y3$MEAN_DEPTH, slope, y3$TRAPS_FISHED)
		vals3 = c(i, n, 3, ac.mean3, ac.var3, y3$MEAN_DEPTH, slope, y3$TRAPS_FISHED)
	
		# vals to record stat and end times - leads to atomic vector problems
	
		#vals1 = c(i, n, x3.1$DateTime[1], x3.1$DateTime[1440], 1, ac.mean1, ac.var1)
		#vals2 = c(i, n, x3.2$DateTime[1], x3.2$DateTime[x3.2rows], 2, ac.mean2, ac.var2)
		#vals3 = c(i, n, x3.3$DateTime[1], x3.3$DateTime[1440], 3, ac.mean3, ac.var3)
		
		vals  = rbind (vals,vals1,vals2,vals3)
			
	}
	}
}
}


colnames(vals)= c("Set","Trap","Period","AccMean","AccVar", "MeanSetDepth", "SetSlope", "#Traps")




write.csv(vals, file="2012Survey_Processed.csv")	