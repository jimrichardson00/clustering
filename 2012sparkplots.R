# Processing 2012 Sablefish survey data for exploratory analysis	#
# Author: B. Doherty                                          		#
# Date Revised:                                                     #
# Aug. 26,2014-06													#
#-------------------------------------------------------------------#

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
		
		Events  = nrow(x3)
		ac.mean = mean(x3$AccVectorSum)
		ac.var	= var(x3$AccVectorSum)	
		ac.max  = max(x3$AccVectorSum)
		ac.min  = min(x3$AccVectorSum)
		T1		= sum(x3$AccVectorSum >= .9*ac.max) # of Events within 10% of ac.max
		T2		= sum(x3$AccVectorSum >= .75*ac.max) # of Events within 25% of ac.max
	
		vals1 = c(i, n, ac.mean, ac.var, ac.max, ac.min, T1,T2, Events, y3$MEAN_DEPTH, slope)
	
		# vals to record start and end times - leads to atomic vector problems
		vals  = rbind (vals, vals1)		
	}
	}
}
}

#colnames(vals)= c("Set","Trap","AccMean","AccVar", "ac.max", "ac.min", "T1", "T2", "Events", "MeanDepth", "SetSlope")

write.csv(vals, file="2012Survey_Sparkplots.csv")	