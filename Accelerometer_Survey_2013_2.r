# install.packages("plyr")
# install.packages("sqldf")
# install.packages("tcltk")
# install.packages("lubridate")
# install.packages("beepr")
# install.packages("doParallel")
# install.packages("beepr")
# install.packages("stats")
# install.packages("signal")
# install.packages('segmented')

library(plyr)
library(tcltk)
library(sqldf)
library(lubridate)
library(beepr)
library(doParallel)
library(beepr)
library(stats)
library(signal)
library(segmented)

setwd("/home/jim/Dropbox/REM/tasks/accelerometer/DFO data request - August, 2014")

# read in data
Accelerometer_Survey_2013 <- read.csv("Accelerometer_Survey_2013.csv")
Accelerometer_Survey_2013$DateTime <- as.POSIXct(Accelerometer_Survey_2013$DateTime,format="%Y-%m-%d %H:%M:%S")
Accelerometer_Survey_2013$Sets <- Accelerometer_Survey_2013$Set
Accelerometer_Survey_2013$Traps <- Accelerometer_Survey_2013$Trap
Accelerometer_Survey_2013 <- Accelerometer_Survey_2013[order(Accelerometer_Survey_2013$Sets,Accelerometer_Survey_2013$Traps),]

# VecSum
Accelerometer_Survey_2013$VecSum <- sqrt(Accelerometer_Survey_2013$XAccel^2 + Accelerometer_Survey_2013$YAccel^2 + Accelerometer_Survey_2013$ZAccel^2)

# Set year of survey
year <- min(year(Accelerometer_Survey_2013$DateTime))

# sets and Traps sets
Sets <- unique(Accelerometer_Survey_2013$Sets)
Traps <- unique(Accelerometer_Survey_2013$Traps)

# grid of Set & Trap combinations
grid_ST <- unique(data.frame(Sets=Accelerometer_Survey_2013$Sets,Traps=Accelerometer_Survey_2013$Traps))
grid_ST <- grid_ST[order(grid_ST$Sets,grid_ST$Traps),]
head(grid_ST)

# define function time difference
time_diff <- function(DateTime){as.numeric(max(DateTime) - min(DateTime))*24}

# summarize Set and Trap by length of time
d <- ddply(Accelerometer_Survey_2013,.(Set,Trap),summarize,time_diff=time_diff(DateTime),length=length(DateTime)*5/(60*60))
head(d)

# read in sensor fishing events data, (used for start and end time for sets)
Sensor_Fishing_Events <- read.csv("Sensor_Fishing_Events.csv")
Sensor_Fishing_Events <- Sensor_Fishing_Events[Sensor_Fishing_Events$YEAR == year,]
Sensor_Fishing_Events <- Sensor_Fishing_Events[order(Sensor_Fishing_Events$SETNO),]

# join two data sets
data <- sqldf("SELECT 
				Accelerometer_Survey_2013.*
				,Sensor_Fishing_Events.*
				FROM Accelerometer_Survey_2013
				LEFT JOIN Sensor_Fishing_Events
				ON Accelerometer_Survey_2013.Sets = Sensor_Fishing_Events.SETNO
				;")

# start time and end time
data$Start_DateTime <- as.POSIXct(paste(data$YEAR,"-",data$MON,"-",data$DAY," ",data$TIME,":00",sep=""),format="%Y-%m-%d %H:%M:%S")
data$End_DateTime <-data$Start_DateTime + data$DURATION*60

# subset data based on start time and end time
data <- data[data$DateTime >= data$Start_DateTime & data$DateTime <= data$End_DateTime,]

# subset column names
data <- data[,c(names(Accelerometer_Survey_2013),"Start_DateTime","End_DateTime")]

# order by sets and Traps, datetime
data <- data[order(data$Sets,data$Traps,data$DateTime),]

# comment this out
grid_ST <- grid_ST[seq(1,10,1),]
data <- data[data$Sets %in% grid_ST$Sets & data$Traps %in% grid_ST$Traps,]

deriv <- function(x){
	dx <- x[seq(2,length(x),1)] - x[seq(1,length(x)-1,1)]
	dx <- c(dx[1],dx)
}

cost <- function(data,DateTime1,DateTime2){
	if(DateTime1 >= DateTime2 | DateTime1 <= 2 | DateTime2 >= nrow(data) - 2) {
		cost <- 10000000000000000000000000000000000000000000
	} else {

		smth <- smooth.spline(seq(1,nrow(data),1),data$VecSum,spar=10^(-50))
		data$VecSum_l <- predict(smth,seq(1,nrow(data),1),type='response')$y

		data$Cumu_diff <- cumsum((data$VecSum - data$VecSum_l)^2)

		# splits data into three piecies based on dates
		data1 <- data[seq(1,DateTime1,1),]
		data2 <- data[seq(DateTime1,DateTime2,1),]
		data3 <- data[seq(DateTime2,nrow(data),1),]

		data1$Time <- seq(1,nrow(data1),1) 
		data2$Time <- seq(1,nrow(data2),1) 
		data3$Time <- seq(1,nrow(data3),1) 

		lm1 <- lm(Cumu_diff ~ Time,data=data1)
		data1$Cumu_diff_l <- predict(lm1,type='response')

		lm2 <- lm(Cumu_diff ~ Time,data=data2)
		data2$Cumu_diff_l <- predict(lm2,type='response')

		lm3 <- lm(Cumu_diff ~ Time,data=data3)
		data3$Cumu_diff_l <- predict(lm3,type='response')

		# defines cost of each segment as mean difference from the median
		cost1 <- sum((data1$Cumu_diff - data1$Cumu_diff_l)^2)
		cost2 <- sum((data2$Cumu_diff - data2$Cumu_diff_l)^2)
		cost3 <- sum((data3$Cumu_diff - data3$Cumu_diff_l)^2)

		cost <-  cost1 + cost2 + cost3

	}
	return(cost)
}

datetime12 <- function(Set,Trap){

	data_st <- data[data$Sets == Set & data$Traps == Trap,]

	data_st$Time <- seq(1,nrow(data_st),1)

	smth <- smooth.spline(seq(1,nrow(data_st),1),data_st$VecSum,spar=10^(-50))
	data_st$VecSum_l <- predict(smth,seq(1,nrow(data_st),1),type='response')$y

	data_st$Cumu_diff <- cumsum((data_st$VecSum - data_st$VecSum_l)^2)

	# 1
	data_st1 <- data_st[seq(1,floor(nrow(data_st)/2),1),]

	lm1 <- lm(Cumu_diff ~ Time,data=data_st1)
	segmented1 <- segmented(lm1, seg.Z = ~ Time, psi=c(data_st1$Time[floor(nrow(data_st1)/2)]))
	dt1 <- round(segmented1[["psi"]][2],0)

	# 2
	data_st2 <- data_st[seq(ceiling(nrow(data_st)/2),nrow(data_st),1),]

	lm2 <- lm(Cumu_diff ~ Time,data=data_st2)
	segmented2 <- segmented(lm2, seg.Z = ~ Time, psi=c(data_st2$Time[floor(nrow(data_st2)/2)]))
	dt2 <- round(segmented2[["psi"]][2],0)

	DateTime1 <- as.character(data_st$DateTime[dt1])
	DateTime2 <- as.character(data_st$DateTime[dt2])

	return(c(DateTime1,DateTime2))
}

# ------------------------

ma <- mapply(FUN=datetime12,grid_ST$Sets,grid_ST$Traps)

beep()

grid_ST$DateTime1 <- t(ma)[,1]
grid_ST$DateTime2 <- t(ma)[,2]
grid_ST <- as.data.frame(grid_ST)
grid_ST

data <- data[,names(data)[!(names(data) %in% c("DateTime1","DateTime2","Period"))]]

data <- sqldf("SELECT
				data.*
				,grid_ST.DateTime1
				,grid_ST.DateTime2
				FROM data
				LEFT JOIN grid_ST
				ON data.Sets = grid_ST.Sets
					and data.Traps = grid_ST.Traps
				;")

beep()

data$DateTime1 <- as.POSIXct(data$DateTime1,format="%Y-%m-%d %H:%M:%S")
data$DateTime2 <- as.POSIXct(data$DateTime2,format="%Y-%m-%d %H:%M:%S")

data$Period <- ifelse(data$DateTime <= data$DateTime1,1,ifelse(data$DateTime <= data$DateTime2,2,3))

st <- 1
for(st in seq(1,10,1)){	

	Set <- grid_ST[st,"Sets"]
	Trap <- grid_ST[st,"Traps"]

	data_st <- data[data$Sets == Set & data$Traps == Trap,]

	data_st$Cumu_diff <- cumsum((data_st$VecSum - median(data_st$VecSum))^2)

	plot(data_st$DateTime,data_st$VecSum,type='l')

	plot(data_st$DateTime,data_st$VecSum - median(data_st$VecSum),type='l')

	plot(data_st$DateTime,sqrt((data_st$VecSum - median(data_st$VecSum))^2),type='l')

	plot(data_st$DateTime,cumsum(((data_st$VecSum - median(data_st$VecSum))^2)),type='l')


	data_st$Time <- seq(1,nrow(data_st),1)

	lm <- lm(Cumu_diff ~ Time,data=data_st)

	plot(data_st$DateTime,data_st$Cumu_diff,type='l')
	lines(data_st$DateTime,	predict(lm,type='response'))
	abline(v=data_st$DateTime1[1],col='red')
	abline(v=data_st$DateTime2[1],col='red')

	dev.new()
}

graphics.off()


x <- c(1:10, 13:22)
y <- numeric(20)
## Create first segment
y[1:10] <- 20:11 + rnorm(10, 0, 1.5)
## Create second segment
y[11:20] <- seq(11, 15, len=10) + rnorm(10, 0, 1.5)
## Plot it
par(mar=c(4,4,1,1)+0.2)
plot(x,y, ylim=c(5, 20), pch=16)

lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, seg.Z = ~x+x, psi=c(14))

segmented.mod

st <- 10

Set <- grid_ST[st,"Sets"]
Trap <- grid_ST[st,"Traps"]

data_st <- data[data$Sets == Set & data$Traps == Trap,]

data_st$Time <- seq(1,nrow(data_st),1)

smth <- smooth.spline(seq(1,nrow(data_st),1),data_st$VecSum,spar=10^(-50))
data_st$VecSum_l <- predict(smth,seq(1,nrow(data_st),1),type='response')$y

data_st$Cumu_diff <- cumsum((data_st$VecSum - data_st$VecSum_l)^2)

plot(data_st$Time,data_st$Cumu_diff,type='l')

# 1
data_st1 <- data_st[seq(1,floor(nrow(data_st)/2),1),]

lm1 <- lm(Cumu_diff ~ Time,data=data_st1)
segmented1 <- segmented(lm1, seg.Z = ~ Time, psi=c(data_st1$Time[floor(nrow(data_st1)/2)]))
dt1 <- round(segmented1[["psi"]][2],0)

# 2
data_st2 <- data_st[seq(ceiling(nrow(data_st)/2),nrow(data_st),1),]

lm2 <- lm(Cumu_diff ~ Time,data=data_st2)
segmented2 <- segmented(lm2, seg.Z = ~ Time, psi=c(data_st2$Time[floor(nrow(data_st2)/2)]))
dt2 <- round(segmented2[["psi"]][2],0)

plot(data_st1$Time,data_st1$Cumu_diff,type='l')

lm1 <- lm(Cumu_diff ~ Time,data=data_st1)
segmented1 <- segmented(lm1, seg.Z = ~ Time, psi=c(data_st1$Time[floor(nrow(data_st1)/2)]))
dt1 <- round(segmented1[["psi"]][2],0)

plot(data_st1$Time,data_st1$Cumu_diff,type='l')
abline(v=dt1)

plot(data_st2$Time,data_st2$Cumu_diff,type='l')
abline(v=dt2)

plot(c(data_st1$Time,data_st2$Time),c(data_st1$Cumu_diff,data_st2$Cumu_diff))
abline(v=dt1)
abline(v=dt2)

DateTime1 <- as.character(data_st$DateTime[dt1])
DateTime2 <- as.character(data_st$DateTime[dt2])


