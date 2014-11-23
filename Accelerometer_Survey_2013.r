# install.packages("plyr")
# install.packages("sqldf")
# install.packages("tcltk")
# install.packages("lubridate")
# install.packages("beepr")
# install.packages("doParallel")
# install.packages("beepr")
# install.packages("stats")
# install.packages("signal")

library(plyr)
library(tcltk)
library(sqldf)
library(lubridate)
library(beepr)
library(doParallel)
library(beepr)
library(stats)
library(signal)

setwd("/home/jim/Dropbox/REM/tasks/accelerometer/DFO data request - August, 2014")

# read in data
Accelerometer_Survey_2013 <- read.csv("Accelerometer_Survey_2013.csv")
Accelerometer_Survey_2013$DateTime <- as.POSIXct(Accelerometer_Survey_2013$DateTime,format="%Y-%m-%d %H:%M:%S")
Accelerometer_Survey_2013$Sets <- Accelerometer_Survey_2013$Set
Accelerometer_Survey_2013$Traps <- Accelerometer_Survey_2013$Trap
Accelerometer_Survey_2013 <- Accelerometer_Survey_2013[order(Accelerometer_Survey_2013$Sets,Accelerometer_Survey_2013$Traps),]

# VecSum
Accelerometer_Survey_2013$VecSum <- sqrt(Accelerometer_Survey_2013$XAccel^2 + Accelerometer_Survey_2013$YAccel^2 + Accelerometer_Survey_2013$ZAccel^2)

# set year of survey
year <- min(year(Accelerometer_Survey_2013$DateTime))

# sets and traps sets
Sets <- unique(Accelerometer_Survey_2013$Sets)
Traps <- unique(Accelerometer_Survey_2013$Traps)

# grid of set & trap combinations
grid_ST <- unique(data.frame(Sets=Accelerometer_Survey_2013$Sets,Traps=Accelerometer_Survey_2013$Traps))
grid_ST <- grid_ST[order(grid_ST$Sets,grid_ST$Traps),]
head(grid_ST)

# define function time difference
time_diff <- function(DateTime){as.numeric(max(DateTime) - min(DateTime))*24}

# summarize set and trap by length of time
d <- ddply(Accelerometer_Survey_2013,.(Set,Trap),summarize,time_diff=time_diff(DateTime),length=length(DateTime)*5/(60*60))
d

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

# order by sets and traps, datetime
data <- data[order(data$Sets,data$Traps,data$DateTime),]

st <- 1
for(st in seq(1,5,1)){	
	Set <- grid_ST[st,"Sets"]
	Trap <- grid_ST[st,"Traps"]

	data_st <- data[data$Sets == Set & data$Traps == Trap,]
	Accelerometer_Survey_2013_s <- Accelerometer_Survey_2013[Accelerometer_Survey_2013$Sets == set & Accelerometer_Survey_2013$Traps == trap,]

	plot(Accelerometer_Survey_2013_s$DateTime,Accelerometer_Survey_2013_s$VecSum,type='l')
	lines(data_st$DateTime,data_st$VecSum,type='l',col='red')
	dev.new()
}
graphics.off()

# save.image('Accelerometer_Survey_2013.RData')

setwd("/home/jim/Dropbox/REM/tasks/accelerometer/DFO data request - August, 2014")
# load('Accelerometer_Survey_2013.RData')

# comment this out
grid_ST <- grid_ST[seq(1,10,1),]
nrow(grid_ST)

data <- data[data$Sets %in% grid_ST$Sets & data$Traps %in% grid_ST$Traps,]

nrow(data)
nrow(Accelerometer_Survey_2013)s

deriv <- function(x){
	dx <- x[seq(2,length(x),1)] - x[seq(1,length(x)-1,1)]
	dx <- c(dx[1],dx)
}

cost <- function(data,DateTime1,DateTime2){
	if(DateTime1 >= DateTime2 | DateTime1 <= 2 | DateTime2 >= nrow(data) - 2) {
		cost <- Inf
	} else {

		data$VecSum[data$VecSum > 1.2] <- 1.2

		smth <- smooth.spline(seq(1,nrow(data),1),data$VecSum,spar=10^(-50))
		data$VecSum_l <- predict(smth,seq(1,nrow(data),1),type='response')$y

		data$Cumu_diff <- cumsum((data$VecSum - data$VecSum_l)^2)
		data$Time <- seq(1,nrow(data),1)

		# splits data into three piecies based on dates
		data1 <- data[seq(1,DateTime1,1),]
		data2 <- data[seq(DateTime1,DateTime2,1),]
		data3 <- data[seq(DateTime2,nrow(data),1),]

		data1$Time <- seq(1,nrow(data1),1) 
		data2$Time <- seq(1,nrow(data2),1) 
		data3$Time <- seq(1,nrow(data3),1) 

# ---------------------------------------------------

		# smth1 <- smooth.spline(seq(1,nrow(data1),1),data1$Velo,spar=10^(-50))
		# data1$Velo_l <- predict(smth1,seq(1,nrow(data1),1),type='response')$y

		# smth2 <- smooth.spline(seq(1,nrow(data2),1),data2$Velo,spar=10^(-50))
		# data2$Velo_l <- predict(smth2,seq(1,nrow(data2),1),type='response')$y

		# smth3 <- smooth.spline(seq(1,nrow(data3),1),data3$Velo,spar=10^(-50))
		# data3$Velo_l <- predict(smth3,seq(1,nrow(data3),1),type='response')$y

# ---------------------------------------------------

		lm1 <- lm(Cumu_diff ~ Time,data=data1)
		data1$Cumu_diff_l <- predict(lm1,type='response')

		lm2 <- lm(Cumu_diff ~ Time,data=data2)
		data2$Cumu_diff_l <- predict(lm2,type='response')

		lm3 <- lm(Cumu_diff ~ Time,data=data3)
		data3$Cumu_diff_l <- predict(lm3,type='response')

# ---------------------------------------------------

		# bf <- butter(2, 0.2, type = 'low', plane='z')

		# data1$VecSum_l <- filter(bf,data1$VecSum)

		# data2$VecSum_l <- filter(bf,data2$VecSum)
		# data2$VecSum_l <- rep(median(data2$VecSum_l),nrow(data2))
		
		# data3$VecSum_l <- filter(bf,data3$VecSum)

# ---------------------------------------------------

		# defines cost of each segment as mean difference from the median
		cost1 <- sum((data1$Cumu_diff - data1$Cumu_diff_l)^2)
		cost2 <- sum((data2$Cumu_diff - data2$Cumu_diff_l)^2)
		cost3 <- sum((data3$Cumu_diff - data3$Cumu_diff_l)^2)

		cost <-  cost1 + cost2 + cost3

	}
	return(cost)
}

# # set up parallel processing cores for foreach
# library(doParallel)
# cl <- makeCluster(4)
# registerDoParallel(cl)

# # empty datatime fields for grid_ST
# grid_ST$DateTime1 <- rep(NA,nrow(grid_ST))
# grid_ST$DateTime2 <- rep(NA,nrow(grid_ST))

datetime12 <- function(Set,Trap){

	data_st <- data[data$Sets == Set & data$Traps == Trap,]

	cost_st <- function(DateTimes){return(cost(data_st,DateTimes[1],DateTimes[2]))}

	opt <- optim(c(3,nrow(data_st) - 3),cost_st)

	DateTime1 <- as.character(data_st$DateTime[round(opt$par[1],0)])
	DateTime2 <- as.character(data_st$DateTime[round(opt$par[2],0)])

	return(c(DateTime1,DateTime2))
}

# ------------------------

ma <- mapply(FUN=datetime12,grid_ST$Sets,grid_ST$Traps)

beep()

grid_ST$DateTime1 <- t(ma)[,1]
grid_ST$DateTime2 <- t(ma)[,2]
grid_ST <- as.data.frame(grid_ST)
grid_ST

# save.image('Accelerometer_Survey_2013.RData')

setwd("/home/jim/Dropbox/REM/tasks/accelerometer/DFO data request - August, 2014")
# load('Accelerometer_Survey_2013.RData')

# grid_ST$DateTime1 <- as.POSIXct(grid_ST$DateTime1,format="%Y-%m-%d %H:%M:%S")
# grid_ST$DateTime2 <- as.POSIXct(grid_ST$DateTime2,format="%Y-%m-%d %H:%M:%S")

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

save.image('Accelerometer_Survey_2013.RData')

setwd("/home/jim/Dropbox/REM/tasks/accelerometer/DFO data request - August, 2014")
# load('Accelerometer_Survey_2013.RData')

nrow(data)

st <- 1
for(st in seq(1,10,1)){	

	set <- grid_ST[st,"Sets"]
	trap <- grid_ST[st,"Traps"]

	data_st <- data[data$Sets == set & data$Traps == trap,]

	# plot(data_st$DateTime,data_st$VecSum,type='l')
	# lines(data_st$DateTime,data_st$VecSum_l,type='l',col='red')
	# abline(v=data_st$DateTime1[1],col='red')
	# abline(v=data_st$DateTime2[1],col='red')

	data_st$Velo <- cumsum(data_st$VecSum - median(data_st$VecSum))

	smth <- smooth.spline(seq(1,nrow(data_st),1),data_st$Velo,spar=10^(-50))
	data_st$Velo_l <- predict(smth,seq(1,nrow(data_st),1),type='response')$y

	# plot(data_st$DateTime,deriv(data_st$Velo),type='l')
	# lines(data_st$DateTime,deriv(data_st$Velo_l),type='l',col='red')
	
	# plot(data_st$DateTime,data_st$Velo-data_st$Velo_l,type='l')
	# lines(data_st$DateTime,0.01*data_st$Velo_l,type='l',col='red')
	# abline(v=data_st$DateTime1[1],col='red')
	# abline(v=data_st$DateTime2[1],col='red')

	# smth2 <- smooth.spline(seq(1,nrow(data_st),1),(data_st$Velo-data_st$Velo_l)^2,spar=10^(-50))
	# data_st$Velo_l2 <- predict(smth2,seq(1,nrow(data_st),1),type='response')$y

	# plot(data_st$DateTime,cumsum((data_st$Velo - data_st$Velo_l)^2),type='l')
	# # lines(data_st$DateTime,data_st$Velo_l2,type='l',col='red')
	# abline(v=data_st$DateTime1[1],col='red')
	# abline(v=data_st$DateTime2[1],col='red')

	smth <- smooth.spline(seq(1,nrow(data_st),1),data_st$VecSum,spar=10^(-50))
	data_st$VecSum_l <- predict(smth,seq(1,nrow(data_st),1),type='response')$y

	data_st$Cumu_diff <- cumsum((data_st$VecSum-data_st$VecSum_l)^2)

	plot(data_st$DateTime,data_st$Cumu_diff)
	data_st$Time <- seq(1,nrow(data_st),1)

	lm <- lm(Cumu_diff ~ Time,data=data_st)

	plot(data_st$DateTime,data_st$Cumu_diff,type='l')
	lines(data_st$DateTime,	predict(lm,type='response'))
	abline(v=data_st$DateTime1[1],col='red')
	abline(v=data_st$DateTime2[1],col='red')

	dev.new()
}

graphics.off()