# install.packages("plyr")
# install.packages("sqldf")
# install.packages("tcltk")
# install.packages("lubridate")
# install.packages("beepr")
# install.packages("doParallel")
# install.packages("beepr")
# install.packages("stats")
# install.packages("signal")
# install.packages("animation")
# install.packages("knitr")

library(plyr)
library(tcltk)
library(sqldf)
library(lubridate)
library(beepr)
library(doParallel)
library(beepr)
library(stats)
library(signal)
library(animation)
library(knitr)

setwd("/home/jim/Dropbox/REM/Tasks/accelerometer/clustering")

# read in data
Accelerometer_Survey_2012 <- read.csv("Accelerometer_Survey_2012.csv")
Accelerometer_Survey_2013 <- read.csv("Accelerometer_Survey_2013.csv")

# set year
Accelerometer_Survey_2012$Year <- year(Accelerometer_Survey_2012$DateTime) 
Accelerometer_Survey_2013$Year <- year(Accelerometer_Survey_2013$DateTime) 

# list of names for each data set
names2012 <- sort(names(Accelerometer_Survey_2012))
names2013 <- sort(names(Accelerometer_Survey_2013))

# subsets data sets by names shared by both of them. (for joining them together)
Accelerometer_Survey_2012 <- Accelerometer_Survey_2012[,names2012[names2012 %in% names2013]]
Accelerometer_Survey_2013 <- Accelerometer_Survey_2013[,names2012[names2012 %in% names2013]]

# joins the 2012, 2013 data sets
Accelerometer_Survey <- rbind(Accelerometer_Survey_2012,Accelerometer_Survey_2013)

# data cleaning, formats DateTime, orders by DateTime etc
Accelerometer_Survey$DateTime <- as.POSIXct(Accelerometer_Survey$DateTime,format="%Y-%m-%d %H:%M:%S")
Accelerometer_Survey$Sets <- Accelerometer_Survey$Set
Accelerometer_Survey$Traps <- Accelerometer_Survey$Trap
Accelerometer_Survey <- Accelerometer_Survey[order(Accelerometer_Survey$Sets,Accelerometer_Survey$Traps),]

# Sets VecSum 
Accelerometer_Survey$VecSum <- sqrt(Accelerometer_Survey$XAccel^2 + Accelerometer_Survey$YAccel^2 + Accelerometer_Survey$ZAccel^2)

# define function time difference
time_diff <- function(DateTime){as.numeric(max(DateTime) - min(DateTime))*24}

# summarize set and trap by length of time
d <- ddply(Accelerometer_Survey,.(Set,Trap),summarize,time_diff=time_diff(DateTime),length=length(DateTime)*5/(60*60))
head(d)

# read in sensor fishing events data, (used for start and end time for sets)
Sensor_Fishing_Events <- read.csv("Sensor_Fishing_Events.csv")
Sensor_Fishing_Events <- Sensor_Fishing_Events[order(Sensor_Fishing_Events$SETNO),]

# join two data sets
data <- sqldf("SELECT 
				Accelerometer_Survey.*
				,Sensor_Fishing_Events.*
				FROM Accelerometer_Survey
				LEFT JOIN Sensor_Fishing_Events
				ON Accelerometer_Survey.Sets = Sensor_Fishing_Events.SETNO
					AND Accelerometer_Survey.Year = Sensor_Fishing_Events.YEAR
				;")

# start time and end time
data$Start_DateTime <- as.POSIXct(paste(data$YEAR,"-",data$MON,"-",data$DAY," ",data$TIME,":00",sep=""),format="%Y-%m-%d %H:%M:%S")
data$End_DateTime <-data$Start_DateTime + data$DURATION*60

# subset data based on start time and end time
data <- data[data$DateTime >= data$Start_DateTime & data$DateTime <= data$End_DateTime,]

# subset column names
data <- data[,c(names(Accelerometer_Survey),"Start_DateTime","End_DateTime")]

# order by sets and traps, datetime
data <- data[order(data$Sets,data$Traps,data$DateTime),]

# grid of set & trap & year combinations
grid_STY <- unique(data.frame(Sets=data$Sets,Traps=data$Traps,Year=data$Year))

data$XAccel_l <- rep(NA,nrow(data))
data$YAccel_l <- rep(NA,nrow(data))
data$ZAccel_l <- rep(NA,nrow(data))

# apply low pass filter to x, y, z acceleration data
cl <- makeCluster(4)
registerDoParallel(cl)
sty <- 1

low_pass_X <- function(sty){

	Set <- grid_STY[sty,"Sets"]
	Trap <- grid_STY[sty,"Traps"]
	Year <- grid_STY[sty,"Year"]

	data_sty <- data[data$Sets == Set & data$Traps == Trap & data$Year == Year,]

	smth <- smooth.spline(seq(1,nrow(data_sty),1),data_sty$XAccel,spar=10^(-50))
	XAccel_l <- predict(smth,seq(1,nrow(data_sty),1),type='response')$y

	return(XAccel_l - median(XAccel_l))
}

low_pass_Y <- function(sty){

	Set <- grid_STY[sty,"Sets"]
	Trap <- grid_STY[sty,"Traps"]
	Year <- grid_STY[sty,"Year"]

	data_sty <- data[data$Sets == Set & data$Traps == Trap & data$Year == Year,]

	smth <- smooth.spline(seq(1,nrow(data_sty),1),data_sty$YAccel,spar=10^(-50))
	YAccel_l <- predict(smth,seq(1,nrow(data_sty),1),type='response')$y

	return(YAccel_l - median(YAccel_l))
}

low_pass_Z <- function(sty){

	Set <- grid_STY[sty,"Sets"]
	Trap <- grid_STY[sty,"Traps"]
	Year <- grid_STY[sty,"Year"]

	data_sty <- data[data$Sets == Set & data$Traps == Trap & data$Year == Year,]

	smth <- smooth.spline(seq(1,nrow(data_sty),1),data_sty$ZAccel,spar=10^(-50))
	ZAccel_l <- predict(smth,seq(1,nrow(data_sty),1),type='response')$y

	return(ZAccel_l - median(ZAccel_l))
}


low_pass_V <- function(sty){

	Set <- grid_STY[sty,"Sets"]
	Trap <- grid_STY[sty,"Traps"]
	Year <- grid_STY[sty,"Year"]

	data_sty <- data[data$Sets == Set & data$Traps == Trap & data$Year == Year,]

	smth <- smooth.spline(seq(1,nrow(data_sty),1),data_sty$VecSum,spar=10^(-50))
	VecSum <- predict(smth,seq(1,nrow(data_sty),1),type='response')$y

	return(VecSum - median(VecSum))
}

la_X <- lapply(seq(1,nrow(grid_STY),1),low_pass_X)
la_Y <- lapply(seq(1,nrow(grid_STY),1),low_pass_Y)
la_Z <- lapply(seq(1,nrow(grid_STY),1),low_pass_Z)
la_V <- lapply(seq(1,nrow(grid_STY),1),low_pass_V)


data$XAccel_l <- unlist(la_X)
data$YAccel_l <- unlist(la_Y)
data$ZAccel_l <- unlist(la_Z)
data$VecSum_l <- unlist(la_V)

beep()

head(data)

# set time buffer 
buffer <- 2*60*60
buffer
rows <- buffer/5
rows
print(paste("Minutes = ",buffer/60,", Hours = ",buffer/(60*60),sep=""))

# Set Period
data$Period <- ifelse(data$DateTime < data$Start_DateTime + buffer,1,ifelse(data$DateTime > data$End_DateTime - buffer,3,2))

# plot periods and timescale for first 5 set, trap and year combos
for(sty in seq(1,5,1)){

	Set <- grid_STY[sty,"Sets"]
	Trap <- grid_STY[sty,"Traps"]
	Year <- grid_STY[sty,"Year"]

	data_sty <- data[data$Sets == Set & data$Traps == Trap & data$Year == Year,]
	Accelerometer_Survey_sty <- Accelerometer_Survey[Accelerometer_Survey$Sets == Set & Accelerometer_Survey$Traps == Trap & Accelerometer_Survey$Year == Year,]

	# plot(Accelerometer_Survey_sty$DateTime,Accelerometer_Survey_sty$VecSum,type='l')
	# lines(data_sty$DateTime,data_sty$VecSum,type='l',col='red')
	# abline(v=data_sty$Start_DateTime + buffer)
	# abline(v=data_sty$End_DateTime - buffer)
	# dev.new()

	plot(data_sty$DateTime,data_sty$VecSum,type='l',col='red',ylim=c(0.9,1.5))
	abline(v=data_sty$Start_DateTime + buffer)
	abline(v=data_sty$End_DateTime - buffer)
	dev.new()

	# plot(data_sty$DateTime,data_sty$YAccel - median(data_sty$YAccel),type='l',col='red',ylim=c(-0.5,0.5))
	# lines(data_sty$DateTime,data_sty$YAccel_l)
# 	abline(v=data_sty$Start_DateTime + buffer)
# 	abline(v=data_sty$End_DateTime - buffer)
# 	dev.new()

}

graphics.off()

agg_STP <- ddply(data,.(Sets,Traps,Year,Period),summarize,x=length(VecSum))
head(agg_STP) 

agg_STP_p <- agg_STP[agg_STP$Period == 1 & agg_STP$x == rows,]
agg_STP_p

data <- data[,!(names(data) %in% c("x"))]
data <- sqldf("SELECT
		data.*
		,agg_STP_p.x
		from data
		left join agg_STP_p
		on agg_STP_p.Sets = data.Sets
			and agg_STP_p.Traps = data.Traps
			and agg_STP_p.Year = data.Year
			and agg_STP_p.Period = data.Period")

data_p <- data[data$Period == 1 & data$x == rows & is.na(data$x) == FALSE,]
nrow(data_p)/rows

# grid of set & trap combinations
grid_STY <- unique(data.frame(Sets=Accelerometer_Survey$Sets,Traps=Accelerometer_Survey$Traps,Year=Accelerometer_Survey$Year))

beep()

save.image("Accelerometer_Survey_2013_kmeans.RData")

# ------------------------------------------------------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ------------------------------------------------------------


library(plyr)
library(tcltk)
library(sqldf)
library(lubridate)
library(beepr)
library(doParallel)
library(beepr)
library(stats)
library(signal)
library(animation)
library(knitr)

setwd("/home/jim/Dropbox/REM/Tasks/accelerometer/clustering")
load("Accelerometer_Survey_2013_kmeans.RData")
data <- sqldf("SELECT
		data.*
		,agg_STP_p.x
		from data
		left join agg_STP_p
		on agg_STP_p.Sets = data.Sets
			and agg_STP_p.Traps = data.Traps
			and agg_STP_p.Year = data.Year
			and agg_STP_p.Period = data.Period")

data_p <- data[data$Period == 1 & data$x == rows & is.na(data$x) == FALSE,]
length(data_p$VecSum)/rows

# grid of set & trap combinations
grid_STY <- unique(data.frame(Sets=data_p$Sets,Traps=data_p$Traps,Year=data_p$Year))

# period 1

# ------------------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# x, y and z

# VecSum
x <- matrix(data_p[data_p$Period == 1,]$VecSum_l,ncol=rows,byrow=TRUE)
x <- as.data.frame(x)
length(data_p[data_p$Period == 1,]$VecSum_l)/rows
nrow(x)

# X
xX <- matrix(data_p[data_p$Period == 1,]$XAccel_l,ncol=rows,byrow=TRUE)
xX <- as.data.frame(xX)
length(data_p[data_p$Period == 1,]$VecSum)/rows

xY <- matrix(data_p[data_p$Period == 1,]$YAccel_l,ncol=rows,byrow=TRUE)
xY <- as.data.frame(xY)
length(data_p[data_p$Period == 1,]$VecSum)/rows

xZ <- matrix(data_p[data_p$Period == 1,]$ZAccel_l,ncol=rows,byrow=TRUE)
xZ <- as.data.frame(xZ)
length(data_p[data_p$Period == 1,]$VecSum)/rows

Ones <- matrix(rep(1,nrow(x)*ncol(x)),ncol=rows)

distX <- dist(xX,method = "euclidean")
distY <- dist(xY,method = "euclidean")
distZ <- dist(xZ,method = "euclidean")

# dist <- distX^2 +  distY^2 + distZ^2
dist <- dist(x,method = "euclidean")

# wardd Hierarchical Clustering
fit <- hclust(dist, method="average") 
fit <- hclust(dist, method="ward.D") 

png("cluster_dendogram.png")
plot(fit) # display dendogram
groups <- cutree(fit, k=8) 
# draw dendogram with red borders around the 8 clusters 
rect.hclust(fit, k=8, border="red")
dev.off()

# -----------------------------------------------------------------------

cl <- 1
cl in unique(groups))
for(cl in unique(groups)){

	r <- 1

	x_cl <- x[groups == cl,]

	xX_cl <- xX[groups == cl,]
	xY_cl <- xY[groups == cl,]
	xZ_cl <- xZ[groups == cl,]

	grid_STY_cl <- grid_STY[groups == cl,]


	quans_med <- function(x){as.numeric(quantile(x,probs=c(0.5)))}
	quans_upr <- function(x){as.numeric(quantile(x,probs=c(0.01)))}
	quans_lwr <- function(x){as.numeric(quantile(x,probs=c(0.99)))}

	x1 <- seq(Sys.time(),Sys.time() +  buffer, length=rows)
	x1 <- as.POSIXct(x1)
	x2 <- seq(Sys.time() + buffer , Sys.time(),length=rows)
	x2 <- as.POSIXct(x2)

	y1 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_upr)
	y2 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_lwr)

	plot(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',ylim=c(-0.5,0.5),
		main=paste(nrow(x_cl),"\n",
			"Set = ",grid_STY_cl[r,"Sets"],", ",
			"Trap = ",grid_STY_cl[r,"Traps"],", ",
			"Year = ",grid_STY_cl[r,"Year"],sep=""),
		col='red')
			
	# draw 95% credible region using polygon function. "rev" is reverse function.
	polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
	# sample
	lines(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',main=paste(nrow(x_cl)),col='red')
	# median
	lines(x1,apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(0.9,1.5))

	# dev.new()

	ani.options(interval=4/nrow(x_cl),autobrowse=FALSE)

	animationV <- function(){
		for(r in seq(1,nrow(x_cl),1)){

			plot(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',ylim=c(-0.5,0.5),
			main=paste(nrow(x_cl),"\n",
				"Set = ",grid_STY_cl[r,"Sets"],", ",
				"Trap = ",grid_STY_cl[r,"Traps"],", ",
				"Year = ",grid_STY_cl[r,"Year"],sep=""),
			col='red')

			# draw 95% credible region using polygon function. "rev" is reverse function.
			polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
			# sample
			lines(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',main=paste(nrow(x_cl)),col='red')
			# median
			lines(x1,apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(0.9,1.5))

		}
	}

		animationX <- function(){
		for(r in seq(1,nrow(x_cl),1)){

			plot(x1,xX_cl[r,seq(1,ncol(xX_cl),1)],type='l',ylim=c(-0.5,0.5),
			main=paste(nrow(xX_cl),"\n",
				"Set = ",grid_STY_cl[r,"Sets"],", ",
				"Trap = ",grid_STY_cl[r,"Traps"],", ",
				"Year = ",grid_STY_cl[r,"Year"],sep=""),
			col='red')

			y1 <- apply((xX_cl[,seq(1,ncol(xX_cl),1)]),MARGIN=2,FUN=quans_upr)
			y2 <- apply((xX_cl[,seq(1,ncol(xX_cl),1)]),MARGIN=2,FUN=quans_lwr)
				
			# draw 95% credible region using polygon function. "rev" is reverse function.
			polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
			# sample
			lines(x1,xX_cl[r,seq(1,ncol(xX_cl),1)],type='l',main=paste(nrow(xX_cl)),col='red')
			# median
			lines(x1,apply((xX_cl[,seq(1,ncol(xX_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(-0.5,0.5))
		}
	}

		animationY <- function(){
		for(r in seq(1,nrow(xY_cl),1)){

			plot(x1,xY_cl[r,seq(1,ncol(xY_cl),1)],type='l',ylim=c(-0.5,0.5),
			main=paste(nrow(xY_cl),"\n",
				"Set = ",grid_STY_cl[r,"Sets"],", ",
				"Trap = ",grid_STY_cl[r,"Traps"],", ",
				"Year = ",grid_STY_cl[r,"Year"],sep=""),
			col='red')

			y1 <- apply((xY_cl[,seq(1,ncol(xY_cl),1)]),MARGIN=2,FUN=quans_upr)
			y2 <- apply((xY_cl[,seq(1,ncol(xY_cl),1)]),MARGIN=2,FUN=quans_lwr)
				
			# draw 95% credible region using polygon function. "rev" is reverse function.
			polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
			# sample
			lines(x1,xY_cl[r,seq(1,ncol(xY_cl),1)],type='l',main=paste(nrow(xY_cl)),col='red')
			# median
			lines(x1,apply((xY_cl[,seq(1,ncol(xY_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(-0.5,0.5))
		}
	}

		animationZ <- function(){
		for(r in seq(1,nrow(x_cl),1)){

			plot(x1,xZ_cl[r,seq(1,ncol(xZ_cl),1)],type='l',ylim=c(-0.5,0.5),
			main=paste(nrow(xZ_cl),"\n",
				"Set = ",grid_STY_cl[r,"Sets"],", ",
				"Trap = ",grid_STY_cl[r,"Traps"],", ",
				"Year = ",grid_STY_cl[r,"Year"],sep=""),
			col='red')

			y1 <- apply((xZ_cl[,seq(1,ncol(xZ_cl),1)]),MARGIN=2,FUN=quans_upr)
			y2 <- apply((xZ_cl[,seq(1,ncol(xZ_cl),1)]),MARGIN=2,FUN=quans_lwr)
				
			# draw 95% credible region using polygon function. "rev" is reverse function.
			polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
			# sample
			lines(x1,xZ_cl[r,seq(1,ncol(xZ_cl),1)],type='l',main=paste(nrow(xZ_cl)),col='red')
			# median
			lines(x1,apply((xZ_cl[,seq(1,ncol(xZ_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(0.9,1.5))
		}
	}

		animationXYZ <- function(){
		for(r in seq(1,nrow(x_cl),1)){

			plot(x1,xZ_cl[r,seq(1,ncol(xZ_cl),1)],type='l',ylim=c(-1.5,1.5),
			main=paste(nrow(xZ_cl),"\n",
				"Set = ",data_p[r,"Sets"],", ",
				"Trap = ",data_p[r,"Traps"],", ",
				"Year = ",data_p[r,"Year"],sep=""),
			col='red')

			yX1 <- apply((xZ_cl[,seq(1,ncol(xX_cl),1)]),MARGIN=2,FUN=quans_upr) + 1
			yX2 <- apply((xZ_cl[,seq(1,ncol(xX_cl),1)]),MARGIN=2,FUN=quans_lwr) + 1
			
			yY1 <- apply((xZ_cl[,seq(1,ncol(xY_cl),1)]),MARGIN=2,FUN=quans_upr)
			yY2 <- apply((xZ_cl[,seq(1,ncol(xY_cl),1)]),MARGIN=2,FUN=quans_lwr)

			yZ1 <- apply((xZ_cl[,seq(1,ncol(xZ_cl),1)]),MARGIN=2,FUN=quans_upr) - 1
			yZ2 <- apply((xZ_cl[,seq(1,ncol(xZ_cl),1)]),MARGIN=2,FUN=quans_lwr) - 1

			# draw 95% credible region using polygon function. "rev" is reverse function.
			polygon( x=c(x1,x2), y=c(yX1,rev(yX2)), border=NA, col="gray", bg="gray"  )
			polygon( x=c(x1,x2), y=c(yY1,rev(yY2)), border=NA, col="gray", bg="gray"  )
			polygon( x=c(x1,x2), y=c(yZ1,rev(yZ2)), border=NA, col="gray", bg="gray"  )
		
			# sample
			lines(x1,xX_cl[r,seq(1,ncol(xX_cl),1)],type='l',main=paste(nrow(xX_cl)),col='red')
			lines(x1,xY_cl[r,seq(1,ncol(xY_cl),1)],type='l',main=paste(nrow(xY_cl)),col='red')
			lines(x1,xZ_cl[r,seq(1,ncol(xZ_cl),1)],type='l',main=paste(nrow(xZ_cl)),col='red')
			# median
			lines(x1,apply((xX_cl[,seq(1,ncol(xX_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(-1.5,1.5))
			lines(x1,apply((xY_cl[,seq(1,ncol(xY_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(-1.5,1.5))
			lines(x1,apply((xZ_cl[,seq(1,ncol(xZ_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(-1.5,1.5))
		}
	}
	saveGIF(animationV(),movie.name=paste("cluster_",cl,".gif",sep=""))
	# saveGIF(animationX(),movie.name=paste("clusterX_",cl,".gif",sep=""))
	# saveGIF(animationY(),movie.name=paste("clusterY_",cl,".gif",sep=""))
	# saveGIF(animationZ(),movie.name=paste("clusterZ_",cl,".gif",sep=""))
	# saveGIF(animationXYZ(),movie.name=paste("clusterXYZ_",cl,".gif",sep=""))
}

data.frame(grid_STY,groups=groups)

# -------------------------------------------------------------------

graphics.off()

sty <- 28

Set <- grid_STY[sty,"Sets"]
Trap <- grid_STY[sty,"Traps"]
Year <- grid_STY[sty,"Year"]

Set
Trap
Year


data_sty <- data[data$Sets == Set & data$Traps == Trap & data$Year == Year,]

# plot(Accelerometer_Survey_sty$DateTime,Accelerometer_Survey_sty$VecSum,type='l')
# lines(data_sty$DateTime,data_sty$VecSum,type='l',col='red')
# abline(v=data_sty$Start_DateTime + buffer)
# abline(v=data_sty$End_DateTime - buffer)
# dev.new()

plot(data_sty$DateTime,data_sty$VecSum,type='l',col='red',ylim=c(0.9,1.5))
abline(v=data_sty$Start_DateTime + buffer)
abline(v=data_sty$End_DateTime - buffer)
dev.new()

# plot(data_sty$DateTime,data_sty$YAccel - median(data_sty$YAccel),type='l',col='red',ylim=c(-0.5,0.5))
# lines(data_sty$DateTime,data_sty$YAccel_l)
# abline(v=data_sty$Start_DateTime + buffer)
# abline(v=data_sty$End_DateTime - buffer)
# dev.new()
