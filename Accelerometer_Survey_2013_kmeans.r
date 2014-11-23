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

Accelerometer_Survey_2012$Year <- year(Accelerometer_Survey_2012$DateTime) 
Accelerometer_Survey_2013$Year <- year(Accelerometer_Survey_2013$DateTime) 

head(Accelerometer_Survey_2012)
head(Accelerometer_Survey_2013)

names2012 <- sort(names(Accelerometer_Survey_2012))
names2013 <- sort(names(Accelerometer_Survey_2013))

Accelerometer_Survey_2012 <- Accelerometer_Survey_2012[,names2012[names2012 %in% names2013]]
Accelerometer_Survey_2013 <- Accelerometer_Survey_2013[,names2012[names2012 %in% names2013]]

head(Accelerometer_Survey_2012)
head(Accelerometer_Survey_2013)

Accelerometer_Survey <- rbind(Accelerometer_Survey_2012,Accelerometer_Survey_2013)

Accelerometer_Survey$DateTime <- as.POSIXct(Accelerometer_Survey$DateTime,format="%Y-%m-%d %H:%M:%S")
Accelerometer_Survey$Sets <- Accelerometer_Survey$Set
Accelerometer_Survey$Traps <- Accelerometer_Survey$Trap
Accelerometer_Survey <- Accelerometer_Survey[order(Accelerometer_Survey$Sets,Accelerometer_Survey$Traps),]

# VecSum
Accelerometer_Survey$VecSum <- sqrt(Accelerometer_Survey$XAccel^2 + Accelerometer_Survey$YAccel^2 + Accelerometer_Survey$ZAccel^2)

# sets and traps sets
Sets <- unique(Accelerometer_Survey$Sets)
Traps <- unique(Accelerometer_Survey$Traps)

# grid of set & trap & year combinations
grid_STY <- unique(data.frame(Sets=Accelerometer_Survey$Sets,Traps=Accelerometer_Survey$Traps,Year=Accelerometer_Survey$Year))
grid_STY <- grid_STY[order(grid_STY$Sets,grid_STY$Traps,grid_STY$Year),]
head(grid_STY)

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

# data vecsum
data$VecSum <- sqrt(data$XAccel^2 + data$YAccel^2 + data$ZAccel^2)

# subset column names
data <- data[,c(names(Accelerometer_Survey),"Start_DateTime","End_DateTime")]

# order by sets and traps, datetime
data <- data[order(data$Sets,data$Traps,data$DateTime),]

# set time buffer 
buffer <- 2*60*60
buffer
rows <- buffer/5
rows
print(paste("Minutes = ",buffer/60,", Hours = ",buffer/(60*60),sep=""))

# Set Period
data$Period <- ifelse(data$DateTime < data$Start_DateTime + buffer,1,ifelse(data$DateTime > data$End_DateTime - buffer,3,2))

# # plot periods and timescale for first 5 set and trap combos
# for(st in seq(1,5,1)){

# 	Set <- grid_STY[st,"Sets"]
# 	Trap <- grid_STY[st,"Traps"]

# 	data_st <- data[data$Sets == Set & data$Traps == Trap,]
# 	Accelerometer_Survey_2013_st <- Accelerometer_Survey_2013[Accelerometer_Survey_2013$Sets == Set & Accelerometer_Survey_2013$Traps == Trap,]

# 	plot(Accelerometer_Survey_2013_st$DateTime,Accelerometer_Survey_2013_st$VecSum,type='l')
# 	lines(data_st$DateTime,data_st$VecSum,type='l',col='red')
# 	abline(v=data_st$Start_DateTime + buffer)
# 	abline(v=data_st$End_DateTime - buffer)
# 	dev.new()
# }

# graphics.off()

beep()

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
length(data[data$Period == 1,]$VecSum)/rows

# grid of set & trap combinations
grid_STY <- unique(data.frame(Sets=Accelerometer_Survey$Sets,Traps=Accelerometer_Survey$Traps,Year=Accelerometer_Survey$Year))

save.image("Accelerometer_Survey_2013_kmeans.RData")

# ------------------------------------------------------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ------------------------------------------------------------

setwd("/home/jim/Dropbox/REM/Tasks/accelerometer/DFO data request - August, 2014")
load("Accelerometer_Survey_2013_kmeans.RData")

data <- data[data$Period == 1 & data$x == rows & is.na(data$x) == FALSE,]
length(data[data$Period == 1,]$VecSum)/rows

# grid of set & trap combinations
grid_STY <- unique(data.frame(Sets=data$Sets,Traps=data$Traps,Year=data$Year))

# period 1

# ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
# spec

# calculate spec for each set, trap
specs <- vector()
for(st in seq(1,nrow(grid_STY),1)){

	Set <- grid_STY[st,"Sets"]
	Trap <- grid_STY[st,"Traps"]

	data_st <- data[data$Sets == Set & data$Traps == Trap & data$Period == 3,]

	spec <- spectrum(data_st$VecSum)$spec
	specs <- rbind(specs,spec)
}

specs <- as.data.frame(specs)
nrow(specs)

dist <- dist(specs,method = "euclidean",p=2)

fit <- kmeans(dist,centers=3)

specs$groups <- fit$groups

for(cl in unique(fit$groups)){

	specs_cl <- specs[specs$groups == cl,]

	quans_med <- function(x){as.numeric(quantile(x,probs=c(0.5)))}
	quans_upr <- function(x){as.numeric(quantile(x,probs=c(0.01)))}
	quans_lwr <- function(x){as.numeric(quantile(x,probs=c(0.99)))}

	x1 <- seq(1,ncol(specs_cl)-1,1)
	x2 <- seq(ncol(specs_cl)-1,1,-1)

	y1 <- apply((specs_cl[,seq(1,ncol(specs_cl)-1,1)]),MARGIN=2,FUN=quans_upr)
	y2 <- apply((specs_cl[,seq(1,ncol(specs_cl)-1,1)]),MARGIN=2,FUN=quans_lwr)

	plot(seq(1,ncol(specs_cl)-1,1),specs_cl[1,seq(1,ncol(specs_cl)-1,1)],type='l',main=paste(nrow(specs_cl)),ylim=range(0,0.02))
	lines(seq(1,ncol(specs_cl)-1,1),apply((specs_cl[,seq(1,ncol(specs_cl)-1,1)]),MARGIN=2,FUN=quans_med))
	
	# draw 95% credible region using polygon function. "rev" is reverse function.
	polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
	lines(seq(1,ncol(specs_cl)-1,1),apply((specs_cl[,seq(1,ncol(specs_cl)-1,1)]),MARGIN=2,FUN=quans_med))

	dev.new()
}

graphics.off()

# vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
# vecsum

x <- matrix(data[data$Period == 1,]$VecSum,ncol=rows,byrow=TRUE)
x <- as.data.frame(x)
length(data[data$Period == 1,]$VecSum)/rows

dist <- dist(x,method = "euclidean",p=2)
dist <- dist^2

# Ward Hierarchical Clustering
# fit <- hclust(dist, method="average") 
fit <- hclust(dist, method="ward.D") 

png("cluster_dendogram.png")
plot(fit) # display dendogram
groups <- cutree(fit, k=9) 
# draw dendogram with red borders around the 7 clusters 
rect.hclust(fit, k=9, border="red")
dev.off()

cl <- 1
for(cl in unique(groups)){

	r <- 1

	x_cl <- x[groups == cl,]

	quans_med <- function(x){as.numeric(quantile(x,probs=c(0.5)))}
	quans_upr <- function(x){as.numeric(quantile(x,probs=c(0.01)))}
	quans_lwr <- function(x){as.numeric(quantile(x,probs=c(0.99)))}

	x1 <- seq(Sys.time(),Sys.time() +  buffer, length=rows)
	x1 <- as.POSIXct(x1)
	x2 <- seq(Sys.time() + buffer , Sys.time(),length=rows)
	x2 <- as.POSIXct(x2)

	y1 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_upr)
	y2 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_lwr)

	plot(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',ylim=c(0.9,1.5),
		main=paste(nrow(x_cl),"\n",
			"Set = ",data_p[data_p$Period == 1,][r,"Sets"],", ",
			"Trap = ",data_p[data_p$Period == 1,][r,"Traps"],", ",
			"Year = ",data_p[data_p$Period == 1,][r,"Year"],sep=""),
		col='red')
			
	# draw 95% credible region using polygon function. "rev" is reverse function.
	polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
	# sample
	lines(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',main=paste(nrow(x_cl)),col='red')
	# median
	lines(x1,apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(0.9,1.5))

	# dev.new()

	ani.options(interval=4/nrow(x_cl),autobrowse=FALSE)
	
	animation <- function(){
		for(r in seq(1,nrow(x_cl),1)){

			plot(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',ylim=c(0.9,1.5),
				main=paste(nrow(x_cl),"\n",
					"Set = ",data_p[data_p$Period == 1,][r,"Sets"],", ",
					"Trap = ",data_p[data_p$Period == 1,][r,"Traps"],", ",
					"Year = ",data_p[data_p$Period == 1,][r,"Year"],sep=""),
				col='red')
					
			# draw 95% credible region using polygon function. "rev" is reverse function.
			polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
			# sample
			lines(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',main=paste(nrow(x_cl)),col='red')
			# median
			lines(x1,apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(0.9,1.5))
		}
	}

	saveGIF(animation(),movie.name=paste("clusterVS_",cl,".gif",sep=""))

}

graphics.off()

# ------------------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# x, y and z

# VecSum
x <- matrix(data_p[data_p$Period == 1,]$VecSum,ncol=rows,byrow=TRUE)
x <- as.data_p.frame(x)
length(data_p[data_p$Period == 1,]$VecSum)/rows
nrow(x)

# X
xX <- matrix(data_p[data_p$Period == 1,]$XAccel,ncol=rows,byrow=TRUE)
xX <- as.data_p.frame(xX)
length(data_p[data_p$Period == 1,]$VecSum)/rows

xY <- matrix(data_p[data_p$Period == 1,]$YAccel,ncol=rows,byrow=TRUE)
xY <- as.data_p.frame(xY)
length(data_p[data_p$Period == 1,]$VecSum)/rows

xZ <- matrix(data_p[data_p$Period == 1,]$ZAccel,ncol=rows,byrow=TRUE)
xZ <- as.data_p.frame(xZ)
length(data_p[data_p$Period == 1,]$VecSum)/rows

distX <- dist(xX,method = "euclidean")
distY <- dist(xY,method = "euclidean")
distZ <- dist(xZ,method = "euclidean")

dist <- distX^2 +  distY^2 + distZ^2
head(dist)

# Ward Hierarchical Clustering
fit <- hclust(dist, method="average") 
fit <- hclust(dist, method="ward.D") 

png("cluster_dendogram.png")
plot(fit) # display dendogram
groups <- cutree(fit, k=9) 
# draw dendogram with red borders around the 7 clusters 
rect.hclust(fit, k=9, border="red")
dev.off()

# -----------------------------------------------------------------------

cl <- 1
for(cl in unique(groups)){

	r <- 1

	x_cl <- x[groups == cl,]

	xX_cl <- xX[groups == cl,]
	xY_cl <- xY[groups == cl,]
	xZ_cl <- xZ[groups == cl,]


	quans_med <- function(x){as.numeric(quantile(x,probs=c(0.5)))}
	quans_upr <- function(x){as.numeric(quantile(x,probs=c(0.01)))}
	quans_lwr <- function(x){as.numeric(quantile(x,probs=c(0.99)))}

	x1 <- seq(Sys.time(),Sys.time() +  buffer, length=rows)
	x1 <- as.POSIXct(x1)
	x2 <- seq(Sys.time() + buffer , Sys.time(),length=rows)
	x2 <- as.POSIXct(x2)

	y1 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_upr)
	y2 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_lwr)

	plot(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',ylim=c(0.9,1.5),
		main=paste(nrow(x_cl),"\n",
			"Set = ",data_p[data_p$Period == 1,][r,"Sets"],", ",
			"Trap = ",data_p[data_p$Period == 1,][r,"Traps"],", ",
			"Year = ",data_p[data_p$Period == 1,][r,"Year"],sep=""),
		col='red')
			
	# draw 95% credible region using polygon function. "rev" is reverse function.
	polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
	# sample
	lines(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',main=paste(nrow(x_cl)),col='red')
	# median
	lines(x1,apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(0.9,1.5))

	# dev.new()

	ani.options(interval=4/nrow(x_cl),autobrowse=FALSE)

	animation <- function(){
		for(r in seq(1,nrow(x_cl),1)){

			plot(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',ylim=c(0.9,1.5),
			main=paste(nrow(x_cl),"\n",
				"Set = ",data_p[data_p$Period == 1,][r,"Sets"],", ",
				"Trap = ",data_p[data_p$Period == 1,][r,"Traps"],", ",
				"Year = ",data_p[data_p$Period == 1,][r,"Year"],sep=""),
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
				"Set = ",data_p[data_p$Period == 1,][r,"Sets"],", ",
				"Trap = ",data_p[data_p$Period == 1,][r,"Traps"],", ",
				"Year = ",data_p[data_p$Period == 1,][r,"Year"],sep=""),
			col='red')
				
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
				"Set = ",data_p[data_p$Period == 1,][r,"Sets"],", ",
				"Trap = ",data_p[data_p$Period == 1,][r,"Traps"],", ",
				"Year = ",data_p[data_p$Period == 1,][r,"Year"],sep=""),
			col='red')
				
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

			plot(x1,xZ_cl[r,seq(1,ncol(xZ_cl),1)],type='l',ylim=c(0.9,1.5),
			main=paste(nrow(xZ_cl),"\n",
				"Set = ",data_p[data_p$Period == 1,][r,"Sets"],", ",
				"Trap = ",data_p[data_p$Period == 1,][r,"Traps"],", ",
				"Year = ",data_p[data_p$Period == 1,][r,"Year"],sep=""),
			col='red')
				
			# draw 95% credible region using polygon function. "rev" is reverse function.
			polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
			# sample
			lines(x1,xZ_cl[r,seq(1,ncol(xZ_cl),1)],type='l',main=paste(nrow(xZ_cl)),col='red')
			# median
			lines(x1,apply((xZ_cl[,seq(1,ncol(xZ_cl),1)]),MARGIN=2,FUN=quans_med),ylim=c(0.9,1.5))
		}
	}
	saveGIF(animation(),movie.name=paste("cluster_",cl,".gif",sep=""))
	saveGIF(animationX(),movie.name=paste("clusterX_",cl,".gif",sep=""))
	saveGIF(animationY(),movie.name=paste("clusterY_",cl,".gif",sep=""))
	saveGIF(animationZ(),movie.name=paste("clusterZ_",cl,".gif",sep=""))
}

# -------------------------------------------------------------------

graphics.off()


# plot periods and timescale for first 5 set and trap combos
st <- 2
animation2 <- function(n=nrow(grid_STY)){

	ani.options(interval=30/n,autobrowse=FALSE)

	for(st in seq(1,nrow(grid_STY),length=n)){

		Set <- grid_STY[st,"Sets"]
		Trap <- grid_STY[st,"Traps"]
		Year <- grid_STY[st,"Year"]

		data_sty <- data[data$Sets == Set & data$Traps == Trap & data$Year == Year,]

		plot(data_sty$DateTime,data_sty$VecSum,type='l',col='red',ylim=c(0.9,1.5))
		abline(v=data_sty$Start_DateTime + buffer)
		abline(v=data_sty$End_DateTime - buffer)
		# dev.new()
	}
}

saveGIF(animation2())

system("ls")