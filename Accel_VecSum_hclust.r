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
# install.packages("dendextend")

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
library(stats)
library(dendextend)

# set working directory
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
d <- ddply(Accelerometer_Survey,.(Sets,Traps,Year),summarize,diff_time=time_diff(DateTime),diff_length=length(DateTime)*5/(60*60))
d$discrepancy <- d$diff_time - d$diff_length
plot(d$discrepancy^2,ylim=c(0,2))
abline(h=0.5)

# read in sensor fishing events data, (used for start and end time for sets)
Sensor_Fishing_Events <- read.csv("Sensor_Fishing_Events.csv")
Sensor_Fishing_Events <- Sensor_Fishing_Events[order(Sensor_Fishing_Events$SETNO),]

# join two data sets
data <- sqldf("SELECT 
				Accelerometer_Survey.*
				,Sensor_Fishing_Events.*
				,d.discrepancy
				FROM Accelerometer_Survey
				LEFT JOIN Sensor_Fishing_Events
				ON Accelerometer_Survey.Sets = Sensor_Fishing_Events.SETNO
					AND Accelerometer_Survey.Year = Sensor_Fishing_Events.YEAR
				LEFT JOIN d
				ON d.Sets = Accelerometer_Survey.Sets
					AND d.Traps = Accelerometer_Survey.Traps
					AND d.Year = Accelerometer_Survey.Year
				;")

# elimate data sets with wacky time discrepancies
data <- data[is.na(data$discrepancy) == FALSE & data$discrepancy^2 < 0.5,]

# start time and end time
data$Start_DateTime <- as.POSIXct(paste(data$YEAR,"-",data$MON,"-",data$DAY," ",data$TIME,":00",sep=""),format="%Y-%m-%d %H:%M:%S")
data$End_DateTime <- data$Start_DateTime + data$DURATION*60
data$Mid_DateTime <- data$Start_DateTime + data$DURATION*60/2

# subset data based on start time and end time
data <- data[data$DateTime >= data$Start_DateTime & data$DateTime <= data$End_DateTime,]

# subset column names
data <- data[,c(names(Accelerometer_Survey),"discrepancy","Start_DateTime","End_DateTime","Mid_DateTime","DURATION")]

# subset by duration
data <- data[is.na(data$DURATION) == FALSE & data$DURATION/60 > 22,]

# minimum duration
min_time <- min(data$DURATION/60)
min_time

# set time buffer for period 1
buffer1 <- 0.75*60*60
buffer1
rows1 <- buffer1/5
rows1

# set time buffer for period 3
buffer3 <- (25/60)*60*60
buffer3
rows3 <- buffer3/5
rows3

# buffer and rows for soak period (middle 18 hours of 22 hrs)
buffer_mid <- floor((floor(min_time)*60*60 - 2*2*60*60)/2)
buffer_mid
rows_mid <- buffer_mid/5
rows_mid

# Set Period based on buffers
data$Period <- ifelse(data$DateTime < data$Start_DateTime + buffer1,1,
	ifelse(data$DateTime > data$End_DateTime - buffer3,3,
		ifelse(data$DateTime >= data$Mid_DateTime - buffer_mid & data$DateTime < data$Mid_DateTime + buffer_mid,2,
			NA)))


agg_STYP <- ddply(data,.(Sets,Traps,Year,Period),summarize,x=length(VecSum))
agg_STYP <- agg_STYP[is.na(agg_STP$Period) == FALSE,]

agg_STYP$agree <- ifelse((agg_STYP$Period == 1 & agg_STYP$x == rows1) | 
						(agg_STYP$Period == 3 & agg_STYP$x == rows3) | 
						(agg_STYP$Period == 2 & agg_STYP$x == 2*rows_mid)
						,TRUE,FALSE)

data <- data[,names(data)[!(names(data) %in% c("agree"))]]
data <- sqldf("SELECT 
				data.*
				,agg_STYP.agree
				FROM data
				LEFT JOIN agg_STYP
				ON agg_STYP.Sets = data.Sets
					AND agg_STYP.Traps = data.Traps
					AND agg_STYP.Year = data.Year
					AND agg_STYP.Period = data.Period
				;")

# eliminate sets where the rows aren't correct
data <- data[data$agree == TRUE & is.na(data$agree) == FALSE,]

# order by sets and traps, datetime
data <- data[order(data$Sets,data$Traps,data$DateTime),]

# na.omit

# grid of set & trap & year combinations
grid_STY <- unique(data.frame(Sets=data$Sets,Traps=data$Traps,Year=data$Year))
grid_STY <- na.omit(grid_STY)
grid_STY

length(data$Sets)
length(na.omit(data$Sets))

length(data$Traps)
length(na.omit(data$Traps))

length(data$Year)
length(na.omit(data$Year))

nrow(data_sty)
length(data_sty$VecSum)
length(na.omit(data_sty$VecSum))

length(VecSum_c)

sty <- 1
calibrate_V <- function(sty){

	Set <- grid_STY[sty,"Sets"]
	Trap <- grid_STY[sty,"Traps"]
	Year <- grid_STY[sty,"Year"]

	data_sty <- data[data$Sets == Set & data$Traps == Trap & data$Year == Year,]

	# smth <- smooth.spline(seq(1,nrow(data_sty),1),data_sty$VecSum,spar=10^(-50))
	# VecSum_l <- predict(smth,seq(1,nrow(data_sty),1),type='response')$y

	VecSum_c <- data_sty$VecSum - median(data_sty[data_sty$Period == 2 &  is.na(data_sty$VecSum) == FALSE,]$VecSum) + 1

	return(VecSum_c)
}

la_V <- lapply(seq(1,nrow(grid_STY),1),calibrate_V)

data$VecSum_c <- unlist(la_V)

# plot periods and timescale for first 5 set, trap and year combos
sty <- 1
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

	plot(Accelerometer_Survey_sty$DateTime,Accelerometer_Survey_sty$VecSum,type='l',col='red',ylim=c(0.9,1.5))
	lines(data_sty$DateTime,data_sty$VecSum,col='red')
	lines(data_sty$DateTime,data_sty$VecSum_c,col='black')
	abline(v=data_sty$Start_DateTime + buffer1)
	abline(v=data_sty$End_DateTime - buffer3)
	abline(v=data_sty$Mid_DateTime - buffer_mid)
	abline(v=data_sty$Mid_DateTime + buffer_mid)
	dev.new()

}

graphics.off()

system("rm Accelerometer_Survey_2013_kmeans.RData")
save.image("Accelerometer_Survey_2013_kmeans.RData")

beep()

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
library(stats)
library(dendextend)
library(dendroextras)

setwd("/home/jim/Dropbox/REM/Tasks/accelerometer/clustering")
load("Accelerometer_Survey_2013_kmeans.RData")

data <- data[is.na(data$Period) == FALSE,]
data <- data[order(data$Period,data$Sets,data$Traps,data$Year),]

grid_STYP <- unique(data.frame(Sets=data$Sets,Traps=data$Traps,Year=data$Year,Period=data$Period))
grid_STYP

# ------------------------------------------------------------
# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# VecSum

Period <- 2
groups <- vector()
for(Period in c(1,2,3)){

	data_p <- data[data$Period == Period &  is.na(data$Period) == FALSE,]

	if(Period == 1){
		rows <- rows1
		buffer <- buffer1
		k <- 20
		ylim <- c(0.9,1.2)
	} else if (Period == 2 ){
		rows <- 2*rows_mid
		buffer <- 2*buffer_mid
		k <- 15
		ylim <- c(0.95,1.05)
	} else if (Period == 3){
		rows <- rows3
		buffer <- buffer3
		k <- 15
		ylim <- c(0.9,1.2)
	}

	# VecSum
	x <- matrix(data_p$VecSum_c,ncol=rows,byrow=TRUE)
	tx <- t(x)
	for(c in seq(1,ncol(tx),1)){
		smth <- smooth.spline(seq(1,nrow(tx),1),tx[,c])
		tx[,c] <- predict(smth,seq(1,nrow(tx),1),type='response')$y
	}
	x <- t(tx)
	x <- data.frame(x)
	row.names(x) <-  paste("ST",formatC(seq(1,nrow(x),1),digits=2,flag="0"),sep="")

	if(Period == 2){
		x <- matrix(data_p$VecSum_c,ncol=rows,byrow=TRUE)
		tx <- t(x)
		tx_new <- matrix(NA,ncol=ncol(tx),nrow=1)
		for(c in seq(1,ncol(tx),1)){
			tx_new[,c] <- var(tx[,c])
		}
	x <- t(tx_new)
	x <- as.data.frame(x)
	row.names(x) <-  paste("ST",formatC(seq(1,nrow(x),1),digits=2,flag="0"),sep="")
	}

	# dist
	dist <- dist(x,method = "euclidean")

	# wardd Hierarchical Clustering
	# fit <- hclust(dist, method="average") 
	fit <- hclust(dist, method="ward.D2") 

	sl <- slice(fit,k=k)
	sl <- as.matrix(sl)
	sl <- as.data.frame(sl)
	sl$rn <- row.names(sl)
	sl <- sl[order(sl$rn),]

	png(paste("cluster_dendogram_p",Period,".png",sep=""),width=800,height=800)

	fit_col <- colour_clusters(hclust(dist(x,method='euclidean'), method="ward.D2") ,k=k,groupLabels=TRUE,col=rep('black',k))
	plot(fit_col,horiz=TRUE)
	rect.dendrogram(fit_col,horiz=TRUE , k=k, border="red")

	dev.off()

	# VecSum
	x <- matrix(data_p$VecSum_c,ncol=rows,byrow=TRUE)
	tx <- t(x)
	for(c in seq(1,ncol(tx),1)){
		smth <- smooth.spline(seq(1,nrow(tx),1),tx[,c])
		tx[,c] <- predict(smth,seq(1,nrow(tx),1),type='response')$y
	}
	x <- t(tx)
	x <- data.frame(x)
	row.names(x) <-  paste("ST",formatC(seq(1,nrow(x),1),digits=2,flag="0"),sep="")

	cl <- 1
	for(cl in unique(groups_p)){

		r <- 1

		x_cl <- x[groups_p == cl,]
		grid_STY_cl <- grid_STY[groups_p == cl,]

		quans_med <- function(x){as.numeric(quantile(x,probs=c(0.5)))}
		quans_upr <- function(x){as.numeric(quantile(x,probs=c(0.01)))}
		quans_lwr <- function(x){as.numeric(quantile(x,probs=c(0.99)))}

		x1 <- seq(Sys.time(),Sys.time() +  buffer, length=rows)
		x1 <- as.POSIXct(x1)
		x2 <- seq(Sys.time() + buffer , Sys.time(),length=rows)
		x2 <- as.POSIXct(x2)

		y1 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_upr)
		y2 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_lwr)

		png(paste("cluster_p",Period,"_",cl,".png",sep=""))
		plot(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',ylim=ylim,
			main=paste("Cluster = ",cl,"\n",
				"Number of members = ",nrow(x_cl),
				sep=""),
			col='red')
			# draw 95% credible region using polygon function. "rev" is reverse function.
		polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
		for(r in seq(1,nrow(x_cl),1)){
			# sample
			lines(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',main=paste(nrow(x_cl)),col='red',lty='dashed')
		}
		# median
		lines(x1,apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_med),lwd=2)
		dev.off()

		animationV <- function(){
			for(r in seq(1,nrow(x_cl),1)){

				plot(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',ylim=c(0.9,1.5),
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
				lines(x1,apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_med))

			}
		}

		ani.options(interval=4/nrow(x_cl),autobrowse=FALSE)
		# saveGIF(animationV(),movie.name=paste("cluster_p",Period,"_",cl,".gif",sep=""))

	}
	groups <- c(groups,groups_p)
	print(Period)
}

groups

grid_STYP$groups <- groups
grid_STYP <- grid_STYP[order(grid_STYP$Period,grid_STYP$groups,grid_STYP$Sets,grid_STYP$Traps,grid_STYP$Year),]
grid_STYP

data <- data[,names(data)[!(names(data) %in% c("groups"))]]
data <- sqldf("SELECT 
				data.*
				,grid_STYP.groups
				FROM data
				LEFT JOIN grid_STYP
				ON grid_STYP.Sets = data.Sets
					AND grid_STYP.Traps = data.Traps
					AND grid_STYP.Year = data.Year
					AND grid_STYP.Period = data.Period
				;")


write.csv(grid_STYP,"grid_STYP.csv")


styp_plot <- function(Set,Trap,Year,Period){
		data_styp <- data[data$Sets == Set & data$Traps == Trap & data$Year == Year & data$Period == Period,]

		plot(data_styp$DateTime,data_styp$VecSum_c,type='l')
}

styp_plot(52,14,2013,3)

Period <- 2
cl <- 9
plot_Period_cl <- function(Period,cl){

	data_p <- data[data$Period == Period &  is.na(data$Period) == FALSE,]

	if(Period == 1){
		rows <- rows1
		buffer <- buffer1
		k <- 20
		ylim <- c(0.9,1.2)
	} else if (Period == 2 ){
		rows <- 2*rows_mid
		buffer <- 2*buffer_mid
		k <- 15
		ylim <- c(0.95,1.05)
	} else if (Period == 3){
		rows <- rows3
		buffer <- buffer3
		k <- 15
		ylim <- c(0.9,1.2)
	}

	# VecSum
	x <- matrix(data_p$VecSum_c,ncol=rows,byrow=TRUE)
	tx <- t(x)
	for(c in seq(1,ncol(tx),1)){
		smth <- smooth.spline(seq(1,nrow(tx),1),tx[,c])
		tx[,c] <- predict(smth,seq(1,nrow(tx),1),type='response')$y
	}
	x <- t(tx)
	x <- data.frame(x)
	row.names(x) <- paste("ST",formatC(seq(1,nrow(x),1),digits=2,flag="0"),sep="")

	if(Period == 2){
		x <- matrix(data_p$VecSum_c,ncol=rows,byrow=TRUE)
		tx <- t(x)
		tx_new <- matrix(NA,ncol=ncol(tx),nrow=1)
		for(c in seq(1,ncol(tx),1)){
			tx_new[,c] <- var(tx[,c])
		}
	x <- t(tx_new)
	x <- as.data.frame(x)
	row.names(x) <- paste("ST",formatC(seq(1,nrow(x),1),digits=2,flag="0"),sep="")
	}

	# dist
	dist <- dist(x,method = "euclidean")

	fit <- hclust(dist, method="ward.D2") 

	sl <- slice(fit,k=k)

	sl <- as.matrix(sl)
	sl <- as.data.frame(sl)
	sl$rn <- row.names(sl)
	sl <- sl[order(sl$rn),]

	groups_p <- sl$V1

	# VecSum
	x <- matrix(data_p$VecSum_c,ncol=rows,byrow=TRUE)
	tx <- t(x)
	for(c in seq(1,ncol(tx),1)){
		smth <- smooth.spline(seq(1,nrow(tx),1),tx[,c])
		tx[,c] <- predict(smth,seq(1,nrow(tx),1),type='response')$y
	}
	x <- t(tx)
	x <- data.frame(x)
	row.names(x) <- paste("ST",formatC(seq(1,nrow(x),1),digits=2,flag="0"),sep="")


	x_cl <- x[groups_p == cl,]
	grid_STY_cl <- grid_STY[groups_p == cl,]	

		x1 <- seq(Sys.time(),Sys.time() +  buffer, length=rows)
		x1 <- as.POSIXct(x1)
		x2 <- seq(Sys.time() + buffer , Sys.time(),length=rows)
		x2 <- as.POSIXct(x2)

		y1 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_upr)
		y2 <- apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_lwr)

	par(mar=c(5,5,5,5))

	plot(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',ylim=ylim,
		main=paste("Cluster = ",cl,"\n",
			"Number of members = ",nrow(x_cl),
			sep=""),
		ylab=NA,
		yaxt='n',
		xlab='Time',
		col='red')
		# draw 95% credible region using polygon function. "rev" is reverse function.
	polygon( x=c(x1,x2), y=c(y1,rev(y2)), border=NA, col="gray", bg="gray"  )
	for(r in seq(1,nrow(x_cl),1)){
		# sample
			lines(x1,x_cl[r,seq(1,ncol(x_cl),1)],type='l',main=paste(nrow(x_cl)),col='red',lty='dashed')
	}
	# median
	lines(x1,apply((x_cl[,seq(1,ncol(x_cl),1)]),MARGIN=2,FUN=quans_med),lwd=2)

	axis(side = 4, tck = -.015, labels = NA)
	axis(side = 4, lwd = 0, line = -.4, las = 1)
	mtext(side = 4, "Acceration (g)", line = 3)
}

plot_dendro_Period <- function(Period){

	data_p <- data[data$Period == Period &  is.na(data$Period) == FALSE,]

	if(Period == 1){
		rows <- rows1
		buffer <- buffer1
		k <- 20
		ylim <- c(0.9,1.2)
	} else if (Period == 2 ){
		rows <- 2*rows_mid
		buffer <- 2*buffer_mid
		k <- 15
		ylim <- c(0.95,1.05)
	} else if (Period == 3){
		rows <- rows3
		buffer <- buffer3
		k <- 15
		ylim <- c(0.9,1.2)
	}

	# VecSum
	x <- matrix(data_p$VecSum_c,ncol=rows,byrow=TRUE)
	tx <- t(x)
	for(c in seq(1,ncol(tx),1)){
		smth <- smooth.spline(seq(1,nrow(tx),1),tx[,c])
		tx[,c] <- predict(smth,seq(1,nrow(tx),1),type='response')$y
	}
	x <- t(tx)
	x <- data.frame(x)
	row.names(x) <-  paste("ST",formatC(seq(1,nrow(x),1),digits=2,flag="0"),sep="")

	if(Period == 2){
		x <- matrix(data_p$VecSum_c,ncol=rows,byrow=TRUE)
		tx <- t(x)
		tx_new <- matrix(NA,ncol=ncol(tx),nrow=1)
		for(c in seq(1,ncol(tx),1)){
			tx_new[,c] <- var(tx[,c])
		}
	x <- t(tx_new)
	x <- as.data.frame(x)
	row.names(x) <- paste("ST",formatC(seq(1,nrow(x),1),digits=2,flag="0"),sep="")
	}

	# dist
	dist <- dist(x,method = "euclidean")

	# wardd Hierarchical Clustering
	# fit <- hclust(dist, method="average") 
	fit <- hclust(dist, method="ward.D2") 

	sl <- slice(fit,k=k)
	sl <- as.matrix(sl)
	sl <- as.data.frame(sl)
	sl$rn <- row.names(sl)
	sl <- sl[order(sl$rn),]

	groups_p <- sl$V1

	fit_col <- colour_clusters(as.dendrogram(fit) ,k=k,groupLabels=TRUE,col=rep('black',k))
	plot(fit_col,horiz=TRUE)
	rect.dendrogram(fit_col,horiz=TRUE , k=k, border="red")

}


slide1 <- function(){

	layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE))

	# ---------------------------
	# left hand side

	plot_dendro_Period(Period=1)

	# --------------------------
	# bottom left

	plot_Period_cl(Period=1,cl=16)

	# -------------------------
	# bottom right

	plot_Period_cl(Period=1,cl=9)

}

slide2 <- function(){

	layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE))

	# ---------------------------
	# left hand side

	plot_dendro_Period(Period=2)

	# --------------------------
	# bottom left

	plot_Period_cl(Period=2,cl=12)

	# -------------------------
	# bottom right

	plot_Period_cl(Period=2,cl=6)

}

slide3 <- function(){

	layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE))

	# ---------------------------
	# left hand side

	plot_dendro_Period(Period=3)

	# --------------------------
	# bottom left

	plot_Period_cl(Period=3,cl=14)

	# -------------------------
	# bottom right

	plot_Period_cl(Period=3,cl=8)

}

png("slide1.png",height=800,width=800)
slide1()
dev.off()

png("slide2.png",height=800,width=800)
slide2()
dev.off()

png("slide3.png",height=800,width=800)
slide3()
dev.off()