#--------------------------------------------------------------------#
# sparky.r: Beta version code for producing Tufte's "sparklines".    #
# Author  : A.R. Kronlund                                            #
# Date Revised:                                                      #
# 19-Oct-06 - Proof of concept prototype for that Cox guy.           #
# 20-Oct-06 - Beta version 1, no warrantee supplied.                 #
#--------------------------------------------------------------------#

calcStats <- function( x )
{
  # Calculates summary statistics.
  result <- summary( x )
  result
}

calcXaxis <- function( x, labelPct=0.25, plotPct=0.25 )
{
  # Extends the range of the x-axis to left and right such
  # that the plot occupies plotPct of the range, and the label
  # occupies labelPct of the range.

  # Check to ensure the percentages are within [0,1].
  if ( labelPct < 0 & labelPct > 1 )
  {
    labelPct <- 0.25
    cat( "\ncalcXaxis: labelPct out of bounds - reset.\n" )
  }
  if ( plotPct  < 0 | plotPct  > 1 )
  {
    labelPct <- 0.25
    cat( "\ncalcXaxis: plotPct out of bounds - reset.\n" )
  }

  xLim <- range( x,na.rm=TRUE )
  xDiff <- xLim[2] - xLim[1]

  # Some stupid algebra.
  xRange <- xDiff / plotPct
  newLim <- xLim
  newLim[1] <- xLim[1] - labelPct*xRange
  newLim[2] <- xLim[2] + (1.0-labelPct-plotPct)*xRange
  newLim
}

panLab <- function( x, y, txt, ... )
{
  # Allows text to be placed in plot panel at 0<x<1, 0<y<1.
  usr <- par( "usr" )
  par( usr=c(0,1,0,1) )
  text( x, y, txt, ... )
  par( usr=usr )
  return( NULL )
}

plt.lines <- function( x, y, xLim,... )
{
  # Plot a line graph.
  plot( x,y, type="n", axes=F, xlab="", xlim=xLim, ylab="" )
  lines( x,y,... )
}

plt.spikes <- function( x, y, xLim,... )
{
  # Plot a high density "spike" graph.
  plot( x,y,type="n",axes=F, xlab="", xlim=xLim, ylab="" )
  lines( x,y,type="h",... )
}

plt.spark <- function( x, y, plotFun, stats, labelPct=0.15, plotPct=0.25,
                       statPos=NULL,... )
{
  # Plots sparklines plots as described by Edward Tufte.
  #
  # y       : matrix with one column for each sparkline.
  # plotFun : the plot function to call (user defined).
  # stats   : matrix with statistics for each sparkline.
  #           All statistics in the stats matrix are placed on the plot, so
  #           subset the columns of this matrix as appropriate.
  # labelPct: the percent of the plot x-axis used by the spark labels.
  # plotPct : the percent of the plot x-axis used by the spark plot.
  # statPos : vector holding [0,1] positions of summary stats (optional).
  #           If statPos is not supplied then the statistics are equally
  #           spaced in along the x-axis in the space leftover after the
  #           labels and the spark line plot.
  #
  # Note that plot parameters such as "col" and "lwd" or "lty" can be passed
  # because of the "..." notation in the function header.

  # Number of sparklines and maximum data points each sparkline.
  nSpark <- ncol( y )
  nPts   <- nrow( y )

  # Extract the spark category names from the data, or build them.
  dataNames <- dimnames( y )
  if ( is.null( dataNames[[2]] ) )
    sparkNames <- paste( "Category",c(1:nSpark) )
  else
    sparkNames <- dataNames[[2]]

  # Get the statistics labels.
  statNames <- dimnames( stats )
  if ( is.null( statNames[[2]] ) )
    statNames <- paste( "Stat",c(1:ncol(stats)) )
  else
    statNames <- statNames[[2]]

  # Compute the positions of the statistics.
  statPct <- 1.0 - labelPct - plotPct
  statPos <- seq( (labelPct+plotPct),1,statPct/length(statNames) )
  offset  <- diff(statPos)/2.0
  statPos <- statPos[1:(length(statPos)-1)] + offset

  # Configure graphics panels to have nSpark rows and one column.
  # An additional panel is required for the header.

  par( oma=c(1,1,1,1), mar=c(0,0,0,0), mfrow=c(nSpark+1,1) )

  # Plot the header graphics panel.
  plot( c(0,1),c(0,1), type="n", axes=F, xlab="", ylab="" )

  # Output the header.
  panLab( labelPct/2,0.5,"Category" )
  for ( k in 1:ncol(stats) )
    panLab( statPos[k],0.5, adj=0, statNames[k] )

  # Loop over the sparklines.
  for ( j in 1:nSpark )
  {
    # Extend the x-axis to allow the spark plot to occupy
    # "plotPct" of the range, and the labels "labelPct".

    xLim <- calcXaxis( x,labelPct,plotPct )

    # Plot the sparkline.  User controls what plot is used by
    # passing a function name plotFun= as an argument to plt.spark.

    plotFun( x,y[,j],xLim,...)

    # Sparkline main row labels.
    panLab( 0.01, 0.5, adj=0, sparkNames[j] )

    # Make summary text of stats.
    for ( k in 1:ncol(stats) )
      panLab( statPos[k],0.5, adj=0, round(stats[j,k],3 ) )
  }
  par( mfrow=c(1,1) )
}

#-------------------------------------------------------------------#

# Generate test data set.
nData <- 1000
sparkData <- matrix( rnorm( nData, mean=1, sd=1 ), nrow=nData/20, ncol=20 )
dimnames( sparkData ) <- list( NULL,paste( "Spark",c(1:ncol(sparkData)) ) )

# Calculate summary statistics make columns the spark categories.
sparkStats <- t( apply( sparkData,2,calcStats ) )

# Plot some examples.

plt.spark( x=c(1:nrow(sparkData)),y=sparkData,plt.lines,
           sparkStats[,c("Min.","Mean","Max.")],
           col="red", lwd=2,
           labelPct=0.1, plotPct=0.3 )

cat( "\nPress Enter to continue\n" )
scan()

plt.spark( x=c(1:nrow(sparkData)),y=sparkData,plt.spikes,
           sparkStats[,c("Min.","Median","Mean","Max.")],
           col="blue", plotPct=0.35 )

cat( "\nPress Enter to continue\n" )
scan()

plt.spark( x=c(1:nrow(sparkData)),y=sparkData,plt.lines,
           sparkStats[,c("Min.","Mean","Max.")],
           cex=2.0, col="green", lty=3, lwd=2,
           labelPct=0.1, plotPct=0.3 )
