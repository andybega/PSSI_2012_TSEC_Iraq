##########
# What: APSA time-series models
# Date: August 2012
#
# Copyright 2012 Andreas Beger
# GNU GPL 3.0 <http://www.gnu.org/licenses/>
# Please reference Beger, Andreas, 2012, "Event count time series model", 
# <http://andybeger.wordpress.com>
##########

setwd("~/Research/NB1_model")

library(foreign)
library(R2jags)

## Source functions
source("R/functions.R")

##########
# Introduction to time-series terminology
#
##########

## Trend with noise
png("graphics/TE.png", width = 480, height = 360)
plot(TSdata(1,0.1,1e+10,proc_error=0,obs_error=1), 
     main="Trend and obs. error", ylab="y")
dev.off()

## Trend with seasonality
png("graphics/TS.png", width = 480, height = 360)
plot(TSdata(1,0.1,12,0,0),
     main="Seasonal", ylab="y")
dev.off()

## Trend with seasonality and error
png("graphics/TSE.png", width = 480, height = 360)
plot(TSdata(1,0.1,12,0,0.5),
     main="Trend, seasonal, and obs. error", ylab="y")
dev.off()

## Random walk
png("graphics/TSE.png", width = 480, height = 360)
set.seed(11123459)
plot(TSdata(1,0,1e+10,1,0),
     main="Random walk (transition error)", ylab="y")
dev.off()

## Everything
png("graphics/RWTSE.png", width = 480, height = 360)
set.seed(1129383)
plot(TSdata(1,0.1,12,0.5,0.5),
     main="Random walk with trend, seasonality, and obs. error", ylab="y")
dev.off()

##########
# Examples to illustrate difference between NB(1) and PEWMA (simplified) processes.
#
##########

## Initial set of graphics to introduce their format
png("graphics/gamma-poisson.png", width=640, height=320)
par(mfrow=c(1,2))
set.seed(123)
data <- i1count(obs_type="poisson",end=50, mu1=10, beta.proc=1)
BandPlot(data[, 2], data[, 3:4], main="Mu (beta=1)", ylim=c(0,60))
BandPlot(data[, 5], data[, 6:7], main="Y", ylim=c(0,60))
dev.off()

## Changing the process variance in gamma-poisson
png("graphics/gamma-poisson_s.png", width=640, height=640)
par(mfrow=c(2,2))
set.seed(123)
data <- i1count(obs_type="poisson",end=50, mu1=10, beta.proc=5)
BandPlot(data[, 2], data[, 3:4], main="Mu (beta=5)", ylim=c(0,60))
BandPlot(data[, 5], data[, 6:7], main="Y", ylim=c(0,60))
set.seed(123)
data <- i1count(obs_type="poisson",end=50, mu1=10, beta.proc=1)
BandPlot(data[, 2], data[, 3:4], main="Mu (beta=1)", ylim=c(0,60))
BandPlot(data[, 5], data[, 6:7], main="Y", ylim=c(0,60))
dev.off()

## Changing the measurement variance in gamma-nb
png("graphics/gamma-nb_z.png", width=640, height=640)
par(mfrow=c(2,2))
set.seed(123)
data <- i1count(obs_type="nbinom",end=50, mu1=10, beta.proc=5, theta.obs=100)
BandPlot(data[, 2], data[, 3:4], main="Mu (beta=5)", ylim=c(0,60))
BandPlot(data[, 5], data[, 6:7], main="Y (theta=100)", ylim=c(0,60))
set.seed(123)
data <- i1count(obs_type="nbinom",end=50, mu1=10, beta.proc=5, theta.obs=1)
BandPlot(data[, 2], data[, 3:4], main="Mu (beta=5)", ylim=c(0,60))
BandPlot(data[, 5], data[, 6:7], main="Y (theta=1)", ylim=c(0,60))
dev.off()

## Changing the transition variance in gamma-nb
png("graphics/gamma-nb_s.png", width=640, height=640)
par(mfrow=c(2,2))
set.seed(123)
data <- i1count(obs_type="nbinom",end=50, mu1=10, beta.proc=5, theta.obs=100)
BandPlot(data[, 2], data[, 3:4], main="Mu (beta=5)", ylim=c(0,60))
BandPlot(data[, 5], data[, 6:7], main="Y (theta=100)", ylim=c(0,60))
set.seed(123)
data <- i1count(obs_type="nbinom",end=50, mu1=10, beta.proc=1, theta.obs=100)
BandPlot(data[, 2], data[, 3:4], main="Mu (beta=1)", ylim=c(0,60))
BandPlot(data[, 5], data[, 6:7], main="Y (theta=100)", ylim=c(0,60))
dev.off()

## Examples of all four combinations
png("graphics/combo4.png", width=640, height=640)
par(mfrow=c(2,2))
set.seed(12115)
data <- i1count(obs_type="nbinom",end=50, mu1=10, beta.proc=5, theta.obs=100)
BandPlot(data[, 5], data[, 6:7], main="Y (beta=5, theta=100)", ylim=c(0,60))
set.seed(12115)
data <- i1count(obs_type="nbinom",end=50, mu1=10, beta.proc=5, theta.obs=10)
BandPlot(data[, 5], data[, 6:7], main="Y (beta=5, theta=10)", ylim=c(0,60))
set.seed(11221)
data <- i1count(obs_type="nbinom",end=50, mu1=10, beta.proc=1, theta.obs=100)
BandPlot(data[, 5], data[, 6:7], main="Y (beta=1, theta=100)", ylim=c(0,60))
set.seed(11221)
data <- i1count(obs_type="nbinom",end=50, mu1=10, beta.proc=1, theta.obs=1)
BandPlot(data[, 5], data[, 6:7], main="Y (beta=1, theta=1)", ylim=c(0,60))
dev.off()

##########
# Real world data examples.
#
##########

library(foreign)

guat <- read.table("inputData/JunkMonthlyGuatData.txt", header = TRUE)

## Uganda from UCDP
ucdp <- read.dta("inputData/UCDP GED v.1.0-2011.dta")
uganda <- subset(ucdp, country=="Uganda", select=c("date_start", "civilian_deaths"))
rm(ucdp)


uganda$month <- as.integer(paste(substr(uganda$date_start,1,4), substr(uganda$date_start,6,7), sep=""))
uganda <- uganda[,c("month", "civilian_deaths")]
uganda <- aggregate(uganda, by=list(uganda$month), sum)
plot(uganda$civilian_deaths, ylab="", xlab="", type="l")

##########
# nb-gamma model, verify against dgp with known parameters
#
##########

# Generate data
set.seed(123)
data <- i1count(obs_type="nbinom",end=100, mu1=100, beta.proc=5, theta.obs=10)
y <- data[, "y"]

# Call JAGS
nb_gamma <- jags(
  data=list(N=length(y), y=y, mu1=100),
  inits=NULL,
  parameters.to.save=c("beta", "theta"),
  model.file="JAGS/nb-gamma.txt",
  n.chains=3,
  n.iter=5000,
  n.burnin=500,
  jags.seed=1234
  )

# beta and theta estimates should match those in i1count call above
nb_gamma

##########
# nb-gamma model for Guatemala
#
##########

# Get data
guat <- read.table("inputData/JunkMonthlyGuatData.txt", header = TRUE)
mu1 <- 1
set.seed(1235435)

# Call JAGS
guat.press <- jags(
  data=list(N=length(guat$Press), Press=guat$Press, mu1=mu1),
  inits=NULL,
  parameters.to.save=c("beta", "theta"),
  model.file="JAGS/nb-gamma_guat.txt",
  n.chains=3,
  n.iter=5000,
  n.burnin=500,
  jags.seed=1234
)