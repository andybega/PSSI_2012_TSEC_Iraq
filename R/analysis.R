##########
# What: Code for 2012 peace science poster
# Date: October 2012
# Who:  Andreas Beger
#
# Copyright 2012 Andreas Beger
# CC BY-NC 3.0 license <http://creativecommons.org/licenses/by-nc/3.0/>
##########

library(plyr)
library(sp)
library(classInt)
library(shape)
library(RColorBrewer)
library(R2jags)

source('R/functions.R')

setwd('~/Research/NB1_model/2012 PSSI Iraq/')
load('data/iraq.RData')

############
# Map of dead by province
#
############

# Collapse to province
by.province <- ddply(iraq, .(province), summarise,
                     N = length(dead.min),
                     dead.min = sum(dead.min),
                     dead.max = sum(dead.max),
                     dead.avg = sum(dead.min, dead.max)/2,
                     records = sum(records))

by.province$NAME_1 <- c('Al-Anbar', 'Arbil', 'Babil', 'Baghdad', 'Al-Basrah', 
                        'Dihok', 'Dhi-Qar', 'Diyala', 'Karbala\'', 'At-Ta\'mim',  
                        'Maysan', 'Al-Muthannia', 'An-Najaf', 'Ninawa', 
                        'Al-Qadisiyah', 'Sala ad-Din', 'As-Sulaymaniyah', 
                        'Wasit')
by.province$pop97 <- c(1023736, 1095992, 1181751, 5423964, 1556445, 
                       402970, 1184796, 1135223, 594235, 753171,
                       637126, 436825, 775042, 2042852, 
                       751331, 904432, 1362739, 
                       783614)
by.province$dead.avg.pc <- with(by.province, dead.avg/pop97) * 10000

# Load administrative layers
for (i in 1:3) {
  load(paste('data/admin_boundaries/IRQ_adm', (i-1), '.RData', sep=''))
  assign(paste('iraq', (i-1), sep='.'), gadm)
}

# Merge to province spatial object
iraq.1@data <- data.frame(iraq.1@data, by.province[match(iraq.1@data[, 'NAME_1'], by.province[, 'NAME_1']), ])

iraqMap <- function() {
  ## For per capita deaths
  # Color parameters
  breaks <- c(0, 10, 25, 50, 100)
  maxy <- ceiling(max(iraq.1@data$dead.avg.pc))
  # Create colors
  colorpal <- brewer.pal(9, 'Reds')
  colorpal <- colorRampPalette(colorpal)
  colors <- colorpal(maxy)[ceiling(iraq.1@data$dead.avg.pc)]

  ## For number of records
#   breaks <- c(0, 1, 2, 3, 4, 5)
#   maxy <- ceiling(max(log10(iraq.1@data$records)))
#   # Create colors
#   colorpal <- brewer.pal(9, 'Reds')
#   colorpal <- colorRampPalette(colorpal)
#   colors <- colorpal(maxy)[ceiling(log10(iraq.1@data$records))]
  
  # Plot
  plot(iraq.1, col=colors, border='grey')
  plot(iraq.0, col=NULL, border='grey', add=T)
  colorlegend(posy = c(0.05,0.9), posx = c(0.9,0.92),
              col = colorpal(maxy),
              zlim=c(0, maxy), zval = breaks,
              main = 'dead /10,000',
              cex = 4,
              main.cex = 5)
  par(bg='white')
}

png('graphics/map_dead.png', height=2048, width=2400)
iraqMap()
dev.off()

############
# Plot of monthly deaths
#
############

smooth <- HoltWinters(ts(iraq.total$deaths), alpha=0.6, beta=FALSE, gamma=FALSE)
iraq.total$smooth <- c(NA, as.numeric(smooth$fitted[, 1]))
iraq.total$baghdad <- baghdad$dead.max
iraq.total$roiraq <- iraq.total$deaths - iraq.total$baghdad

plotMonthly <- function() {
  p <- ggplot(data=iraq.total)
  p <- p + 
    geom_line(aes(x=date, y=deaths), data=iraq.total, size=1.5) +
    geom_line(aes(x=date, y=baghdad), data=iraq.total, colour=hsv(0, 0.6, 0.6), size=1.5) +
    geom_line(aes(x=date, y=roiraq), data=iraq.total, colour=hsv(0.6, 0.6, 0.6), size=1.5) +
    theme(text = element_text(size=24), panel.grid.major=element_line(size=1),
          panel.grid.minor=element_line(size=0.75))
  print(p)
}

png("graphics/deaths_iraq_total.png", width=2048, height=1024)
plotMonthly()
dev.off()

############
# NB 1 model code
#
############

# Setup for JAGS
N <- length(iraq.total$deaths)
data <- list('N'=length(iraq.total$deaths), 'deaths'=iraq.total$deaths, 'election'=election, 
             'ramadan'=ramadan, 'invasion'=invasion, 'battles'=battles, 'mu1'=500)
model.params <- c('beta', 'inv.theta', 'd[1]', 'd[2]', 'd[3]', 'd[4]')

set.seed(123455)

# Call JAGS for covariates
model.cov <- jags(data=data, inits=NULL, parameters.to.save=model.params, 
                  model.file="JAGS/nb-gamma.txt", 
                  n.chains=3, n.iter=50000, n.burnin=500, n.thin=25, jags.seed=1234
                  )

# Convergence diagnostics
#traceplot(model.cov)
model.cov

# Coefficients
coefs <- summaryPD(model.cov, model.params)
png('graphics/iraq_coefs.png', width=1024, height=1536)
plotPD(model.cov, model.params, labels=c('beta', '1/theta', 'invasion', 'election',
                                         'ramadan', 'battles'))
dev.off()

# Call JAGS for model fit
model.fit <- jags(data=data, inits=NULL, parameters.to.save='yhat', 
                  model.file="JAGS/nb-gamma.txt", 
                  n.chains=1, n.iter=50000, n.burnin=1000, jags.seed=1234
                  )

# Plot fitted values
yhat <- summaryPD(model.fit, 'yhat', params.list=T)
png('graphics/iraq_fitted.png', width=2048, height=1024)
plotFit(iraq.total$deaths, yhat, iraq.total$date, main=NULL)
dev.off()

############
# Simulate data with estimated model parameters
#
############

# Higher starting mu or it sticks at 0
test.data <- i1count(obs_type='nbinom', end=108, mu1=1000, beta.proc=0.027, theta.obs=3.447)
# Plot y
BandPlot(test.data[, 5], test.data[, 6:7], main="Y", ylim=c(0,2000))
# Plot mu
BandPlot(test.data[, 2], test.data[, 3:4], main="Mu", ylim=c(0,2000))
