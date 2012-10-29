##########
# What: Utility functions for 2012 Peace Science time-series models poster
# Date: October 2012
#
# Copyright 2012 Andreas Beger
# CC BY-NC 3.0 license <http://creativecommons.org/licenses/by-nc/3.0/>
##########

## Plot realized variable along with density interval from which it was drawn. 
BandPlot <- function(x, band, ylim, ...) {
  x.max <- length(x)
  # Default ylim if option is missing
  if (missing(ylim)) {
    ylim <- c(0, 1.2*max(band))
  }
  plot(1, type="n", xlim=c(1, x.max), ylim=ylim, xlab="", ylab="", ...)
  polygon(c(1:x.max, x.max:1), c(band[, 1], rev(band[, 2])), density=NA, col="gray")
  lines(x, type="l")
}

##########
# DGP for NB(1) and PEWMA (simplified) processes.
#
##########

i1count <- function(obs_type, end, mu1, beta.proc, theta.obs) {
  require(coda)
  ## Number of draws for density intervals
  N <- 1000
  ## Validate obs_type
  obs_type_list <- list("poisson", "nbinom")
  if (missing(obs_type)) stop("No value for observation process distribution (obs_type)")
  if ((obs_type %in% obs_type_list)==FALSE) stop("obs_type must be one of (", paste(obs_type_list, collapse=" "), ")")
  ## Initialize empty results objects
  mu <- NULL
  y <- NULL
  mu.ci <- matrix(NA, nrow=end, ncol=2)
  y.ci <- matrix(NA, nrow=end, ncol=2)
  ## Latent mean for first time period
  mu[1] <- rgamma(1, mu1*beta.proc, rate=beta.proc)
  mu.ci[1,] <- HPDinterval(as.mcmc(rgamma(N, shape=mu[1]*beta.proc, rate=beta.proc)))
  ## Update transition equation for remaining time
  for (t in 2:end) {
    mu[t] <- rgamma(1, mu[t-1]*beta.proc, rate=beta.proc)
    mu.ci[t, ] <- HPDinterval(as.mcmc(rgamma(N, shape=mu[t-1]*beta.proc, rate=beta.proc)))
    # mean is mu[t-1], var = mu[t-1] * s.proc
  }
  ## Draw observed values based on measurement equation
  if (obs_type=="nbinom") {
    for (t in 1:end) {
      y[t] <- rnbinom(1, mu=mu[t], size=theta.obs)
      y.ci[t,] <- HPDinterval(as.mcmc(rnbinom(N, mu=mu[t], size=theta.obs)))
      # mean is mu[t], var = mu[t] + mu[t]^2/theta.obs
    }
  } 
  if (obs_type=="poisson") {
    for (t in 1:end) {
      y[t] <- rpois(1, mu[t])
      y.ci[t,] <- HPDinterval(as.mcmc(rpois(N, mu[t])))
    }
  }
  ## Combine and return results
  r <- cbind(c(1:end), mu, mu.ci, y, y.ci)
  colnames(r) <- list("t", "mu", "mu.lower", "mu.upper", "y", "y.lower", "y.upper")
  return(r)
}

# Plot "observed" and "estimate" values of y over "date".
plotFit <- function(observed, yhat, date, main=NULL) {
  require(ggplot2)
  colour <- I(hsv(0, 0.6, 0.6))
  p <- qplot(date, observed, geom="line", main=main, ylab="civilian deaths", xlab=NULL)
  p <- p + 
    geom_line(aes(x=date, y=yhat[, "median"]), colour=colour, alpha=0.8) +
    geom_ribbon(aes(x=date, ymin=yhat[, "p20"], ymax=yhat[, "p80"]), fill=colour, alpha=0.4) +
    theme(text = element_text(size=24), panel.grid.major=element_line(size=1),
          panel.grid.minor=element_line(size=0.75))
  return(p)
}

##########
# Summarize a matrix of posterior density samples with median and HPD interval.
# Returns 3 column data frame with optional row (coefficient) labels.
# params.list = T if params is a string describing related paramters, e.g. x[1], x[2],...
#
##########

summaryPD <- function(data, params, labels=NULL, params.list=FALSE) {
  require(coda)
  
  # Check input is mcmc object
  try(data <- as.mcmc(data), silent=TRUE)
  if (class(data)=="mcmc.list") {
    data <- as.mcmc(do.call(rbind, data))
  }
  if (class(data)!="mcmc") {
    stop("Data must be mcmc or related object")
  }
  
  # Extract and format parameter estimates
  if (params.list==FALSE) {
    data <- data[, params]
  }
  if (params.list==TRUE) {
    data <- data[, grep(params, colnames(data))]
  }
  
  # Calcuate summary stats and format
  CI <- HPDinterval(data, prob=0.8)
  attr(CI, "Probability") <- NULL
  CI <- cbind(apply(data, 2, function(x) {median(x)}), CI)
  CI <- data.frame(CI)
  colnames(CI) <- c("median", "p20", "p80")
  if (!is.null(labels)) {rownames(CI) <- labels}
  
  # Done
  return(CI)
}

##########
# Extract fitted values (i.e. large number of variables).
#
##########

# Create pointrange plot for mcmc coefficients
plotPD <- function(data, params, labels=NULL, main=NULL) {
  require(ggplot2)
  
  # Check input
  if (missing(params)) { stop("No parameter(s) provided") }
  
  # Get table of pd summary
  coef.table <- summaryPD(data, params, labels)
  print(coef.table)
  coef.table$x <- rownames(coef.table)
  
  # Order for plot
  if (is.null(labels)) { 
    xorder <- rev(coef.table$x)
  } else {
    xorder <- rev(labels)
  }
  
  # Plot coefficient pointranges
  p <- ggplot(coef.table, aes(x=x, y=median, ymin=p20, ymax=p80))
  p <- p + geom_pointrange(size=2) + 
    geom_hline(yintercept=0, lwd=2, colour=hsv(0, 0.6, 0.6), alpha=0.8) +
    xlim(xorder) + coord_flip() + theme_bw() +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
          text = element_text(size=36), panel.grid.major=element_line(size=1),
          panel.grid.minor=element_line(size=0.75))
  ## add title................., blank background
  
  # Done return plot
  return(p)
}