##########
# What: Aggregate the IBC event data to province month level and add variables.
# Date: March 2012
# By:   Andreas Beger
#
# Creative Commons BY-NC-SA 3.0 License:
# Attribution, Non-Commercial, Share Alike
##########

#
# Functions
#

# Aggregate to months
#   input: events data, province char
#   output: data frame of deaths by month
AggMonth <- function(province, data) {
  data <- subset(data, Province==province)
  min <- by(data[, "Reported.Minimum"], data$t, sum)
  max <- by(data[, "Reported.Maximum"], data$t, sum)
  records <- by(data[, 'Reported.Minimum'], data$t, length)
  dead <- data.frame(as.vector(rep(province, length(min))), 
                     rownames(min), 
                     as.vector(min), 
                     as.vector(max, "integer"),
                     as.vector(records, 'integer'),
                     stringsAsFactors = FALSE)
  colnames(dead) <- c("province", "t", "dead.min", "dead.max", 'records')
  return(dead)
}

#
# Transform the data to province-month
#

## Process the deaths data

# Read raw data
events <- read.table("data/IBC_event.csv", 
                     header = TRUE, sep = ",", stringsAsFactor = TRUE)

# Drop if location was not coded
events <- events[ ! is.na(events$Province), ]
rownames(events) <- events$row.names

# Fix date for d4777
events$Start.Date[events$IBC.code=="d4777"] <- "12 Jan 2008"

# Reformat into POSIX date and extract t (year-month)
events$date <- strptime(events$Start.Date, "%d %b %Y")
events$t <- substr(events$date, 1, 7)

## Aggregate deaths data to province-month

# List of provinces to loop over
prov.list <- levels(events$Province)

# Loop over provinces and aggregate to monthly data
temp <- lapply(prov.list, function(x) { AggMonth(x, events) })
iraq <- do.call(rbind, temp)
rm(temp)

## Create a blank province-month set
panel <- NULL
for (p in prov.list) {
  for (y in 2003:2012) {
    for (m in 1:12) {
      if ((y==2012) & (m > 2)) { break }
      m <- formatC(m, width = 2, flag = "0")
      panel <- rbind(panel, cbind(p, paste(y, m, sep = "-"))) 
    }
  }
}
rm(m, p, y)
colnames(panel) <- c("province", "t")

# Join with blank panel date template
iraq <- merge(iraq, panel, by = c("province", "t"), all.y = TRUE)
rm(panel)
iraq[is.na(iraq)] <- 0

# Clean up variables
iraq$province <- as.factor(iraq$province)
remove <- c("2003-01", "2003-02")
iraq <- iraq[iraq$t != remove, ]
rm(remove)
# Add ISO date
iraq$date <- ISOdate(paste(substr(iraq$t, 1, 4)), paste(substr(iraq$t, 6, 7)), 1)

# Save data
save(iraq, file="data/dead_by_province_month.Rdata")

