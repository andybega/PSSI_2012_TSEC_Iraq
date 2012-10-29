##########
# What: Clean up data and add variables
# Date: April 2012
# By:   Andreas Beger
#
# Creative Commons BY-NC-SA 3.0 License:
# Attribution, Non-Commercial, Share Alike
##########

# Import data
load(file="data/dead_by_province_month.Rdata")

# Aggregate to country-wide total
iraq.total <- aggregate(iraq[,"dead.max"], by=list(iraq$date), FUN=sum)
iraq.total$t <- iraq$t[1:length(unique(iraq$t))]
colnames(iraq.total) <- c("date", "deaths", "t")

# Subset Baghdad data
baghdad <- subset(iraq, province=="Baghdad")

# National elections
election <- rep(0, length(iraq.total$deaths))
election[baghdad$t %in% c("2005-01", "2005-10", "2005-12", "2010-03")] <- 1

# Ramadan
ramadan <- rep(0, length(iraq.total$deaths))
ramadan[baghdad$t %in% c("2003-10", "2003-11", "2004-10", "2004-11", "2005-10", 
                         "2005-11", "2006-09", "2006-10", "2007-09", "2007-10", 
                         "2008-09", "2009-08", "2009-09", "2010-08", "2010-09", 
                         "2011-08")] <- 1

# Dummy for the March/April invasion of Iraq
invasion <- c(1, 1, rep(0, length(iraq.total$deaths)-2))

# Major battles (1st & 2nd Fallujah, South Iraq offensive)
battles <- rep(0, length(iraq.total$deaths))
battles[baghdad$t %in% c('2004-04', '2004-11', '2004-12', '2008-03', '2008-04',
                         '2008-05')] <- 1

# Save workspace image
save.image(file="data/iraq.Rdata")