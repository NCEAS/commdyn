library(reshape2)
library(plyr)

# BBS data downloaded from 
# ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/Califor.zip
# on 2014-02-06
dat = read.csv(
  "workflows-and-analysis/BBS/Califor.csv", 
  as.is = TRUE
)

wide.data = dcast(
  data = dat, 
  formula = Year + Route ~ Aou, 
  fill = 0,
  fun.aggregate = function(x) sum(x),
  value.var = "SpeciesTotal"
)

write.csv(
  wide.data, 
  file = "workflows-and-analysis/BBS/BBS_wide.csv"
)
