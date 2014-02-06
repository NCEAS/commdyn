# TODO: Look into better ways of dealing with major errors (negative periods)?

library(reshape2)
library(plyr)

# DATA CITATION: Ecological Archives E090-118-D1
# S. K. Morgan Ernest, Thomas J. Valone, and James H. Brown. 2009. 
#   Long-term monitoring and experimental manipulation of a Chihuahuan Desert 
#   ecosystem near Portal, Arizona, USA. Ecology 90:1708.
#
# Metadata available from http://esapubs.org/archive/ecol/E090/118/metadata.htm
#
# Data from 2002 to 2013 added by Erica Christensen

dat = read.csv(
  "workflows-and-analysis/portal-rodents/Portal_rodents_19772013.csv", 
  as.is = TRUE
)

# Remove blank rows
dat = dat[!(is.na(dat$mo) | is.na(dat$plot)), ]

# Create a new note1 code for entries with no note
dat$note1[is.na(dat$note1)] = 0

# There's a real species called NA, so make sure that the NAs are actually "NA"
dat$species[is.na(dat$species)] = "NA"


# Manage based on note1 ---------------------------------------------------

# 2: trapped but no animals present (treat as Empty)
dat$Empty = dat$note1 == 2

# 4: plot not trapped (remove)
dat = dat[dat$note1 != 4, ]

# 13: non-target animal (remove)
dat = dat[dat$note1 != 13, ]

# Erroneous data has negative sampling period and is just removed for now
dat = dat[dat$period > 0, ]

# Record 54411 has a missing species but was not empty.  Remove it.
dat = dat[dat$Record_ID != 54411, ]

# A few escapees were not identified to species and should be listed as 
# unknown rodent (UR)
dat[dat$species == "" & !dat$Empty & dat$note5 == "E", "species"] = "UR"


# Assert that rows with blank species and "Empty" observations are co-extensive
stopifnot(
  all.equal(
    which(dat$species == ""),
    which(dat$Empty)
  )
)

# Rename "" to "blank"
dat$species[dat$species == ""] = "blank"

# Find (supposedly) Empty plots that occur more than once
# (possibly indicating non-emptiness)
sampling.ids = apply(
  dat, 
  1, 
  function(x){paste(x["date"],  x["plot"], sep = "_")}
)
duplicates = sapply(
  unique(sampling.ids[dat$Empty]), 
  function(x){sum(sampling.ids == x) > 1}
)

# Record 31750 is a duplicate of 31749
dat = dat[dat$Record_ID != 31749, ]

# Dates -------------------------------------------------------------------

dat$date = paste(dat$yr, dat$mo, dat$dy, sep = "-")

# There is not April 31 or September 21
dat$date = gsub("4-31-00", "5-1-00", dat$date)
dat$date = gsub("9-31-00", "10-1-00", dat$date)

wide.data = dcast(
  data = dat, 
  formula = period + date + plot ~ species, 
  fill = 0,
  fun.aggregate = length
)

# "blank" isn't a real species, it's just a placeholder
wide.data$blank = NULL

write.csv(
  wide.data, 
  file = "workflows-and-analysis/portal-rodents/Portal_wide.csv"
)
