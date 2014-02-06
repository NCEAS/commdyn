library(reshape2)
library(plyr)

# DATA CITATION: Ecological Archives E090-118-D1
# S. K. Morgan Ernest, Thomas J. Valone, and James H. Brown. 2009. 
#   Long-term monitoring and experimental manipulation of a Chihuahuan Desert 
#   ecosystem near Portal, Arizona, USA. Ecology 90:1708.
#
# Metadata available from http://esapubs.org/archive/ecol/E090/118/metadata.htm

dat = read.csv(
  "workflows-and-analysis/portal-rodents/Portal_rodents_19772002.csv", 
  as.is = TRUE
)

# There's a real species called NA, so make sure that the NAs are actually "NA"
dat$species[is.na(dat$species)] = "NA"

# Blanks are the *real* missing values
dat$species[dat$species == ""] = NA

dat$time.step = paste(dat$yr, dat$mo, dat$dy, sep = "-")

wide.data = dcast(
  data = dat, 
  formula = time.step + plot ~ species, 
  fill = 0,
  fun.aggregate = length
)

write.csv(
  wide.data, 
  file = "workflows-and-analysis/portal-rodents/Portal_wide.csv"
)
