library(reshape2)
library(plyr)

dat = read.csv(
  "workflows-and-analysis/portal-rodents/Portal_rodents_19772002.csv", 
  as.is = TRUE
)
dat$time.step = paste(dat$yr, dat$mo, dat$dy, sep = "-")
dat$species[dat$species == ""] = NA

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
