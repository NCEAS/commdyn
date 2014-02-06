library(reshape2)
library(plyr)
library(stringr)

# BBS California observational data downloaded from 
# ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/States/Califor.zip
# on 2014-02-06

# AOU codes and other species data downloaded from 
# ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/SpeciesList.txt
# on 2014-02-16



# Deal with malformed species list ----------------------------------------

raw_species = readLines("workflows-and-analysis/BBS/SpeciesList.txt")
pasted_species = paste0(raw_species, collapse = "\n")

# Convert to UTF-8 and then save.
# The encoding wasn't actually latin1, it was something R couldn't handle.  But
# on my machine at least, iconv seemed to accept it.
write(
  iconv(gsub("\n([:alnum:])", "\1", pasted_species), from = "latin1", to = "UTF-8"), 
  file = "workflows-and-analysis/BBS/SpeciesList_fixed_lines.txt"
)

dashes = readLines("workflows-and-analysis/BBS/SpeciesList_fixed_lines.txt")[6]
species_colnames = strsplit(
  readLines("workflows-and-analysis/BBS/SpeciesList_fixed_lines.txt")[5],
  " +"
)[[1]]

species_list = read.fwf(
  "workflows-and-analysis/BBS/SpeciesList_fixed_lines.txt", 
  widths = nchar(strsplit(dashes, " ")[[1]]) + 1, 
  header = FALSE, 
  skip = 6,
  encoding = "UTF-8",
  stringsAsFactors = FALSE
)
colnames(species_list) = species_colnames
write.csv(species_list, "workflows-and-analysis/BBS/SpeciesList.csv")

# California --------------------------------------------------------------


dat = read.csv(
  "workflows-and-analysis/BBS/Califor.csv", 
  as.is = TRUE
)
dat$Aou = as.character(dat$Aou)
dat$latin_name = str_trim(
  species_list[match(dat$Aou, species_list$AOU), "Spanish_Common_Name"]
)

wide.data = dcast(
  data = dat, 
  formula = Year + Route ~ latin_name, 
  fill = 0,
  fun.aggregate = function(x) sum(x),
  value.var = "SpeciesTotal"
)

write.csv(
  wide.data, 
  file = "workflows-and-analysis/BBS/BBS_wide.csv"
)
