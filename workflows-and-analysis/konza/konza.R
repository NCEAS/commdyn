
# Konza Grassland data:

library(reshape2)

# - Accessing raw data:
# 
# Go to http://www.konza.ksu.edu/knz/pages/data
# Search for PVC02
# Download data (maximum date ranges)

konza <- read.csv("PVC021.dat", na.strings=0, stringsAsFactors = FALSE)

# Subset 
konza <- subset(konza, WATERSHED=="001d")
konza <- subset(konza, SOILTYPE %in% c("f", "t"))


## Keep only the columns we care about 
data.cols <- grep("[A-D][1-9]", colnames(konza))
species.dates <- which(colnames(konza) %in% c("RECYEAR", "RECMONTH", "RECDAY", "SOILTYPE", "AB_GENUS", "AB_SPECIES", "AB_VARIETY"))
dat <- konza[c(data.cols, species.dates)]


# - Combine Species names to single column. (Variety not really necessary)


scientific.name = paste(dat$AB_GENUS, dat$AB_SPECIES, dat$AB_VARIETY, sep=" ")
keepme <- !(colnames(dat) %in% c("AB_GENUS", "AB_SPECIES", "AB_VARIETY"))
dat <- dat[keepme]
dat$scientific.name <- scientific.name


# Combine dates into a single column, format as date class
date <- as.Date(paste(dat$RECYEAR, dat$RECMONTH, dat$RECDAY, sep="-"))
keepme <- !(colnames(dat) %in% c("RECYEAR", "RECMONTH", "RECDAY"))
dat <- dat[keepme]
dat$date <- date


# translate abundance codes: code corresponds the midpoint of the percent cover range.  
abundance_codes <- 
  c("0" = 0,
    "1" = 0.5,
    "2" = 3,
    "3" = 17.5,
    "4" = 37.5,
    "5" = 62.5,
    "6" = 85,
    "7" = 98.5)

## Idetnify the columns that need to be remapped
data.cols <- grep("[A-D][1-9]", colnames(dat))


## Now use the code lookup table to translate the abundance codes to percent covers
M <- data.frame(sapply(dat[data.cols], function(x){
  x <- as.character(x)
  x[is.na(x)] <- "0"
  x[x==""] <- "0"
  abundance_codes[x]
}))
dat[data.cols] <- M # stick the transformed columns back




## Reshape the data into long format: site should be a variable
A <- melt(dat, id = names(dat)[-data.cols])

B <- dcast(A, ... ~ scientific.name, value.var="value", fill = 0, fun.aggregate=max)


dat.obs <- aggregate_by_time(B,
                  unit = "year",
                  fun.aggregate = max,
                  species.columns = which(sapply(B, is, "numeric")), 
                  date.column = which(sapply(B, is, "Date")))

