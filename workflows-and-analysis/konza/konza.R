# Group 1.5
# =========
# 
# Group 1: Data set transformation to allow compute of many metrics
# 
#     Who: Erica Carl Scott Eric Sydney  Julie
# 
# Group 2: Time series analysis of community level metrics (consider higher freq data too)
# 
#     Who: Elizabeth Lauren Carl2 Seekell Julie Andrew
# 
# Datasets
# --------
# 
# - ARTHROPODS - CAP
# - KONZA Grassland
# - PORTAL small mammal
# - FISH - Corinna
# - HUDSON River fisher
# - Diatoms / Fish / Inverts 
# - Stream inverts
# 
# 
# Konza Grassland data:
# 
# - Accessing raw data:
# 
# Go to http://www.konza.ksu.edu/knz/pages/data
# Search for PVC02
# Download data (maximum date ranges)

konza <- read.csv("~/Downloads/PVC021.dat", na.strings=0)


column.codes <- colnames(konza)

column.definitions <- c("Data code (factor), same for all data in this dataset", 
                        RECTYPE = "",
                        RECYEAR = "Year data recorded (YYYY)", 
                        RECYEAR1 = "Two digit version of year recorded (YY)",
                        RECMONTH = "Month data recorded (M)",
                        RECDAY = "Day data recorded (D)",
                        WATERSHED = "code describing the location data was measured",
                        SOILTYPE = "code describing the soil class",
                        SPECODE = "Species code",
                        AB_GENUS = "Genus name",
                        AB_SPECIES = "Species name",
                        AB_VARIETY = "Species variety",
                        A1 = "plot 1, transect A",
                        A2 = "plot 2, transect A",
#                        ...
                        E1,
                        DateInserted = "Date data was uploaded"
                        )



# Data cleanup 
# 
# - All `NA` characters should be numeric 0.  
# 
# - Extract by transect (watershed), by plot, or by site
# - Species as columns 
# - Date as a column

watershed_001d = subset(konza, WATERSHED=="001d")
watershed_001d_ft <- subset(watershed_001d, SOILTYPE %in% c("f", "t"))
data.cols <- grep("[A-D][1-9]", colnames(konza))
species.dates <- which(colnames(dat) %in% c("RECYEAR", "RECMONTH", "RECDAY", "SOILTYPE", "AB_GENUS", "AB_SPECIES", "AB_VARIETY"))
watershed_001d_ft <- watershed_001d_ft[c(data.cols, species.dates)]
dat <- watershed_001d_ft 


# - Combine Species names to single column. (Variety not really necessary)


scientific.name = paste(dat$AB_GENUS, dat$AB_SPECIES, dat$AB_VARIETY, sep=" ")
keepme <- !(colnames(dat) %in% c("AB_GENUS", "AB_SPECIES", "AB_VARIETY"))
dat <- dat[keepme]
dat$scientific.name <- scientific.name


# Combine dates into a single column



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

data.cols <- grep("[A-D][1-9]", colnames(dat))

sapply(dat, function(x){
  x <- as.character(x)
  x[x==""] <- "0"
  abundance_codes[x]
}




