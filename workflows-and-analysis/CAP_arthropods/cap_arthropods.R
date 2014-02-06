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
# CAP arthropod data:
arth <- read.csv("~/Dropbox/conferences/NCEAS_workshop_feb2014/lter_arthropods.csv")

column.codes <- colnames(arth)

#building metadata into table. save for later. available at: http://caplter.asu.edu/data/data-catalog/?id=41
#column.definitions <- c("Data code (factor), same for all data in this dataset", 
#                        sampling_event_id = "ID number connecting to a sampling event",
#                         site_id = "Unique numbers representing specific sites", 
#                         sample_date = "Date the sample was taken",
#                         default_person_for_trap_samples = "Month data recorded (M)",
#                         RECDAY = "Day data recorded (D)",
#                         WATERSHED = "code describing the location data was measured",
#                         SOILTYPE = "code describing the soil class",
#                         SPECODE = "Species code",
#                         AB_GENUS = "Genus name",
#                         AB_SPECIES = "Species name",
#                         AB_VARIETY = "Species variety",
#                         A1 = "plot 1, transect A",
#                         A2 = "plot 2, transect A",
# #                        ...
#                         E1,
#                         DateInserted = "Date data was uploaded"
#                         )



# Data cleanup
# replace NULL values in data (columns 14:18) with 0
size <- sapply(arth[,14:18], function(x) as.numeric(as.character(x)))
arth[,14:18] <- size

# sum size class counts into counts by display_name
arth$abundance <- rowSums( arth[,14:18], na.rm=TRUE )

# keep only columns of interest
myvars <- c("site_id", "sample_date", "abundance", "display_name")
arth2 <- arth[myvars]

# Reshape data from tabular into site X species matrix with abundance as value
library(reshape2)
sxs <- dcast(arth2, site_id + sample_date ~ display_name, value.var="abundance")
head(sxs)


