### Started 6 February 2014 ###
### By Lizzie (mostly for now), Elsa and others at ABI community dynamics meeting ###

## Revisting the varcomps of weather data project from Germany #
## meeting in July 2009. #
## Yes, really! ##
## Some of this code I straight up pulled from #
## LTER_weatherVarGeneral.r, but a lot I made the code #
## better than before ##

# basic housekeeping #
options(stringsAsFactors=FALSE)
print("working directory fun: change me please!")
setwd("/Users/Lizzie/Documents/git/R/misc/varclimLTER") 

# libraries
library(reshape)
library (doBy) # for Elsa's code
library(ggplot2) 
library(gridExtra)

## super boring functions ##
## note: I (Lizzie) usually toss all f(x)s in source files
# or at the top like this but you'll see below I walk through f(x)s
# and the Cedar Creek data, I am leaving it this way for now
# until we figure exactly what is going on ##

makerichnessfigures <- function(data, variablename){
    par(mfrow=c(2, 3))
    plot(richness~at, data=subset(data, data$variable==variablename),
        pch=16)
    plot(richness~et, data=subset(data, data$variable==variablename),
        pch=16)
    plot(richness~st, data=subset(data, data$variable==variablename),
        pch=16)
    plot(richness~annaverage, data=subset(data, 
        data$variable==variablename), pch=16)
}

makerichnessSTfiguresUgh <- function(data){
    par(mfrow=c(2, 1))
    plot1 <- ggplot(data, aes(richness, OurST, color=variable)) +
        geom_point(shape=1) 
    plot2 <- ggplot(data, aes(year, OurST, color=variable)) +
        geom_point(shape=1) 
    grid.arrange(plot1, plot2, ncol=2)
}

makerichnessSTfigures <- function(data){
    # kudos to anyone who switches this to apply!
    par(mfrow=c(2, 2))
    for (i in seq_along(unique(data$variable))){
    plot(richness ~ OurST, data=subset(data,
        data$variable==unique(data$variable)[i]),
        pch=16, main=unique(data$variable)[i])
    }
}

makeyrSTfigures <- function(data){
    # kudos to anyone who switches this to apply!
    par(mfrow=c(2, 2))
    for (i in seq_along(unique(data$variable))){
    plot(OurST ~ year, data=subset(data,
        data$variable==unique(data$variable)[i]),
        pch=16, main=unique(data$variable)[i])
    }
}

##
## Here, here, here! Starts the cleaning and calculating
##

# get the data #
# ALERT! Massively assuming that NA is a dot
# though I could not find that for sure in the metadata
# 
cdrclimate <- read.delim("input/e080_Dailyclimatesummary.txt", header=TRUE, sep = '\t', check.name=FALSE, na.strings="   .")
# https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-cdr.419.1

# do any renaming to make life in the future easier
names(cdrclimate)[names(cdrclimate)=="MaxTemp(degF)"] <- "maxtemp"
names(cdrclimate)[names(cdrclimate)=="MinTemp(degF) "] <- "mintemp"
names(cdrclimate)[names(cdrclimate)=="Precip(inches)"] <- "precip"

# deal with dates
# I know already we'll want month and year so grab those
cdrclimate$date <- as.Date(cdrclimate$Date, format="%m/%d/%Y")
cdrclimate$month <- format(cdrclimate$date, "%m")
cdrclimate$year <- format(cdrclimate$date, "%Y")
cdrclimate$doy <- format(cdrclimate$date, "%j")

# get the mean
cdrclimate$meantemp <- (cdrclimate$mintemp+cdrclimate$maxtemp)/2

# Set the growing season for the LTER site #
gstart <- 4 # This month onward will be included
gend <- 10  # This is the first month at the end of the season that will *not* be included

# Take a look at the time series #
# requires a dataframe with the column names:
# date, mintemp, maxtemp, precip
lookatdata <- function(data){
    par(mfrow=c(2,2))
    plot(maxtemp~date, data=data, pch=16)
    plot(mintemp~date, data=data, pch=16)
    plot(meantemp~date, data=data, pch=16)
    plot(precip~date, data=data, pch=16)
}

# example!
lookatdata(cdrclimate)

# aggregate by month #
# again, give it data with meantemp, maxtemp, mintemp, precip and year
# with those exact colnames
takemeanbymonth <- function(data){
    dater <- aggregate(cbind(data[["meantemp"]], data[["mintemp"]],
        data[["maxtemp"]], data[["precip"]]), by=list(year=data[["year"]],
        month=data[["month"]]), mean, na.rm=TRUE)
    names(dater)[names(dater)=="V1"] <- "monthlymean"
    names(dater)[names(dater)=="V2"] <- "monthlymin"
    names(dater)[names(dater)=="V3"] <- "monthlymax"
    names(dater)[names(dater)=="V4"] <- "monthlyprecip"
    return(dater)
  }
# simple function to throw out climate data outside of the growing season
# give the f(x) data -- with a "month" col., and months in numeric formats
growingseasonize <- function(data, gstart, gend){
    data[["month"]] <- as.numeric(data[["month"]])
    dater <- subset(data, month>gstart & month<gend)
    return(dater)
  }

# examples!
cdrmeans <- takemeanbymonth(cdrclimate)
cdrmeanseas <- growingseasonize(cdrmeans, 4, 10)

## start the variability code (stolen from Jon Shurin) ##
# first, take the overall average across ALL years for each predictor variable for each LTER site (er, for now just CDR) #

# below could be rewritten quicker with reshape
# but it is 17:00 and no one has given me a beer (sadface)
makeallaverage <- function(data){
    goo <- melt(data, id=c("year","month"), all=TRUE)
    allaverage <- aggregate(goo$value, list(variable=goo$variable),
        mean, na.rm=TRUE)
    return(allaverage)
  }

getannualandinterannual <- function(data){
    goo <- melt(data, id=c("year","month"), all=TRUE)
    allaverage <- aggregate(goo$value, list(variable=goo$variable),
        mean, na.rm=TRUE)
    annaverage <- aggregate(goo$value, list(variable=goo$variable,
        year=goo$year), mean, na.rm=TRUE) # get the inter-annual variance
    names(allaverage)[2]<-"allaverage" 
    names(annaverage)[3]<-"annaverage"
    alldata<-merge(annaverage, allaverage, by=c("variable"))
    alldata<-merge(goo, alldata, by=c("variable", "year"))
    # puts annual and overall means together with monthly observations
    # Calculating variance components a la Jon Shurin #
    alldata$at<-alldata$annaverage/alldata$allaverage
    #at, annual deviation from long-term mean
    alldata$st<-alldata$value/alldata$annaverage
    #st, seasonal deviation from annual mean
    allAt=aggregate(alldata$at, list(variable=alldata$variable,
    year=alldata$year), mean, na.rm=TRUE)
    allSt=aggregate(alldata$st, list(variable=alldata$variable,
        month=alldata$month), mean, na.rm=TRUE)
    names(allAt)[3]<-"at"; names(allSt)[3]<-"avgSt";
    alldata<-merge(alldata, allSt, by=c("variable", "month"))
    alldata$et<-alldata$st/alldata$avgSt
    return(alldata)
  }

cdrdevfrommeans <- getannualandinterannual(cdrmeanseas)
cdrallaverage <- makeallaverage(cdrmeanseas)

# data must be run through getannualandinterannual f(x)
# we should someday figure out what this did
# and also double check all my code!!!
# next up, something related to standard deviations
getSDetcstuff <- function(alldata, allaverage, nameme){
    alldata$seast <- abs(alldata$value-alldata$annaverage)   
    tseas <- tapply(alldata$seast, list(alldata$variable),
        mean, na.rm=TRUE)
    newtseas <- data.frame(cbind(variable=c("monthlymean", "monthlyprecip",
        "monthlymax", "monthlymin"), SeasMeanAllYears=unname(tseas)))
    allSeas <- aggregate(alldata$seast, list(variable=alldata$variable,
        year=alldata$year), mean, na.rm=TRUE)
    seasagain <- merge(newtseas, allSeas, by=c("variable"))
    names(seasagain)[4]<-"YearSeasDiff"
    seasagain$OurST <- as.numeric(seasagain$SeasMeanAllYears)/
       as.numeric(seasagain$YearSeasDiff)
    SdAt<-aggregate(alldata$at, list(variable=alldata$variable),
        sd, na.rm=TRUE) # Sd of annual variability, the annual component
    names(SdAt)[2]<-"SdAt"
    SdSt<-aggregate(alldata$avgSt, list(variable=alldata$variable), sd,
        na.rm=TRUE) # Sd of seasonal variability
    names(SdSt)[2]<-"SdSt"
    SdEt<-aggregate(alldata$et, list(variable=alldata$variable), sd,
        na.rm=TRUE) # Sd of events
    names(SdEt)[2]<-"SdEt"
# Above is the last bit of changes to AllData, so I am below adding the new terrestrial seasonal thing here #
    seasagainsm <- subset(seasagain, select =
        c("variable", "year", "OurST"))
    alldata <- merge(alldata, seasagain,
        by=c("variable", "year"), all.x=TRUE, all.y = TRUE)
# Back to Jon's codes, merging up the variance components to get wee file per LTER site -- this is really only useful for comparison across sites #
    VarComps<-merge(SdAt, SdSt, by=c("variable"))
    VarComps<-merge(VarComps, SdEt, by=c("variable"))
    VarComps<-merge(VarComps, allaverage, by=c("variable"))
    names(VarComps)[2:5] <- c("annual","seasonal","events", "allaverage")
    VarComps$site <- nameme
    outputlist <- list(alldata=alldata, varcomps=VarComps,
        season=seasagainsm)
    return(outputlist)
}

cdrbig <- getSDetcstuff(cdrdevfrommeans, cdrallaverage, "CDR")

##
## calculate species richness & evenness at the plot level
## taken from cdr_commdyn.R., which is by Elsa
##

# import CDR data (ask Elsa for it, good workflow, I know!)
cdrplants<-read.delim("input/e001_Plantabovegroundbiomassdata.txt", header=TRUE, sep = '\t', check.name=FALSE)

#just keep the control plots
cdrplants2 = subset(cdrplants, cdrplants$NTrt == 9)

#look at my column names
names(cdrplants)

# take out some columns we don't need
cdrplants2$NTrt = NULL
cdrplants2$NAdd = NULL
cdrplants2$NitrAdd = NULL
cdrplants2 <- cdrplants2[c(-5)]

# calculate species richness & evenness at the plot level
cdrrich <- summaryBy(Biomass ~ Exp + Year + Field  + Plot, data=cdrplants2, FUN=c(length, sum))

##
## now back to Lizzie!
##

# aggregate richness to annual, skipping over everything
aggregatetoyr <- function(data, yearcol, richcol){
    richbyyr <- aggregate(cbind(data[[richcol]]),
    by=list(year=data[[yearcol]]), mean, na.rm=TRUE)
    names(richbyyr)[names(richbyyr)=="V1"] <- "richness"
    return(richbyyr)
  }

cdrrichyrs <- aggregatetoyr(cdrrich, "Year", "Biomass.length")

# merge the climate output and this richness
cdr <- merge(cdrbig[["alldata"]], cdrrichyrs, by=c("year"))
cdrOurST <- merge(cdrbig[["season"]], cdrrichyrs, by=c("year"))

# make figures 
makerichnessfigures(cdr,  "monthlymax")
makerichnessfigures(cdr, "monthlyprecip")
makerichnessSTfigures(cdrOurST)
makeyrSTfigures(cdrOurST)
# bleahh
makerichnessSTfiguresUgh(cdrOurST)
