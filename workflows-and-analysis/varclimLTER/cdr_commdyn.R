## By Elsa ##
## emailed to me on 7 February 2014 ##
## note from Elsa: Richness at the plot level gets calculated early and ends up being called Biomass.length ##

library (doBy)
library(car)
library(reshape)

setwd("/Users/Lizzie/Documents/git/R/misc/varclimLTER")

# import CDR data
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
cdrrich<-summaryBy(Biomass~ Exp + Year + Field  + Plot, data=cdrplants2, FUN=c(length, sum))

cdrrichlong=merge(cdrrich, cdrplants2, by=c("Exp", "Year", "Field", "Plot"))
cdrrichlong$relabun= cdrrichlong$Biomass/cdrrichlong$Biomass.sum
cdrrichlong$forH = abs(cdrrichlong$relabun*(log(cdrrichlong$relabun)))

cdrrichH<-summaryBy(forH~ Exp + Year + Field + Plot + Biomass.length, data=cdrrichlong, FUN=c(sum))
cdrrichH$evenness=cdrrichH$forH.sum/log(cdrrichH$Biomass.length)

# calculate mean and standard error of richness for control and fertilized treatments for each year
cdrrich2<-summaryBy(Biomass.length~ Exp + Year + Field + NAdd, data=cdrrich, FUN=c(length, mean, sd))
cdrrich2$richSE=cdrrich2$Biomass.length.sd/sqrt(cdrrich2$Biomass.length.length)

# take a look at these data

xyplot(evenness ~ Year | Field, data=cdrrichH,
            xlab="Year", ylab="evenness", 
            main="CDR")

xyplot(Biomass.length ~ Year | Field, data=cdrrichH,
       xlab="Year", ylab="richness", 
       main="CDR")
