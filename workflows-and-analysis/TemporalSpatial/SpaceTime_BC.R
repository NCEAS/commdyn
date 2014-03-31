library(vegan)
library(reshape)

##############################
####DATA-SET SPECIFIC CODE####
#############################

##IMPORT DATA, SET COLUMN NAMES
master<-read.csv("LTERmaster_clean_011313_LL.csv", header=TRUE, check.name=FALSE)
masterdat<-master[,c("species","site","year","sitesubplot","abundance")]
names(masterdat)=c("species", "site", "year", "plot", "abundance")

##SET 'MINPLOT' AS THE SMALLEST NUMBER OF REPLICATE PLOTS AT A SITE
CALCunique<-function(x){
  length(unique(x))
}
minplot<-min(tapply(masterdat[["plot"]], masterdat[["site"]], CALCunique))

##RUN ANALYSES ON ALL SITES IN MASTERDAT; RETURNS AVERAGE SPATIAL AND TEMPORAL BC VALUES 
sites<-unique(masterdat$site)
BCsites<-data.frame(cbind(site=character(0), spacebc=numeric(0), tempbc=numeric(0)))
for (i in 1:length(sites)){
  subber<-subset(masterdat, masterdat$site==sites[[i]])
  tempout<-CALCreps(subber, "plot", "year", "species", "abundance")  
  tempout2<-data.frame(t(tempout))
  tempout2$site<-unique(subber$site)
  BCsites<-rbind(BCsites, tempout2)
}


#####################################################
##GENERAL FUNCTIONS FOR BC TEMPORAL/SPATIAL ANALYSIS
####################################################

##RANDOMLY SUBSET A MINPLOT NUMBER OF PLOTS FROM A SITE
CALCsample<-function(data1, plotcol){
plots<-as.vector(sample(unique(data1[[plotcol]]), minplot, replace=FALSE))
subber<-data1[which(data1[[plotcol]]%in%c(plots)),]
return(subber)
}

##GENERATE UNIQUE PLOT-PLOT PAIRINGS
CALCplotpairs<-function(data1, plotcol){
plots<-unique(data1[[plotcol]])
myreps<-matrix(0, length(plots), length(plots))
myrep2<-lower.tri(myreps)
rownames(myrep2)=plots
colnames(myrep2)=plots
myrep3<-as.data.frame(myrep2)
myrep3$plot1<-as.factor(rownames(myrep3))
myrep4<-melt(myrep3)
names(myrep4)[2]="plot2"
myrep5<-subset(myrep4, myrep4$value==TRUE)
return(myrep5[,1:2])
}

##CALCULATE BRAY CURTIS DISSIMILARITY BETWEEN ALL PLOTPAIRS
##RETURNS AVERAGE VALUE (FIRST WITHIN YEAR AND THEN ACROSS YEARS)
CALCspatialbc<-function(data1, plotcol, yearcol,  speciescol, abundancecol){
  plotpairs<-CALCplotpairs(data1, plotcol)
  distBC<-data.frame(cbind((data1[0,yearcol]), braycurtis=numeric(0), plot1=character(0), plot2=character(0)))
  yrs<-unique(data1[[yearcol]])
  for (i in c(1:length(yrs))){
    subyr<-subset(data1, data1[[yearcol]]==yrs[i])
    for (j in c(1:(dim(plotpairs)[1]))){
      plot1<-as.character(plotpairs[j,1])
      plot2<-as.character(plotpairs[j,2])
      subyrplot<-subyr[which(subyr[[plotcol]]==plot1 | subyr[[plotcol]]==plot2),]
      subyrplotdummy<-subyrplot[c(plotcol, speciescol, abundancecol)]
      names(subyrplotdummy)=c("plot", "species", abundancecol)
      fullmat<-cast(subyrplotdummy, plot~species, value=abundancecol, fill=0)
      fmdim<-as.numeric(dim(fullmat)[2])
      com<-as.data.frame(fullmat[,2:fmdim])
      comsum1<-sum(com[1,])  
      comsum2<-sum(com[2,])
      com.mat<-ifelse(comsum1>0 & comsum2>0, vegdist(com,method="bray", diag=FALSE, upper=FALSE), NA)
      dat.long<-data.frame(t(combn(fullmat[,1],2)), as.numeric(com.mat))
      names(dat.long)<-c("plot1", "plot2", "braycurtis")
      dat.long[[yearcol]]<-unique(subyr[[yearcol]])
    distBC<-rbind(distBC, dat.long)
    }
  }    
  distBC2<-distBC[which(is.na(distBC$braycurtis)==FALSE),]  
  BCspatialyr<-aggregate(braycurtis~year, data=distBC2, mean)
  BCspatialmean<-mean(BCspatialyr$braycurtis)
  return(BCspatialmean)
}


##CALCULATE BRAY CURTIS ON AVERAGE PLOT COMPOSITION ACROSS THE SITE OVER TIME
##RETURNS VALUE AVERAGED ACROSS YEARS
##another option is to calculated bray-curtis within each plot over time and then average
##it's a question of what scale/question of interest
CALCtemporalmeanbc <- function(data1, yearcol, speciescol, abundancecol){
  meandata1<-aggregate(data1[abundancecol], by=data1[c(yearcol, speciescol)], mean)
  names(meandata1)=c("year", "species", abundancecol)
  fullmat<-cast(meandata1, year~species, value="abundance", fill=0)
  com<-fullmat[,-1]
  com.mat<-vegdist(com,method="bray", diag=FALSE, upper=FALSE)
  dat.long<-data.frame(t(combn(fullmat[,1],2)), as.numeric(com.mat))
  names(dat.long)<-c("Yr1", "Yr2", "braycurtis")
  tempBCout<-subset(dat.long, (dat.long$Yr2-dat.long$Yr1)==1)
  tempBCout$site<-unique(meandata1$site)
  tempBCout$Yr1<-NULL
  names(tempBCout)=c("year", "braycurtis")
  BCtemporalmean<-mean(tempBCout$braycurtis)
  return(BCtemporalmean)  
}


##CALCULATE SPATIAL AND TEMPORAL BRAY CURTIS  USING THE SAME SUBSET OF PLOTS FOR BOTH CALCULATIONS
##(INEGRATES THE ABOVE FUNCTIONS)
CALCspacetime<-function(data1, plotcol, yearcol, speciescol, abundancecol){
  subdata1<-CALCsample(data1, plotcol)
  spacebc<-CALCspatialbc(subdata1, plotcol, yearcol,  speciescol, abundancecol)
  tempbc<-CALCtemporalmeanbc(subdata1, yearcol, speciescol, abundancecol)
  spacetimeout<-cbind(spacebc, tempbc)
  return(spacetimeout)
}


##RUN THE MONTE CARLO ANALYSIS ON MULTIPLE 'MINPLOT' SAMPLES
CALCreps<-function(data1, plotcol, yearcol, speciescol, abundancecol){
  spacetimerepeat<-data.frame(cbind(spacebc=numeric(0), tempbc=numeric(0)))
  #set low rep for now, high rep for final
  for (i in 1:10){
    spacetimetemp<-CALCspacetime(data1, plotcol, yearcol, speciescol, abundancecol)
    spacetimerepeat<-rbind(spacetimerepeat, spacetimetemp)  
  }
  meanspacetime<-colMeans(spacetimerepeat)
  return(meanspacetime)
}
