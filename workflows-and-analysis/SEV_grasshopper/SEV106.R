require(plyr)
require(plotrix)
require(calibrate)
require(vegan)
require(car)


SEV106 <- read.csv("workflows-and-analysis/SEV_grasshopper/SEV106_raw.csv", header = TRUE, strip.white = TRUE)

### removing rows were SPECIES = NONE
SEV106 <- SEV106[-which(SEV106$SPECIES == "NONE"),]

### creating year column from date variable 
SEV106["year"] <- strptime(SEV106$DATE, "%m/%d/%Y")$year+1900

##########################################
###creating grass and creosote data.frames

# subsetting by "5 points" location 
grass.hop <- SEV106[which(SEV106$SITE == "G"),]

larrea.hop <- SEV106[which(SEV106$SITE == "C"),]

###########
###richness

###grass
grass.hop.rich <- ddply(grass.hop, .(year), summarise, 
                    spp_count = length(unique(SPECIES)))

grass.hop.rich.m <- mean(grass.hop.rich$spp_count)
#19.7

###creosote
larrea.hop.rich <- ddply(larrea.hop, .(year), summarise, 
                    spp_count = length(unique(SPECIES)))

larrea.hop.rich.m <- mean(larrea.hop.rich$spp_count)
#13.9

###plot
par(mar=c(5, 4, 2, 3), xpd=TRUE)
leg.text <- 	c("grass", "creosote")
leg.col <- c("cornflowerblue", "darkorchid4")

plot(grass.hop.rich, type = "n",
  		ylab="grasshopper richness",
			xlim = c(1989, 2013), ylim = c(5, 25))

lines(grass.hop.rich$year, grass.hop.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="cornflowerblue")
lines(larrea.hop.rich$year, larrea.hop.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="darkorchid4")

###lines for overall means
ablineclip(a=grass.hop.rich.m, b=0, x1=1991, x2=2002, lty= 3, lwd=3.0, col="cornflowerblue")
ablineclip(a=larrea.hop.rich.m, b=0, x1=1991, x2=2002, lty= 3, lwd=3.0, col="darkorchid4")

legend(2008, 25, cex = 1.0, leg.text, col=leg.col, pch=16)


############
###abundance

###grass
grass.hop.ab <- ddply(grass.hop, .(year, SPECIES), summarise, 
                    abundance = sum(CNT))
grass.hop.ab <- ddply(grass.hop.ab, .(year), summarise, 
                    abundance = sum(abundance))

grass.hop.ab.m <- mean(grass.hop.ab$abundance)
#715.7

###grass
larrea.hop.ab <- ddply(larrea.hop, .(year, SPECIES), summarise, 
                    abundance = sum(CNT))
larrea.hop.ab <- ddply(larrea.hop.ab, .(year), summarise, 
                    abundance = sum(abundance))

larrea.hop.ab.m <- mean(larrea.hop.ab$abundance)
#133.8

###plot
par(mar=c(5, 4, 2, 3), xpd=TRUE)

plot(grass.hop.ab, type = "n",
  		ylab="grasshopper abundance",
			xlim = c(1989, 2013), ylim = c(50, 1500))

lines(grass.hop.ab$year, grass.hop.ab$abundance, type="b", lty= 1, pch=16, lwd=3.5, col="cornflowerblue")
lines(larrea.hop.ab$year, larrea.hop.ab$abundance, type="b", lty= 1, pch=16, lwd=3.5, col="darkorchid4")

###lines for overall means
ablineclip(a=grass.hop.ab.m, b=0, x1=1991, x2=2002, lty= 3, lwd=3.0, col="cornflowerblue")
ablineclip(a=larrea.hop.ab.m, b=0, x1=1991, x2=2002, lty= 3, lwd=3.0, col="darkorchid4")

legend(2008, 1500, cex = 1.0, leg.text, col=leg.col, pch=16)


##########################
###creating species matrix

###grass
grass.hop.spp <- aggregate(SPECIES ~ year, data = grass.hop, FUN= table)
grass.hop.spp <- data.frame(year=grass.hop.spp$year, grass.hop.spp$SPECIES)

###larrea
larrea.hop.spp <- aggregate(SPECIES ~ year, data = larrea.hop, FUN= table)
larrea.hop.spp<- data.frame(year=larrea.hop.spp$year, larrea.hop.spp$SPECIES)


#############
###ordination

###combining grass and larrea data.frames for ordination
comb.hop <- join(grass.hop.spp, larrea.hop.spp, type="full")
comb.hop <- comb.hop[,-1]

###ordination
hop.ord <- metaMDS(comb.hop, distance = "bray", trace = TRUE)
hop.ord.scores <- scores(hop.ord, display = c("sites", "species"))

###adding year to scores data.frame
year <- data.frame(rep(1992:2001, times=2))
colnames(year) <- "year"

scores.yr <- cbind(hop.ord.scores, year)

###plot
par(mar=c(4, 4, 2, 2), xpd=TRUE)
leg.text <- 	c("grass", "creosote")
leg.col <- c("cornflowerblue", "darkorchid4")

plot(hop.ord, type = "n", xlim = c(-1.5, 1.0), ylim = c(-1.0, 1.0))
 
points(hop.ord.scores[c(1:10),], cex = 1.2, pch=16, col="cornflowerblue")
points(hop.ord.scores[c(11:20),], cex = 1.2, pch=16, col="darkorchid4")

textxy(scores.yr$NMDS1, scores.yr$NMDS2, scores.yr$year)

legend(-1.5, 1.0, cex = 1, leg.text, col=leg.col, pch=16)


####################
###time-lag analysis

###grass

# creating distance matrix from species matrix
DM_grass.hop <- vegdist(grass.hop.spp, method="euclidean",
								diag=FALSE, upper=FALSE, na.rm=TRUE)
DM_grass.hop <- as.matrix(DM_grass.hop)	

# get the lags from distance matrix
get_lags = function(DM_grass.hop){

# label each row and each column.
rownums = row(DM_grass.hop)
colnums = col(DM_grass.hop)

# A mini-function to get all the elements that are lagged by i time steps,
# then put them into a long-form matrix with i in the first column and
# the matrix value in the second column.
get_lag_i = function(i){
    cbind(lag = i, value = DM_grass.hop[rownums == (colnums + i)])
  }

# apply get_lag_i to all lags from 1 to n-1
# replace n with number of columns 
  lag_list = lapply(
    1:(10-1),
    get_lag_i
  )

# squash all the lags from the list into one long-form matrix
  do.call(rbind, lag_list)
}

# final: matrix converted to long-form
DM_grass.hop.long <- data.frame(get_lags(DM_grass.hop))

# regression
lm.grass.hop <- lm(value ~ lag, data=DM_grass.hop.long)
summary(lm.grass.hop)

# plotting regression graph 
par(mar=c(5, 5, 1, 1), xpd=TRUE)
plot(DM_grass.hop.long$value, DM_grass.hop.long$lag, type="n",
     xlab="time lag",
     ylab="distance",
		xlim=c(0,10), ylim=c(0,500))

# plotting points
points(DM_grass.hop.long$lag, DM_grass.hop.long$value, cex = 1.0, pch=16, col="cornflowerblue")

# plotting regression line
ablineclip(lm.grass.hop, x1=0, x2=10, type="l", lty= 1, lwd=3.5, col="#000000")


###creosote

# creating distance matrix from species matrix
DM_creosote.hop <- vegdist(larrea.hop.spp, method="euclidean",
								diag=FALSE, upper=FALSE, na.rm=TRUE)
DM_creosote.hop <- as.matrix(DM_creosote.hop)	

# get the lags from distance matrix
get_lags = function(DM_creosote.hop){

# label each row and each column.
rownums = row(DM_creosote.hop)
colnums = col(DM_creosote.hop)

# A mini-function to get all the elements that are lagged by i time steps,
# then put them into a long-form matrix with i in the first column and
# the matrix value in the second column.
get_lag_i = function(i){
    cbind(lag = i, value = DM_creosote.hop[rownums == (colnums + i)])
  }

# apply get_lag_i to all lags from 1 to n-1
# replace n with number of columns 
  lag_list = lapply(
    1:(10-1),
    get_lag_i
  )

# squash all the lags from the list into one long-form matrix
  do.call(rbind, lag_list)
}

# final: matrix converted to long-form
DM_creosote.hop.long <- data.frame(get_lags(DM_creosote.hop))

# regression
lm.creosote.hop <- lm(value ~ lag, data=DM_creosote.hop.long)
summary(lm.creosote.hop)

# plotting regression graph 
par(mar=c(5, 5, 1, 1), xpd=TRUE)
plot(DM_creosote.hop.long$value, DM_creosote.hop.long$lag, type="n",
     xlab="time lag",
     ylab="distance",
		xlim=c(0,10), ylim=c(0,100))

# plotting points
points(DM_creosote.hop.long$lag, DM_creosote.hop.long$value, cex = 1.0, pch=16, col="darkorchid4")

# plotting regression line
ablineclip(lm.creosote.hop, x1=0, x2=10, type="l", lty= 1, lwd=3.5, col="#000000")



#############
###Rank plots

###grass

# Removing species that never occure throughout time-series
grass.hop.sum <- colSums(grass.hop.spp, dims = 1)
grass.hop_v2 <- grass.hop.spp[grass.hop.sum > 0]

# Reshape the data in long format
grass.hop.rank <- melt(grass.hop_v2, value.name = "abundance", id.vars = "year", variable.name = "species")

# graph: linear version 
ggplot(grass.hop.rank, aes(year,log(abundance),color=species)) + geom_line()

# graph: final circular version
par(mar=c(10, 10, 1, 1), xpd=TRUE)
grass.hop.rank.plot <- ggplot(grass.hop.rank, aes(year,log(abundance),color=species)) + 
																			geom_line() + 
																			coord_polar() + 
																			theme_bw() + 
																			ylab("grass log abundance") + 
																			theme(legend.position = "none", 
																				axis.title.x=element_text(size=15), 
																				axis.title.y=element_text(size=15))
grass.hop.rank.plot <- grass.hop.rank.plot + geom_text(data = grass.hop.rank[grass.hop.rank$year == "1997",], aes(label = species))
plot(grass.hop.rank.plot)


###creosote

# Removing species that never occure throughout time-series
creosote.hop.sum <- colSums(larrea.hop.spp, dims = 1)
creosote.hop_v2 <- larrea.hop.spp[creosote.hop.sum > 0]

# Reshape the data in long format
creosote.hop.rank <- melt(creosote.hop_v2, value.name = "abundance", id.vars = "year", variable.name = "species")

# graph: linear version 
ggplot(creosote.hop.rank, aes(year,log(abundance),color=species)) + geom_line()

# graph: final circular version
par(mar=c(10, 10, 1, 1), xpd=TRUE)
creosote.hop.rank.plot <- ggplot(creosote.hop.rank, aes(year,log(abundance),color=species)) + 
																			geom_line() + 
																			coord_polar() + 
																			theme_bw() + 
																			ylab("creosote log abundance") + 
																			theme(legend.position = "none", 
																				axis.title.x=element_text(size=15), 
																				axis.title.y=element_text(size=15))
creosote.hop.rank.plot <- creosote.hop.rank.plot + geom_text(data = creosote.hop.rank[creosote.hop.rank$year == "1997",], aes(label = species))
plot(creosote.hop.rank.plot)

