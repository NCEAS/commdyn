require(plyr)
require(plotrix)
require(calibrate)
require(vegan)
require(car)


SEV008 <- read.csv("workflows-and-analysis/SEV_small_mammal/SEV008_raw.csv", header = TRUE, strip.white = TRUE)

##########################################
###creating grass and creosote data.frames

# subsetting by "5 points" location 
grass <- SEV008[which(SEV008$location == "5pgrass"),]

larrea <- SEV008[which(SEV008$location == "5plarrea"),]

###########
###richness

###grass

grass.rich <- ddply(grass, .(year), summarise, 
                    spp_count = length(unique(species)))

grass.rich.m <- mean(grass.rich$spp_count)
#8.2

###creosote
larrea.rich <- ddply(larrea, .(year), summarise, 
                    spp_count = length(unique(species)))

larrea.rich.m <- mean(larrea.rich$spp_count)
#9.96

###plot
par(mar=c(5, 4, 2, 3), xpd=TRUE)
leg.text <- 	c("grass", "creosote")
leg.col <- c("#99CC66", "#990000")

plot(grass.rich, type = "n",
  		ylab="species richness",
			xlim = c(1989, 2013), ylim = c(4, 16))

lines(grass.rich$year, grass.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="#99CC66")
lines(larrea.rich$year, larrea.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="#990000")

###lines for overall means
ablineclip(a=8.2, b=0, x1=1989, x2=2013, lty= 3, lwd=3.0, col="#99CC66")
ablineclip(a=9.96, b=0, x1=1989, x2=2013, lty= 3, lwd=3.0, col="#990000")

legend(2008, 16, cex = 1.0, leg.text, col=leg.col, pch=16)
	
###############
###capture rate

###grass
grass.cap <- ddply(grass, .(year), summarise, 
                    capture_rate = length(species))

###creosote
larrea.cap <- ddply(larrea, .(year), summarise, 
                    capture_rate = length(species))

###plot
par(mar=c(5, 4, 2, 3), xpd=TRUE)
leg.text <- 	c("grass", "creosote")
leg.col <- c("#99CC66", "#990000")

plot(larrea.cap, type = "n",
  		ylab="capture rate",
			xlim = c(1989, 2013), ylim = c(0, 1000))

lines(grass.cap$year, grass.cap$capture_rate, type="b", lty= 1, pch=16, lwd=3.5, col="#99CC66")
lines(larrea.cap$year, larrea.cap$capture_rate, type="b", lty= 1, pch=16, lwd=3.5, col="#990000")

legend(1990, 1000, cex = 1.0, leg.text, col=leg.col, pch=16)

##########################
###creating species matrix

###grass
grass.spp <- aggregate(species ~ year, data = grass, FUN= table)
grass.spp <- data.frame(year=grass.spp$year, grass.spp$species)

###larrea
larrea.spp <- aggregate(species ~ year, data = larrea, FUN= table)
larrea.spp<- data.frame(year=larrea.spp$year, larrea.spp$species)


#############
###ordination

###combining grass and larrea data.frames for ordination
comb <- join(grass.spp, larrea.spp, type="full")
comb <- comb[,-1]

###ordination
comb.ord <- metaMDS(comb, distance = "bray", trace = TRUE)
comb.ord.scores <- scores(comb.ord, display = c("sites", "species"))

###adding year to scores data.frame
year <- data.frame(rep(1989:2013, times=2))
colnames(year) <- "year"

scores.yr <- cbind(comb.ord.scores, year)

###plot
par(mar=c(4, 4, 2, 2), xpd=TRUE)
leg.text <- 	c("grass", "creosote")
leg.col <- c("#99CC66", "#990000")

plot(comb.ord, type = "n", xlim = c(-2.5, 1.0), ylim = c(-1.0, 1.0))
 
points(comb.ord.scores[c(1:25),], cex = 1.2, pch=16, col="#99CC66")
points(comb.ord.scores[c(26:50),], cex = 1.2, pch=16, col="#990000")

textxy(scores.yr$NMDS1, scores.yr$NMDS2, scores.yr$year)

legend(-2.5, 1.25, cex = 1, leg.text, col=leg.col, pch=16)

####################
###time-lag analysis

###grass

# creating distance matrix from species matrix
DM_grass <- vegdist(grass.spp, method="euclidean",
								diag=FALSE, upper=FALSE, na.rm=TRUE)
DM_grass <- as.matrix(DM_grass)	

# get the lags from distance matrix
get_lags = function(DM_grass){

# label each row and each column.
rownums = row(DM_grass)
colnums = col(DM_grass)

# A mini-function to get all the elements that are lagged by i time steps,
# then put them into a long-form matrix with i in the first column and
# the matrix value in the second column.
get_lag_i = function(i){
    cbind(lag = i, value = DM_grass[rownums == (colnums + i)])
  }

# apply get_lag_i to all lags from 1 to n-1
# replace n with number of columns 
  lag_list = lapply(
    1:(25-1),
    get_lag_i
  )

# squash all the lags from the list into one long-form matrix
  do.call(rbind, lag_list)
}

# final: matrix converted to long-form
DM_grass.long <- data.frame(get_lags(DM_grass))

# regression
lm.grass <- lm(value ~ lag, data=DM_grass.long)
summary(lm.grass)

# plotting regression graph 
par(mar=c(5, 5, 1, 1), xpd=TRUE)
plot(DM_grass.long$value, DM_grass.long$lag, type="n",
     xlab="time lag",
     ylab="distance",
		xlim=c(0,25), ylim=c(0,300))

# plotting points
points(DM_grass.long$lag, DM_grass.long$value, cex = 1.0, pch=16, col="#99CC66")

# plotting regression line
ablineclip(lm.grass, x1=0, x2=25, type="l", lty= 1, lwd=3.5, col="#000000")


###creosote

# creating distance matrix from species matrix
DM_creosote <- vegdist(larrea.spp, method="euclidean",
								diag=FALSE, upper=FALSE, na.rm=TRUE)
DM_creosote <- as.matrix(DM_creosote)	

# get the lags from distance matrix
get_lags = function(DM_creosote){

# label each row and each column.
rownums = row(DM_creosote)
colnums = col(DM_creosote)

# A mini-function to get all the elements that are lagged by i time steps,
# then put them into a long-form matrix with i in the first column and
# the matrix value in the second column.
get_lag_i = function(i){
    cbind(lag = i, value = DM_creosote[rownums == (colnums + i)])
  }

# apply get_lag_i to all lags from 1 to n-1
# replace n with number of columns 
  lag_list = lapply(
    1:(25-1),
    get_lag_i
  )

# squash all the lags from the list into one long-form matrix
  do.call(rbind, lag_list)
}

# final: matrix converted to long-form
DM_creosote.long <- data.frame(get_lags(DM_creosote))

# regression
lm.creosote <- lm(value ~ lag, data=DM_creosote.long)
summary(lm.creosote)

# plotting regression graph
par(mar=c(5, 5, 1, 1), xpd=TRUE)
plot(DM_creosote.long$value, DM_creosote.long$lag, type="n",
     xlab="time lag",
     ylab="distance",
		xlim=c(0,25), ylim=c(0,500))

# plotting points
points(DM_creosote.long$lag, DM_creosote.long$value, cex = 1.0, pch=16, col="#990000")

# plotting regression line
ablineclip(lm.creosote, x1=0, x2=25, type="l", lty= 1, lwd=3.5, col="#000000")



#############
###Rank plots

###grass

# Removing species that never occure throughout time-series
grass.sum <- colSums(grass.spp, dims = 1)
grass_v2 <- grass.spp[grass.sum > 0]

# Reshape the data in long format
grass.rank <- melt(grass_v2, value.name = "abundance", id.vars = "year", variable.name = "species")

# graph: linear version 
ggplot(grass.rank, aes(year,log(abundance),color=species)) + geom_line()

# graph: final circular version
par(mar=c(10, 10, 1, 1), xpd=TRUE)
grass.rank.plot <- ggplot(grass.rank, aes(year,log(abundance),color=species)) + 
																			geom_line() + 
																			coord_polar() + 
																			theme_bw() + 
																			ylab("grass log abundance") + 
																			theme(legend.position = "none", 
																				axis.title.x=element_text(size=15), 
																				axis.title.y=element_text(size=15))
grass.rank.plot <- grass.rank.plot + geom_text(data = grass.rank[grass.rank$year == "1997",], aes(label = species))
plot(grass.rank.plot)


###creosote

# Removing species that never occure throughout time-series
creo.sum <- colSums(larrea.spp, dims = 1)
creo_v2 <- larrea.spp[creo.sum > 0]

# Reshape the data: year, variable (y1,y2,y3), value
creo.rank <- melt(creo_v2, value.name = "abundance", id.vars = "year", variable.name = "species")

# graph: linear version 
ggplot(creo.rank, aes(year,log(abundance),color=species)) + geom_line()

# graph: final circular version
par(mar=c(10, 10, 1, 1), xpd=TRUE)
creo.rank.plot <- ggplot(creo.rank, aes(year,log(abundance),color=species)) + 
																			geom_line() + 
																			coord_polar() + 
																			theme_bw() + 
																			ylab("creosote log abundance") + 
																			theme(legend.position = "none", 
																				axis.title.x=element_text(size=15), 
																				axis.title.y=element_text(size=15))
																				
creo.rank.plot <- creo.rank.plot + geom_text(data = creo.rank[creo.rank$year == "1994",], aes(label = species))
plot(creo.rank.plot)
