require(plyr)
require(plotrix)
require(calibrate)
require(vegan)
require(car)
library(reshape)
library(ggplot2)


SEV004 <- read.csv("workflows-and-analysis/SEV_vegetation/SEV004_raw.csv", header = TRUE, strip.white = TRUE)

##########################################
###creating "Five points" location subset

veg <- SEV004[which(SEV004$location == "FP"),]

###########
###richness
veg.rich <- ddply(veg, .(year), summarise, 
                    spp_count = length(unique(species_code)))

veg.rich.m <- mean(veg.rich$spp_count)
#39.4

###plot
par(mar=c(5, 4, 2, 3), xpd=TRUE)

plot(veg.rich, type = "n",
  		ylab="vegetation richness",
			xlim = c(1989, 2013), ylim = c(15, 55))

lines(veg.rich$year, veg.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="mediumseagreen")

###lines for overall means
ablineclip(a=veg.rich.m, b=0, x1=1989, x2=2013, lty= 3, lwd=3.0, col="mediumseagreen")


############
###abundance
veg.ab <- ddply(veg, .(year), summarise, 
                    abundance = length(species_code))

veg.ab.m <- mean(veg.ab$abundance)
#5663.88

###plot
par(mar=c(5, 4, 2, 3), xpd=TRUE)

plot(veg.ab, type = "n",
  		ylab="vegetation abundance",
			xlim = c(1989, 2013), ylim = c(2500, 8500))

lines(veg.ab$year, veg.ab$abundance, type="b", lty= 1, pch=16, lwd=3.5, col="mediumseagreen")

###lines for overall means
ablineclip(a=veg.ab.m, b=0, x1=1989, x2=2013, lty= 3, lwd=3.0, col="mediumseagreen")


##########
###"cover"

veg$size <- veg$stop - veg$start

veg.cover <- ddply(veg, .(year, species_code), summarise,
														cover=sum(size))

##########################
###creating species matrix
veg.spp <- aggregate(species_code ~ year, data = veg, FUN= table)
veg.spp <- data.frame(year=veg.spp$year, veg.spp$species_code)

# Removing species that never occure throughout time-series
veg.sum <- colSums(veg.spp, dims = 1)
veg.spp <- veg.spp[veg.sum > 0]


#############
###ordination
veg.ord <- metaMDS(veg.spp, distance = "bray", trace = TRUE)

veg.ord.scores <- scores(veg.ord, display = c("sites", "species"))

###adding year to scores data.frame
year <- data.frame(1989:2013)
colnames(year) <- "year"

scores.yr <- cbind(veg.ord.scores, year)

###plot
par(mar=c(4, 4, 2, 2), xpd=TRUE)
plot(veg.ord, type = "n", xlim = c(-1.5, 1.0), ylim = c(-1.0, 1.0))
 
points(veg.ord.scores[c(1:25),], cex = 1.2, pch=16, col="mediumseagreen")

textxy(scores.yr$NMDS1, scores.yr$NMDS2, scores.yr$year)


####################
###time-lag analysis

# creating distance matrix from species matrix
DM_veg <- vegdist(veg.spp, method="euclidean",
								diag=FALSE, upper=FALSE, na.rm=TRUE)
DM_veg <- as.matrix(DM_veg)	

# get the lags from distance matrix
get_lags = function(DM_veg){

# label each row and each column.
rownums = row(DM_veg)
colnums = col(DM_veg)

# A mini-function to get all the elements that are lagged by i time steps,
# then put them into a long-form matrix with i in the first column and
# the matrix value in the second column.
get_lag_i = function(i){
    cbind(lag = i, value = DM_veg[rownums == (colnums + i)])
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
DM_veg.long <- data.frame(get_lags(DM_veg))

# regression
lm.veg <- lm(value ~ lag, data=DM_veg.long)
summary(lm.veg)

# plotting regression graph 
par(mar=c(5, 5, 1, 1), xpd=TRUE)
plot(DM_veg.long$value, DM_veg.long$lag, type="n",
     xlab="time lag",
     ylab="distance",
		xlim=c(0,25), ylim=c(0,3050))

# plotting points
points(DM_veg.long$lag, DM_veg.long$value, cex = 1.0, pch=16, col="mediumseagreen")

# plotting regression line
ablineclip(lm.veg, x1=0, x2=25, type="l", lty= 1, lwd=3.5, col="#000000")


#############
###Rank plots

# Reshape the data in to long format
veg.rank <- melt(veg.spp, id.vars = "year", variable_name = "species")
names(veg.rank)[3] = "abundance"
# graph: linear version 
ggplot(veg.rank, aes(year,log(abundance),color=species)) + geom_line()

# graph: final circular version
par(mar=c(10, 10, 1, 1), xpd=TRUE)
veg.rank.plot <- ggplot(veg.rank, aes(year,log(abundance),color=species)) + 
																			geom_line() + 
																			coord_polar() + 
																			theme_bw() + 
																			ylab("vegetation log abundance") + 
																			theme(legend.position = "none", 
																				axis.title.x=element_text(size=15), 
																				axis.title.y=element_text(size=15))
veg.rank.plot <- veg.rank.plot + geom_text(data = veg.rank[veg.rank$year == "1990",], aes(label = species))
plot(veg.rank.plot)
