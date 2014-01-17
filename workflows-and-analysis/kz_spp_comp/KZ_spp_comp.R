install.packages("vegan")
install.packages("plotrix")
install.packages("car")
install.packages("gdata")

library(vegan)
library(MASS)


###RaMPs#############################################
#####################################################
###call file from your machine 
spp_matrix <- read.csv("RaMP_spp_comp_matrix_FINAL.csv", header = TRUE, strip.white = TRUE)
head(spp_matrix)
ncol(spp_matrix)
nrow(spp_matrix)

#################################
###calculating cover mean by RaMP
#################################
library(reshape2)
library(plyr)

matrix_long <- melt(spp_matrix, id.vars=c("ramp"), value.name = "cover", variable.name = "species")
head(matrix_long)

###creating treatment column 
ii.control=c(1,8,14)
ii.delay=c(2,5,9,10,12,13)
ii.ambient=c(3,4,6,7,11,15)

matrix_long = within(  matrix_long, {
  treatment = 0
  treatment = treatment + 1*(ramp %in% ii.control)
  treatment = treatment + 2*(ramp %in% ii.delay)
  treatment = treatment + 3*(ramp %in% ii.ambient)
  
  treatment = factor(treatment, levels=1:3, labels=c('control', 'delay', 'ambient'))
}) 

head(matrix_long)
nrow(matrix_long)

###cover means by ramp and treatment
matrix.rm <- ddply(matrix_long, .(ramp), summarise,
	cover_mean = mean(cover))

matrix.rm

matrix.tm <- ddply(matrix_long, .(treatment), summarise,
	cover_mean = mean(cover))

head(matrix.tm)

#############
###Ordination
#############

###removing ramp column so to run ordination
matrix.ord <- spp_matrix[,-c(1)]
head(matrix.ord)
ncol(matrix.ord)
nrow(matrix.ord)


#ordination
ord.spp.comp <- metaMDS(matrix.ord, dustance="bray", trace = FALSE)

#scores
ord.scores <- scores(ord.spp.comp, display = c("sites", "species"), choices = c(1,2))

#################
###"enviro" layer
enviro <- data.frame(spp_matrix[,1])
colnames(enviro) <- "ramp"

###creating treatment column 
ii.control=c(1,8,14)
ii.delay=c(2,5,9,10,12,13)
ii.ambient=c(3,4,6,7,11,15)

enviro = within(  enviro, {
  treatment = 0
  treatment = treatment + 1*(ramp %in% ii.control)
  treatment = treatment + 2*(ramp %in% ii.delay)
  treatment = treatment + 3*(ramp %in% ii.ambient)
  
  treatment = factor(treatment, levels=1:3, labels=c('control', 'delay', 'ambient'))
}) 

head(enviro)
nrow(enviro)

###creating year column
year <- c(1997:2012)
N <- rep(15,16)

enviro$year <- rep(year, N) 


###binding scores with enviro to calculate means and se
scores.env <- cbind(ord.scores, enviro)
head(scores.env)

###############
###mean by ramp
#x 
scores.m.x <- ddply(scores.env, .(ramp), summarise,
					Xmean = mean(NMDS1))

#y
scores.m.y <- ddply(scores.env, .(ramp), summarise,
					Ymean = mean(NMDS2))

scores.m <- cbind(scores.m.x, scores.m.y)
scores.m <- scores.m[,-3]

#######################
#standard error by ramp
require(plyr)

#x
scores.se.x <- ddply(scores.env, .(ramp), summarise,
					Xse = sd(NMDS1)/sqrt(length(NMDS1)))

#y
scores.se.y <- ddply(scores.env, .(ramp), summarise,
					Yse = sd(NMDS2)/sqrt(length(NMDS2)))

scores.m.se <- cbind(scores.m, scores.se.x, scores.se.y)
scores.m.se <- scores.m.se[,-c(4,6)]


scores.m.se$Xmean_pse <- scores.m.se$Xmean + scores.m.se$Xse
scores.m.se$Xmean_nse <- scores.m.se$Xmean - scores.m.se$Xse
scores.m.se$Ymean_pse <- scores.m.se$Ymean + scores.m.se$Yse
scores.m.se$Ymean_nse <- scores.m.se$Ymean - scores.m.se$Yse

head(scores.m.se)

##########
###plotting
par(mfrow = c(1, 1))

par(mar=c(5, 4, 2, 2), xpd=TRUE)

plot(scores.m$Xmean, scores.m$Ymean, type = "n",
					xlab="NMDS1", ylab="NMDS2",
					xlim = c(-0.5,0.5), ylim = c(-0.5,0.7))

###error bars
#arrows(x0, y0, x1, y1)
#vertical
arrows(scores.m.se[,2], scores.m.se[,9], scores.m.se[,2], scores.m.se[,8], angle=90, code=3, length=0)
#horizontal
arrows(scores.m.se[,7], scores.m.se[,3], scores.m.se[,6], scores.m.se[,3], angle=90, code=3, length=0)
###error bars can be color coded like points if scores.m.se was divided by ramp 

###points
points(scores.m[c(1,8,14),2], scores.m[c(1,8,14),3], cex = 1.2, pch=16, col="grey1", bg="grey1")
points(scores.m[c(2,5,9,10,12,13),2], scores.m[c(2,5,9,10,12,13),3], cex = 1.2, pch=16, col="slategray", bg="slategray")
points(scores.m[c(3,4,6,7,11,15),2], scores.m[c(3,4,6,7,11,15),3], cex = 1.2, pch=16, col="turquoise3", bg="turquoise3")
	
	
###error ellipses
group <- enviro[,1]
ordiellipse(ord.spp.comp, group, kind = "se", conf = 0.95)


###OR error bars as segments so to match color of points
#segments(xLeft, yBottom, xRight, yTop, col="#000000")
#vertical (xLeft and xRight the same)
segments(scores.m.se[1,2], scores.m.se[1,9], scores.m.se[1,2], scores.m.se[1,8], col="grey1")
segments(scores.m.se[2,2], scores.m.se[2,9], scores.m.se[2,2], scores.m.se[2,8], col="slategray")
segments(scores.m.se[3,2], scores.m.se[3,9], scores.m.se[3,2], scores.m.se[3,8], col="turquoise3")
segments(scores.m.se[4,2], scores.m.se[4,9], scores.m.se[4,2], scores.m.se[4,8], col="turquoise3")
segments(scores.m.se[5,2], scores.m.se[5,9], scores.m.se[5,2], scores.m.se[5,8], col="slategray")
segments(scores.m.se[6,2], scores.m.se[6,9], scores.m.se[6,2], scores.m.se[6,8], col="turquoise3")
segments(scores.m.se[7,2], scores.m.se[7,9], scores.m.se[7,2], scores.m.se[7,8], col="turquoise3")
segments(scores.m.se[8,2], scores.m.se[8,9], scores.m.se[8,2], scores.m.se[8,8], col="grey1")
segments(scores.m.se[9,2], scores.m.se[9,9], scores.m.se[9,2], scores.m.se[9,8], col="slategray")
segments(scores.m.se[10,2], scores.m.se[10,9], scores.m.se[10,2], scores.m.se[10,8], col="slategray")
segments(scores.m.se[11,2], scores.m.se[11,9], scores.m.se[11,2], scores.m.se[11,8], col="turquoise3")
segments(scores.m.se[12,2], scores.m.se[12,9], scores.m.se[12,2], scores.m.se[12,8], col="slategray")
segments(scores.m.se[13,2], scores.m.se[13,9], scores.m.se[13,2], scores.m.se[13,8], col="slategray")
segments(scores.m.se[14,2], scores.m.se[14,9], scores.m.se[14,2], scores.m.se[14,8], col="grey1")
segments(scores.m.se[15,2], scores.m.se[15,9], scores.m.se[15,2], scores.m.se[15,8], col="turquoise3")

#horizontal (ybottom and yTop the same)
segments(scores.m.se[1,7], scores.m.se[1,3], scores.m.se[1,6], scores.m.se[1,3], col="grey1")
segments(scores.m.se[2,7], scores.m.se[2,3], scores.m.se[2,6], scores.m.se[2,3], col="slategray")
segments(scores.m.se[3,7], scores.m.se[3,3], scores.m.se[3,6], scores.m.se[3,3], col="turquoise3")
segments(scores.m.se[4,7], scores.m.se[4,3], scores.m.se[4,6], scores.m.se[4,3], col="turquoise3")
segments(scores.m.se[5,7], scores.m.se[5,3], scores.m.se[5,6], scores.m.se[5,3], col="slategray")
segments(scores.m.se[6,7], scores.m.se[6,3], scores.m.se[6,6], scores.m.se[6,3], col="turquoise3")
segments(scores.m.se[7,7], scores.m.se[7,3], scores.m.se[7,6], scores.m.se[7,3], col="turquoise3")
segments(scores.m.se[8,7], scores.m.se[8,3], scores.m.se[8,6], scores.m.se[8,3], col="grey1")
segments(scores.m.se[9,7], scores.m.se[9,3], scores.m.se[9,6], scores.m.se[9,3], col="slategray")
segments(scores.m.se[10,7], scores.m.se[10,3], scores.m.se[10,6], scores.m.se[10,3], col="slategray")
segments(scores.m.se[11,7], scores.m.se[11,3], scores.m.se[11,6], scores.m.se[11,3], col="turquoise3")
segments(scores.m.se[12,7], scores.m.se[12,3], scores.m.se[12,6], scores.m.se[12,3], col="slategray")
segments(scores.m.se[13,7], scores.m.se[13,3], scores.m.se[13,6], scores.m.se[13,3], col="slategray")
segments(scores.m.se[14,7], scores.m.se[14,3], scores.m.se[14,6], scores.m.se[14,3], col="grey1")
segments(scores.m.se[15,7], scores.m.se[15,3], scores.m.se[15,6], scores.m.se[15,3], col="turquoise3")

###labeling points
require(calibrate)
textxy(scores.m$Xmean, scores.m$Ymean, scores.m$ramp, cex=1, m = c(0, 1))

###legend
leg.text <- c("control", "ambient", "delay")
leg.col <- c("grey1", "turquoise3", "slategray")
legend(0.3, 0.7, cex = 1, leg.text, col=leg.col, pch=16)



###Permanova#######################################################
###################################################################
###vegan package "adonis"
require(vegan)
###vegan example
data(dune)
head(dune)
ord_dune <- metaMDS(dune)
summary(ord_dune)

data(dune.env)
attach(dune.env)
head(dune.env)

plot(ord, disp="sites", type="n")
ordihull(ord, Management, col="blue")
ordiellipse(ord, Management, col=3,lwd=2)
ordispider(ord, Management, col="red", label = TRUE)
points(ord, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)

betad <- betadiver(dune, "z")
betad

###all years#####################################################
#################################################################
###post rare removed
spp.comp.rr <- read.csv("RaMP_spp_comp_matrix_FINAL.csv", header = TRUE, strip.white = TRUE)
head(spp.comp.rr)

window.rr <- spp.comp.rr[,2:39]
head(window.rr)
nrow(window.rr)

### creating  z variables with betadiver
betad_all <- betadiver(window.rr, "z")

###"enviro" ramp layer for adonis
nrow(enviro)

###running adonis
ad.all <- adonis(window.rr ~ treatment*year, data=enviro, perm=200)
ad.all
summary(ad.all)

###per year############################################################
#######################################################################
spp.comp.rr
nrow(spp.comp.rr)

###dividing spp.comp.rr by year
window.97 <- spp.comp.rr[1:15,2:39]
nrow(window.97)
window.98 <- spp.comp.rr[16:30,2:39]
window.99 <- spp.comp.rr[31:45,2:39]
window.00 <- spp.comp.rr[46:60,2:39]
window.01 <- spp.comp.rr[61:75,2:39]
window.02 <- spp.comp.rr[76:90,2:39]
window.03 <- spp.comp.rr[91:105,2:39]
window.04 <- spp.comp.rr[106:120,2:39]
window.05 <- spp.comp.rr[121:135,2:39]
window.06 <- spp.comp.rr[136:150,2:39]
window.07 <- spp.comp.rr[151:165,2:39]
window.08 <- spp.comp.rr[166:180,2:39]
window.09 <- spp.comp.rr[181:195,2:39]
window.10 <- spp.comp.rr[196:210,2:39]
window.11 <- spp.comp.rr[211:225,2:39]
window.12 <- spp.comp.rr[226:240,2:39]

### creating  z variables with betadiver
betad_97 <- betadiver(window.97, "z")
betad_98 <- betadiver(window.98, "z")
betad_99 <- betadiver(window.99, "z")
betad_00 <- betadiver(window.00, "z")
betad_01 <- betadiver(window.01, "z")
betad_02 <- betadiver(window.02, "z")
betad_03 <- betadiver(window.03, "z")
betad_04 <- betadiver(window.04, "z")
betad_05 <- betadiver(window.05, "z")
betad_06 <- betadiver(window.06, "z")
betad_07 <- betadiver(window.07, "z")
betad_08 <- betadiver(window.08, "z")
betad_09 <- betadiver(window.09, "z")
betad_10 <- betadiver(window.10, "z")
betad_11 <- betadiver(window.11, "z")
betad_12 <- betadiver(window.12, "z")

###"enviro" layer for adonis
adonis.env <- enviro[1:15,1:2]

###running adonis
ad.97 <- adonis(betad_97 ~ treatment, adonis.env, perm=200)
ad.98 <- adonis(betad_98 ~ treatment, adonis.env, perm=200)
ad.99 <- adonis(betad_99 ~ treatment, adonis.env, perm=200)
ad.00 <- adonis(betad_00 ~ treatment, adonis.env, perm=200)
ad.01 <- adonis(betad_01 ~ treatment, adonis.env, perm=200)
ad.02 <- adonis(betad_02 ~ treatment, adonis.env, perm=200)
ad.03 <- adonis(betad_03 ~ treatment, adonis.env, perm=200)
ad.04 <- adonis(betad_04 ~ treatment, adonis.env, perm=200)
ad.05 <- adonis(betad_05 ~ treatment, adonis.env, perm=200)
ad.06 <- adonis(betad_06 ~ treatment, adonis.env, perm=200)
ad.07 <- adonis(betad_07 ~ treatment, adonis.env, perm=200)
ad.08 <- adonis(betad_08 ~ treatment, adonis.env, perm=200)
ad.09 <- adonis(betad_09 ~ treatment, adonis.env, perm=200)
ad.10 <- adonis(betad_10 ~ treatment, adonis.env, perm=200)
ad.11 <- adonis(betad_11 ~ treatment, adonis.env, perm=200)
ad.12 <- adonis(betad_12 ~ treatment, adonis.env, perm=200)

ad.97
ad.98
ad.99
ad.00
ad.01
ad.02
ad.03
ad.04
ad.05
ad.06
ad.07
ad.08
ad.09
ad.10
ad.11
ad.12


###melting table
library(reshape2)
spp_comp_1997.long <- melt(spp_comp_1997, id.vars = c("ramp"), variable.name = "species", value.name = "value")
head(spp_comp_1997.long)


###SIMPER######################################################
###############################################################

###example 
data(dune)
data(dune.env)
(sim <- with(dune.env, simper(dune, Management)))
summary(sim)


###for real
(sim <- with(enviro, simper(window.rr, treatment)))
summary(sim, ordered = TRUE)



###rda################################################
######################################################

###example
data(varespec)
data(varechem)
head(varespec)
head(varechem)

vare.cca <- cca(varespec, varechem)
vare.cca
plot(vare.cca)

dune.Manure <- rda(dune ~ Manure, dune.env)
plot(dune.Manure)

spp.comp.cca <- cca(sp.comp_m, )


###Community Stability###################################################
####################################################################
head(spp.comp.rr)
head(enviro)

###adding year column from enviro to spp matrix
spp.comp.rr <- cbind(spp.comp.rr, year)
nrow(spp.comp.rr)

###converting spp matric to long format by ramp then year
library(reshape2)
spp.comp.long <- melt(spp.comp.rr, id.vars = c("ramp", "year"), variable.name = "species", value.name = "value")
head(spp.comp.long)

###creating treatment column for community stability 
ii.control=c(1,8,14)
ii.delay=c(2,5,9,10,12,13)
ii.ambient=c(3,4,6,7,11,15)

spp.comp.long = within(  spp.comp.long, {
  treatment = 0
  treatment = treatment + 1*(ramp %in% ii.control)
  treatment = treatment + 2*(ramp %in% ii.delay)
  treatment = treatment + 3*(ramp %in% ii.ambient)
  
  treatment = factor(treatment, levels=1:3, labels=c('control', 'delay', 'ambient'))
})  

###Total cover
###summing values by treatment then by species
library(plyr)
sum.tc <- ddply(spp.comp.long, .(treatment, species), summarise, sum = sum(value))
nrow(sum.tc)

m.tc <- ddply(sum.tc, .(treatment), summarise, 
										mean = mean(sum),
										sd = sd(sum))
m.tc


t.cs <- ddply(m.tc, .(treatment), summarise, 
										cs = mean/sd)
#final
t.cs

###testing normality
shapiro.test(sum.tc$sum)

library(nortest)
ad.test(sum.tc$sum)

df <- data.frame(sum.tc[39:76,3])
p <- ggplot(df, aes(sample = sum.tc[39:76,3]))
p <- p + stat_qq()
print(p)

qqnorm(sum.tc[39:76,3])

#ambient
qqPlot(sum.tc[77:114,3])
#delay
qqPlot(sum.tc[39:76,3])


###ANOVA
sum.tc$treatment <- factor(sum.tc$treatment)
library(car)
library(nnet)
lm.sum.tc <- lm(sum ~ treatment, data = sum.tc)
Anova(lm.sum.tc, type=3)
summary(lm.sum.tc)

tc.aov <- aov(sum ~ treatment, sum.tc)
summary(tc.aov)
TukeyHSD(tc.aov)


###KW ANOVA
fit.sum.tc <- kruskal.test(sum ~ treatment, data = sum.tc)
fit.sum.tc


###Grass cover
#creating veg form column to sum.tc
veg <- c("F", "S", "G", "F", "S", "G", "F", "S", "G", "F")  
N <- c(13, 1, 8, 29, 1, 8, 29, 1, 8, 16)

form <- data.frame(form=rep(veg, N))
nrow(form)
nrow(sum.tc)

sum.tc <- cbind(sum.tc, form)
head(sum.tc)


###taking out grass rows
sum.gc <- sum.tc[which(sum.tc$form == "G"),]
nrow(g.cs)
g.cs

g.cs <- ddply(sum.gc, .(treatment), summarise,
								mean = mean(sum),
								sd = sd(sum))

g.cs <- ddply(g.cs, .(treatment), summarise, 
										cs = mean/sd)

#final
g.cs

###ANOVA
library(car)
library(nnet)
lm.sum.gc <- lm(sum ~ treatment + species, data = sum.gc)
Anova(lm.sum.gc, type=3)
summary(lm.sum.gc)

###KW ANOVA
fit.sum.gc <- kruskal.test(sum ~ treatment + species + treatment:species, data = sum.gc)
fit.sum.gc



###Forb cover
sum.fc <- sum.tc[which(sum.tc$form== "F"),]
sum.fc

###ANOVA
library(car)
library(nnet)
lm.sum.fc <- lm(sum ~ treatment + species, data = sum.fc)
Anova(lm.sum.fc, type=3)
summary(lm.sum.fc)

###KW ANOVA
fit.sum.fc <- kruskal.test(sum ~ treatment + species, data = sum.fc)
fit.sum.fc


###Population Stability###############################################
#####################################################################
spp.comp.rr <- spp.comp.rr[,-40]
head(spp.comp.rr)

library(reshape2)
spp.comp.long <- melt(spp.comp.rr, id.vars = c("ramp"), variable.name = "species", value.name = "value")
head(spp.comp.long)

###summing values by ramp then by species
sum.ps <- ddply(spp.comp.long, .(ramp, species), summarise, sum = sum(value))
sum.ps

m <- ddply(sum.ps, .(ramp), summarise, 
								mean = mean(sum),
								sd = sd(sum))
m

Pop_Stb <- ddply(m, .(ramp), summarise, ps = mean/sd)
Pop_Stb

###creating treatment column 
ii.control=c(1,8,14)
ii.delay=c(2,5,9,10,12,13)
ii.ambient=c(3,4,6,7,11,15)

Pop_Stb = within(  Pop_Stb, {
  treatment = 0
  treatment = treatment + 1*(ramp %in% ii.control)
  treatment = treatment + 2*(ramp %in% ii.delay)
  treatment = treatment + 3*(ramp %in% ii.ambient)
  
  treatment = factor(treatment, levels=1:3, labels=c('control', 'delay', 'ambient'))
})  

PS_trm <- ddply(Pop_Stb, .(treatment), summarise, mean = mean(ps))

#final
PS_trm

###ANOVA
###creating treatment column for sum.ps
ii.control=c(1,8,14)
ii.delay=c(2,5,9,10,12,13)
ii.ambient=c(3,4,6,7,11,15)

sum.ps = within(  sum.ps, {
  treatment = 0
  treatment = treatment + 1*(ramp %in% ii.control)
  treatment = treatment + 2*(ramp %in% ii.delay)
  treatment = treatment + 3*(ramp %in% ii.ambient)
  
  treatment = factor(treatment, levels=1:3, labels=c('control', 'delay', 'ambient'))
}) 
head(sum.ps)

library(car)
library(nnet)
lm.sum.ps <- lm(sum ~ treatment + ramp, data = sum.ps)
Anova(lm.sum.ps, type=3)
summary(lm.sum.ps)

ps.aov <- aov(sum ~ treatment, sum.ps)
summary(ps.aov)
TukeyHSD(ps.aov)


###KW ANOVA
fit.sum.ps <- kruskal.test(sum ~ treatment, data = sum.ps)
fit.sum.ps


###Time-Lag Analysis###################################################
#######################################################################
###example with RaMP 1 a contol treatment

RaMP_1 <- spp.comp.rr[which(spp.comp.rr$ramp== "1"),]
RaMP_1 <- RaMP_1[,-1]

###creating distance matrix using vegdist
require(vegan)

DM_1 <- vegdist(RaMP_1, method="euclidean", diag = FALSE, upper = FALSE, na.rm = TRUE)
DM_1 <- as.matrix(DM_1)

c1 <- data.frame(DM_1[2:16,1])
c2 <- data.frame(DM_1[3:16,2])
c3 <- data.frame(DM_1[4:16,3])
c4 <- data.frame(DM_1[5:16,4])
c5 <- data.frame(DM_1[6:16,5])
c6 <- data.frame(DM_1[7:16,6])
c7 <- data.frame(DM_1[8:16,7])
c8 <- data.frame(DM_1[9:16,8])
c9 <- data.frame(DM_1[10:16,9])
c10 <- data.frame(DM_1[11:16,10])
c11 <- data.frame(DM_1[12:16,11])
c12 <- data.frame(DM_1[13:16,12])
c13 <- data.frame(DM_1[14:16,13])
c14 <- data.frame(DM_1[15:16,14])
c15 <- data.frame(DM_1[16:16,15])

###
library(gdata)
DM_1 <- cbindX(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15)
colnames(DM_1) <- c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15")

DM_1$time.lag <- c(1:15)

###melting matrix
library(reshape2)
DM_1.long <- melt(DM_1, idvars = time.lag, measure.vars = 1:15, value.name = "distance", na.rm = TRUE)
head(DM_1.long)
nrow(DM_1.long)

###regression
library(car)
library(nnet)
lm.DM_1 <- lm(distance ~ time.lag, data = DM_1.long)

cor(DM_1.long$time.lag, DM_1.long$distance)

DM_1.long <- DM_1.long[,-2]

cor.DM_1.lag <- cor(DM_1.long)
cor.DM_1.lag.pval <- cor.DM_1.lag
for(i1 in 1:ncol(DM_1.long)) {
	for(i2 in 1:ncol(DM_1.long)) {
		cor.DM_1.lag.pval[i1,i2] <- cor.test(DM_1.long[, i1], DM_1.long[, i2])$p.value
	}
}

cor.DM_1.lag.pval

###plotting graph
par(mfrow = c(1, 1))
par(mar=c(4, 4, 1, 2), xpd=TRUE)
plot(DM_1.long$time.lag, DM_1.long$distance, type="n",
     xlab="distance",
     ylab="creosote time lag",
		xlim=c(0,15), ylim=c(0,40))

points(DM_1.long$time.lag, DM_1.long$distance, cex = 1.0, pch=16, col="#990000")
#install.packages("plotrix")
require(plotrix)
ablineclip(lm.DM_1, x1=0, x2=15, type="b", lty= 1, pch=16, lwd=3.5, col="#000000")

