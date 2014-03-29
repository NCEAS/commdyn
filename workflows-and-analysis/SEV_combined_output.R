###comb richness
par(mar=c(5, 4, 1, 15), xpd=TRUE)

plot(veg.rich, type = "n",
  		ylab="richness",
			xlim = c(1989, 2013), ylim = c(0, 60))

lines(veg.rich$year, veg.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="mediumseagreen")
lines(grass.hop.rich$year, grass.hop.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="cornflowerblue")
lines(larrea.hop.rich$year, larrea.hop.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="darkorchid4")
lines(grass.rich$year, grass.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="#99CC66")
lines(larrea.rich$year, larrea.rich$spp_count, type="b", lty= 1, pch=16, lwd=3.5, col="#990000")

###legend
leg.text <- 	c("vegetation", "grasshopper - grass", "grasshopper - creosote", "small mammal - grass", "small mammal - creosote")
leg.col <- c("mediumseagreen", "cornflowerblue", "darkorchid4", "#99CC66", "#990000")

legend(2015, 62, cex = 1.0, leg.text, col=leg.col, lty=1, pch=16, lwd=3.5)

###richness ANOVA

#combining taxa with years for 1989 through 2013 
rich.comb.full <- data.frame(cbind(year=1989:2013, veg=veg.rich$spp_count, mammal_grass=grass.rich$spp_count, mammal_creosote=larrea.rich$spp_count))

summary(aov(veg ~ mammal_grass + mammal_creosote, data=rich.comb.full))

#combining taxa data that match grasshopper range from 1992 to 2001
rich.comb.hop <- data.frame(cbind(year=1992:2001, veg=veg.rich[4:13,2], grasshopper_grass=grass.hop.rich$spp_count, grasshopper_creosote=larrea.hop.rich$spp_count, mammal_grass=grass.rich[4:13,2], mammal_creosote= larrea.rich[4:13,2]))

summary(aov(veg ~ mammal_grass + mammal_creosote + grasshopper_grass + grasshopper_creosote, data=rich.comb.hop))

summary(aov(veg ~ mammal_creosote, data=rich.comb.hop))


###comb abundance

par(mar=c(5, 4, 1, 15), xpd=TRUE)

plot(veg.ab, type = "n",
  		ylab="abundance",
			xlim = c(1989, 2013), ylim = c(0, 1500))

lines(veg.ab$year, veg.ab$abundance, type="b", lty= 1, pch=16, lwd=3.5, col="mediumseagreen")

lines(grass.hop.ab$year, grass.hop.ab$abundance, type="b", lty= 1, pch=16, lwd=3.5, col="cornflowerblue")
lines(larrea.hop.ab$year, larrea.hop.ab$abundance, type="b", lty= 1, pch=16, lwd=3.5, col="darkorchid4")

lines(grass.cap$year, grass.cap$capture_rate, type="b", lty= 1, pch=16, lwd=3.5, col="#99CC66")
lines(larrea.cap$year, larrea.cap$capture_rate, type="b", lty= 1, pch=16, lwd=3.5, col="#990000")

#legend
leg.text <- 	c("grasshopper - grass", "grasshopper - creosote", "small mammal - grass", "small mammal - creosote")
leg.col <- c("cornflowerblue", "darkorchid4", "#99CC66", "#990000")

legend(2015, 1560, cex = 1.0, leg.text, col=leg.col, lty=1, pch=16, lwd=3.5)

###abundance ANOVA

#combining taxa with years for 1989 through 2013 
ab.comb.full <- data.frame(cbind(year=1989:2013, veg=veg.ab$abundance, mammal_grass=grass.cap$capture_rate, mammal_creosote=larrea.cap$capture_rate))

summary(aov(veg ~ mammal_grass + mammal_creosote, data=ab.comb.full))

#combining taxa data that match grasshopper range from 1992 to 2001
ab.comb.hop <- data.frame(cbind(year=1992:2001, veg=veg.ab[4:13,2], grasshopper_grass=grass.hop.ab$abundance, grasshopper_creosote=larrea.hop.ab$abundance, mammal_grass=grass.cap[4:13,2], mammal_creosote= larrea.cap[4:13,2]))

summary(aov(veg ~ mammal_grass + mammal_creosote + grasshopper_grass + grasshopper_creosote, data=ab.comb.hop))

summary(aov(veg ~ mammal_creosote, data=rich.comb.hop))




