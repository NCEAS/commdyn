library(ggplot2)
library(gridExtra)
library(codyn)

##turnover
KNZ_turnover <- turnover(df = collins08,  time.var = "year",  species.var = "species", 
                         abundance.var = "abundance", replicate.var = "replicate")
KNZ_appearance <- turnover(df = collins08,  replicate.var = "replicate",  metric = "appearance")
KNZ_disappearance <- turnover(df = collins08, replicate.var = "replicate", metric = "disappearance")

KNZ_turnover$metric<-"total"
names(KNZ_turnover)[1]="turnover"
KNZ_appearance$metric<-"appearance"
names(KNZ_appearance)[1]="turnover"
KNZ_disappearance$metric<-"disappearance"
names(KNZ_disappearance)[1]="turnover"
KNZ_allturnover<-rbind(KNZ_turnover, KNZ_appearance, KNZ_disappearance)

turn.graph <- ggplot(KNZ_allturnover, aes(x=year, y=turnover, color=metric)) + 
  geom_line(size = 2) + theme_bw() + facet_wrap(~replicate) + theme(legend.position="bottom")


##rank clocks
aggdat <- aggregate(abundance ~ species * year * replicate,  data = subset(collins08, species == "andrgera" |
     species == "andrscop" | species == "poaprat"| species == "sorgnuta"), FUN = mean)

rankclock.graph <- ggplot(aggdat, aes(year, abundance, color = species)) + 
  geom_line(size = 2) + coord_polar() + theme_bw() + facet_wrap(~replicate) + theme(legend.position="bottom")

## mean rank shifts
KNZ_rankshift <- mean_rank_shift(df=collins08,  abundance.var = "abundance", replicate.var = "replicate")
#Select the final time point from the returned time.var_pair
KNZ_rankshift$year <- as.numeric(substr(KNZ_rankshift$year_pair, 6,9))
# Plot it
rankshift.graph <- ggplot(KNZ_rankshift, aes(year, MRS)) + 
  geom_line(size= 2) + theme_bw() + facet_wrap(~replicate)



##rate change
comm.res <- rate_change_interval(collins08,   time.var= "year",    
                                 species.var= "species",  abundance.var= "abundance", replicate.var = "replicate")

rate.graph<-ggplot(comm.res, aes(interval, distance, group = replicate)) + facet_wrap(~replicate) + 
  geom_point() + theme_bw() + stat_smooth(method = "lm", se = F, size = 2)

  grid.arrange(turn.graph, rankclock.graph, rankshift.graph, rate.graph)
  
  grid.arrange(turn.graph + theme(legend.position="none")  , rankshift.graph, rate.graph, ncol=1)
  
  , heights=c(.4, .7, .1, .1))
  
