library(ggplot2)
library(grid)
library(gridExtra)
library(codyn)
library(dplyr)
library(tidyr)

### Plain old richness ###
rich.dat <-collins08 %>%
  filter(abundance>0) %>%
  mutate(rich=1) %>%
  group_by(year, replicate) %>%
  summarize(richness=sum(rich)) %>%
  tbl_df() %>%
  mutate(replicate2="Annually burned", replicate2=ifelse(replicate=="unburned", "Unburned", replicate2))

rich.graph <- ggplot(rich.dat, aes(x=year, y=richness)) + geom_line(size = 2) + theme_bw() + facet_wrap(~replicate2) 


### Mean rank abundance ###


### Turnover metric ###

#Run the turnover code
KNZ_turnover <- turnover(df = collins08,  time.var = "year",  species.var = "species", 
                         abundance.var = "abundance", replicate.var = "replicate")
KNZ_appearance <- turnover(df = collins08,  replicate.var = "replicate",  metric = "appearance")
KNZ_disappearance <- turnover(df = collins08, replicate.var = "replicate", metric = "disappearance")

#Format a compiled data frame
KNZ_turnover$metric<-"total"
names(KNZ_turnover)[1]="turnover"
KNZ_appearance$metric<-"appearance"
names(KNZ_appearance)[1]="turnover"
KNZ_disappearance$metric<-"disappearance"
names(KNZ_disappearance)[1]="turnover"
KNZ_allturnover<-rbind(KNZ_turnover, KNZ_appearance, KNZ_disappearance)

#Make the graph
turn.graph <- ggplot(KNZ_allturnover, aes(x=year, y=turnover, color=metric)) + 
  geom_line(size = 2) + theme_bw() + facet_wrap(~replicate) + theme(legend.position="bottom")

### Mean rank shifts ###

#Run the function
KNZ_rankshift <- mean_rank_shift(df=collins08,  abundance.var = "abundance", replicate.var = "replicate")

#Select the final time point from the returned time.var_pair
KNZ_rankshift$year <- as.numeric(substr(KNZ_rankshift$year_pair, 6,9))

# Plot it
rankshift.graph <- ggplot(KNZ_rankshift, aes(year, MRS)) + 
  geom_line(size= 2) + theme_bw() + facet_wrap(~replicate)


### Rate change ###

#Run the function
comm.res <- rate_change_interval(collins08,   time.var= "year",    
                                 species.var= "species",  abundance.var= "abundance", replicate.var = "replicate")

### Make the graph ###
rate.graph<-ggplot(comm.res, aes(interval, distance, group = replicate)) + facet_wrap(~replicate) + 
  geom_point() + theme_bw() + stat_smooth(method = "lm", se = F, size = 2)


### Put it all together!! ###
  tiff("temporal_diversity_graph.tiff", width=400, height=600)
          grid.arrange(rich.graph +
                 labs(x="Year", y=expression(paste("Richness (no / 10", m^2,")"))) +
                 theme(strip.text.x = element_text(size = 14),
                  strip.background = element_blank()) +
                 theme( plot.margin=unit(c(0,1,0,0), "cm")),
               turn.graph + 
                 labs(x="Year", y="Turnover") +
                 theme(legend.position="none") +
                theme(strip.background = element_blank(),
               strip.text.x = element_blank()) +
                 theme( plot.margin=unit(c(0,1,0,0), "cm")),
               rankshift.graph + 
                 labs(x="Year", y="Mean rank shift") +
                 theme(strip.background = element_blank(),
                                        strip.text.x = element_blank()) +
                 theme( plot.margin=unit(c(0,1,0,0), "cm")), 
               rate.graph +  
                 labs(x="Time interval", y="Euclidean distance") +
                 theme(strip.background = element_blank(),
                                  strip.text.x = element_blank()) +
                 theme( plot.margin=unit(c(0,1,0,0), "cm"))
               , ncol=1)
  dev.off()


### Rank clocks ###
aggdat <- aggregate(abundance ~ species * year * replicate,  data = subset(collins08, species == "andrgera" |
              species == "andrscop" | species == "poaprat"| species == "sorgnuta"), FUN = mean)

aggdat$replicate2 <- "Annually burned"
aggdat$replicate2 <- ifelse(aggdat$replicate == "unburned", "Unburned", aggdat$replicate2)

## make the graphs
rankclock.graph <- ggplot(aggdat, aes(year, abundance, color = species)) + 
  geom_line(size = 2) + coord_polar() + theme_bw() + facet_wrap(~replicate2) + 
  labs(x="Year", y="Abundance", color="Species") +
  theme(text= element_text(size = 14), strip.text.x = element_text(size = 14), strip.background = element_blank()) + theme( legend.position="bottom")



tiff("rank_clock.tiff", width=600, height=400)
rankclock.graph
dev.off()

### Make stability graphs ###
data("knz_001d")

# calculate stability
stab <- community_stability(knz_001d, replicate.var="subplot")

#calculate synchrony via loreau
synch_loreau<-merge(synchrony(knz_001d, replicate="subplot"), stab)

#calculate synchrony via gross
synch_gross<-merge(synchrony(knz_001d, replicate="subplot", metric="Gross"), stab)

#calculate VR
vr <- merge(variance_ratio(knz_001d, replicate.var="subplot", bootnumber=1, average.replicates = F), stab)

# make the graphs
vr.graph <-ggplot(vr, aes(x=VR, y=stability)) + geom_point(size=3) + #geom_smooth(size=1, method="lm", se=F)+
  theme_bw() +   theme(text= element_text(size = 14))
loreau.graph <-ggplot(synch_loreau, aes(x=synchrony, y=stability)) + geom_point(size=3) + 
 # geom_smooth(size=1, method="lm", se=F) + 
  theme_bw() +   theme(text= element_text(size = 14))
gross.graph <-ggplot(synch_gross, aes(x=synchrony, y=stability)) + geom_point(size=3) + 
  #geom_smooth(size=1, method="lm", se=F) + 
  theme_bw() +   theme(text= element_text(size = 14))



tiff("stability_graphs.tiff", width=900, height=400)
grid.arrange(vr.graph + 
               labs(x="Variance ratio", y="Community stability (mean / std dev)") +
               theme( plot.margin=unit(c(1,0,1,.5), "cm")),
             loreau.graph + 
               labs(x="Synchrony (Loreau)", y="") +
               theme( plot.margin=unit(c(1,.25,1,.25), "cm")), 
             gross.graph +
               labs(x="Synchrony (Gross)", y="")+
               theme( plot.margin=unit(c(1,.5,1,0), "cm")),
             ncol=3)
dev.off()
