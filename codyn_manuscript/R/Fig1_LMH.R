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



tiff("rank_clock.tiff", width=400, height=300)
rankclock.graph
dev.off()


# Rank-abundance, just for fun. Different species occur in each, so a bit of work to re-format
unb <- codyn:::transpose_community(collins08[collins08$replicate=="unburned",],
                                   tim = "year",
                                   spec = "species",
                                   abun= "abundance")

ann <- codyn:::transpose_community(collins08[collins08$replicate=="annually burned",],
                                   tim = "year",
                                   spec = "species",
                                   abun= "abundance")
ranann <- data.frame(abun = sort(apply(ann, 2, mean)/max(ann), decreasing = T), rank = 1:ncol(ann)); ranann$sp = rownames(ranann)
ranunb <- data.frame(abun = sort(apply(unb, 2, mean)/max(unb), decreasing = T), rank = 1:ncol(unb));ranunb$sp = rownames(ranunb)
ran.all <- merge(ranunb, ranann, by = "sp", all = T)
ran.all <- data.frame(spec = rep(ran.all$sp, 2), 
                      abun = c(ran.all[,2], ran.all[,4]),
                      rank = c(ran.all[,3], ran.all[,5]),
                      burn = gl(2, nrow(ran.all), labels = levels(collins08$replicate)))

rankplot <- ggplot(ran.all, aes(rank, abun)) + geom_line(size = 2) + theme_bw() +
  facet_wrap(~burn) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                    labels = trans_format("log10", math_format(10^.x)))


tiff("rankabun.tiff", width=400, height=300)
rankplot + labs(x="Ranks", y= "Log relative abundance") +
  theme(strip.text.x = element_text(size = 14),
        strip.background = element_blank()) +
  theme( plot.margin=unit(c(0,1,0,0), "cm"))
dev.off()
system("open rankabun.tiff -a /Applications/Preview.app")
