## Supplementary Information for Hallett et al. "codyn: An R package of Community Dynamics" ##
## Code to make figures from the paper ##

# Load libraries
library(ggplot2)
library(grid)
library(gridExtra)
library(codyn)
library(dplyr)
library(tidyr)

# Set the color palette
cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")



### FIGURE 1 ######
### Rank clocks ###

## Load data
data("collins08")

# Average abundances of four focal species within a year and replicate
aggdat <- aggregate(abundance ~ species * year * replicate,  data = subset(collins08, 
                                                                           species == "andrgera" |
                                                                           species == "andrscop" | 
                                                                           species == "poaprat"| 
                                                                           species == "sorgnuta"), FUN = mean)

# Create a descriptive name for the replicate treatments
aggdat$replicate2 <- "Annually burned"
aggdat$replicate2 <- ifelse(aggdat$replicate == "unburned", "Unburned", aggdat$replicate2)

# Rename species for graphing purposes
aggdat$species <- as.factor(as.character(aggdat$species))
levels(aggdat$species) = c("A. gerardii", "S. scoparium","P. pratensis", "S. nutans")


## Create the graph
fig1 <- ggplot(aggdat, aes(year, abundance, color = species)) + 
  # plot species lines
  geom_line(size = 1) + 
  # faceted by species
  facet_wrap(~replicate2) +
  # on polor coordinates
  + coord_polar()
  # label axes
  labs(x="Year", y="Abundance", color="Species") + 
  # format
  theme_bw() +
  theme(text = element_text(size = 14), 
        strip.text.x = element_text(size = 14), 
        strip.background = element_blank(),
        panel.grid.major = element_line(size = 1)) + 
  theme(legend.position="bottom", 
        legend.text=element_text(face = "italic")) +
  # color-blind friendly palette
  scale_color_manual( values = cbbPalette) + 
  # add a line to indicate the start
  geom_segment(aes(x = 1984, y = 0, xend = 1984, yend = 100), color = "grey70")


########## FIGURE 2 ##############
### Temporal diversity indices ###
  
### Panel A: richness ###

# Calculate richness within year and replicate
rich.dat <-collins08 %>%
    filter(abundance>0) %>%
    mutate(rich=1) %>%
    group_by(year, replicate) %>%
    summarize(richness=sum(rich)) %>%
    tbl_df() %>%
    # Create a descriptive name for the replicate treatments
    mutate(replicate2="Annually burned", replicate2=ifelse(replicate=="unburned", "Unburned", replicate2))
  
# Create the graph  
rich.graph <- ggplot(rich.dat, aes(x=year, y=richness)) +
  geom_line(size = 1) + 
  facet_wrap(~replicate2) +
  theme_bw() + 
  xlim(1985, 2001)
  
  
### Panel B: turnover ###
  
#Run the turnover code
KNZ_turnover <- turnover(df = collins08,  
                         time.var = "year",  
                         species.var = "species", 
                         abundance.var = "abundance", 
                         replicate.var = "replicate")

KNZ_appearance <- turnover(df = collins08,  
                           time.var = "year",  
                           species.var = "species", 
                           abundance.var = "abundance", 
                           replicate.var = "replicate",
                           metric = "appearance")

KNZ_disappearance <- turnover(df = collins08, 
                              time.var = "year",  
                              species.var = "species", 
                              abundance.var = "abundance", 
                              replicate.var = "replicate",
                              metric = "disappearance")
  
#Format a compiled data frame
KNZ_turnover$metric<-"total"
names(KNZ_turnover)[1]="turnover"
  
KNZ_appearance$metric<-"appearance"
names(KNZ_appearance)[1]="turnover"
  
KNZ_disappearance$metric<-"disappearance"
names(KNZ_disappearance)[1]="turnover"
  
KNZ_allturnover<-rbind(KNZ_turnover, KNZ_appearance, KNZ_disappearance)
  
#Create the graph
turn.graph <- ggplot(KNZ_allturnover, aes(x=year, y=turnover, color=metric)) + 
  geom_line(size = 1) + 
  facet_wrap(~replicate) + 
  theme_bw() + 
  theme(legend.position="bottom")
  
### Panel C: mean rank shifts ###
  
#Run the rank shift code
KNZ_rankshift <- rank_shift(df=collins08, 
                            time.var = "year", 
                            species.var = "species",
                            abundance.var = "abundance", 
                            replicate.var = "replicate")
  
#Select the final time point from the returned time.var_pair
KNZ_rankshift$year <- as.numeric(substr(KNZ_rankshift$year_pair, 6,9))
  
# Create the graph
rankshift.graph <- ggplot(KNZ_rankshift, aes(year, MRS)) + 
  geom_line(size = 1) + 
  facet_wrap(~replicate) +
  theme_bw() 
  
  
### Panel D: rate change ###
  
# Run the rate change code
comm.res <- rate_change_interval(collins08,   
                                 time.var= "year",    
                                 species.var= "species",  
                                 abundance.var= "abundance", 
                                 replicate.var = "replicate")
  
# Create the graph
rate.graph<-ggplot(comm.res, aes(interval, distance, group = replicate)) + 
  geom_point() + 
  facet_wrap(~replicate) + 
  stat_smooth(method = "lm", se = F, size = 1) +
  theme_bw() 
  
### Put the panel graph together ###
fig2 <- grid.arrange(rich.graph +
                  labs(x="Year", y=expression(paste("Richness (no / 10", m^2,")"))) +
                  theme(strip.text.x = element_text(size = 14),
                        strip.background = element_blank()) +
                  theme( plot.margin=unit(c(0,1,0,0), "cm")),
                  
                turn.graph + 
                  labs(x="Year", y="Turnover") +
                  theme(legend.position="none") +
                  theme(strip.background = element_blank(),
                        strip.text.x = element_blank()) +
                  scale_color_manual( values = cbbPalette) + 
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
                  theme( plot.margin=unit(c(0,1,0,0), "cm")), 
                
                ncol=1)




######### FIGURE 3 ########
### Community stability ###

## Calculate community stability 
stab <- community_stability(knz_001d, 
                            time.var = "year",
                            abundance.var = "abundance",
                            replicate.var="subplot")

# Calculate synchrony via loreau, merge with stab
synch_loreau<-merge(synchrony(knz_001d, 
                              time.var = "year",
                              species.var = "species",
                              abundance.var = "abundance",
                              replicate="subplot"), 
                    stab)

# Calculate synchrony via gross, merge with stab
synch_gross<-merge(synchrony(knz_001d, 
                             time.var = "year",
                             species.var = "species",
                             abundance.var = "abundance",
                             replicate="subplot",
                             metric="Gross"), 
                   stab)

# Calculate variance ratio, merge with stab
vr <- merge(variance_ratio(knz_001d, time.var = "year",
                           species.var = "species",
                           abundance.var = "abundance",
                           replicate="subplot",
                           bootnumber=1, 
                           average.replicates = F), 
            stab)

# Make the graphs
vr.graph <-ggplot(vr, aes(x=VR, y=stability)) + 
  geom_point(size=3) +
  theme_bw() +   
  theme(text= element_text(size = 14))

loreau.graph <-ggplot(synch_loreau, aes(x=synchrony, y=stability)) + 
  geom_point(size=3) + 
  theme_bw() +   
  theme(text= element_text(size = 14))

gross.graph <-ggplot(synch_gross, aes(x=synchrony, y=stability)) + 
  geom_point(size=3) + 
  theme_bw() +   
  theme(text= element_text(size = 14))

## Make the panel graph ##
fig3 <- grid.arrange(vr.graph +
               labs(x="Variance ratio", y="Community stability"),
               
             loreau.graph + 
               labs(x="Synchrony (Loreau)", y=""), 
             
             gross.graph +
               labs(x="Synchrony (Gross)", y=""),
             ncol=3)


