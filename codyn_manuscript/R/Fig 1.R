# Figure 1 of codyn methods paper

# Goal: make a figure which compares non-temporal measures of diversity (richness, rank abundance) with temporal measures (tunoer, rank clock, MRS, rate change).

# use Konza data from Collins et al. 2008 Ecology as the example data set.
library(vegan)
library(codyn)
library(ggplot2)

data(collins08)

# Initial richness measures. Use transpose_community to create plot x species data frames for vegan
all.dat <- codyn:::transpose_community(collins08,
                                   tim = "year",
                                   spec = "species",
                                   abun= "abundance")

unb <- codyn:::transpose_community(collins08[collins08$replicate=="unburned",],
                                   tim = "year",
                                   spec = "species",
                                    abun= "abundance"
                                  )

ann <- codyn:::transpose_community(collins08[collins08$replicate=="annually burned",],
                                   tim = "year",
                                   spec = "species",
                                   abun= "abundance"
                                   )

rich.unb <- apply(unb, 1, function(x) length(x[x>0]))
rich.ann <- apply(ann, 1, function(x) length(x[x>0]))
shan.unb <- apply(unb, 1, diversity)
shan.ann <- apply(ann, 1, diversity)

static.dat <- data.frame(year = rep(rownames(ann), 2), 
           burn = gl(2, nrow(ann), labels = levels(collins08$replicate)),
           rich = c(rich.ann, rich.unb), 
           shan = c(shan.ann, shan.unb))

ggplot(static.dat, aes(year, rich)) + geom_point() + facet_wrap(~burn) + theme_bw()

# Make figures like 08 fig 1. Relative abundance.
rad.ann <- sort(apply(ann, 2, mean)/max(x), decreasing = T)
rad.unb <- sort(apply(unb, 2, mean)/max(x), decreasing = T)


########## For multiple rank lines in one figure. Matt, ignore this
rad.ann <- apply(ann, 1, function(x) sort(
        x[x>0]/max(x),
        decreasing = T))

rad.unb <- apply(unb, 1, function(x) sort(
  x[x>0]/max(x),
  decreasing = T))

par(mfrow = c(1, 2))
plot(1:nrow(ann), seq(0.0001, 1, length.out = nrow(ann)),
     ylab = "Relative abundance",
     xlab = "Year",
     type = "n",
     las = 2,
     xlim = c(0, nrow(ann)+1),
     log = "y",
     xaxt = "n",
     )
axis(1, at = 1:nrow(ann), labels = rownames(ann))
count = 1
for(i in names(rad.ann)){
    points(x =  (count-1) + ( 1:length(rad.ann[[i]]) ) / 18,
                y = rad.ann[[i]], 
                type = "l"
                )  
  count = count + 1
  }

plot(1:nrow(unb), seq(0.0001, 1, length.out = nrow(unb)),
     ylab = "Relative abundance",
     xlab = "Year",
     type = "n",
     las = 2,
     xlim = c(0, nrow(unb)+1),
     log = "y",
     xaxt = "n",
)
axis(1, at = 1:nrow(unb), labels = rownames(unb))
count = 1
for(i in names(rad.unb)){
  points(x =  (count-1) + ( 1:length(rad.unb[[i]]) ) / 18,
         y = rad.unb[[i]], 
         type = "l"
  )  
  count = count + 1
}

