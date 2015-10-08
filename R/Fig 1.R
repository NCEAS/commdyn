# Figure 1 of codyn methods paper

# Goal: make a figure which compares non-temporal measures of diversity (richness, rank abundance) with temporal measures (tunoer, rank clock, MRS, rate change).

# use Konza data from Collins et al. 2008 Ecology as the example data set.
library(vegan)
library(codyn)
library(reshape)

data(collins08)


# Initial richness measures. Use transpose_community to create plot x species data frames for vegan
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

# rich.unb <- apply(unb, 1, specnumber)

rich.ann <- apply(ann, 1, function(x) length(x[x>0]))

#rich <- t(tapply(collins08$species, list(collins08$replicate, collins08$year), FUN = function(x)
#          length(unique(x))))
# melt(rich)

shan.unb <- apply(unb, 1, diversity)
shan.ann <- apply(ann, 1, diversity)

rad.unb <- radfit(unb)
rad.ann <- radfit(ann)

plot(rad.unb)
plot(rad.ann)

data(BCI)
mod <- radfit(BCI[3,])
mod
plot(mod, pch=".")
radlattice(mod)

# Make figures like 08 fig 1. Relative abundance.

rad.ann <- apply(ann, 1, function(x) sort(
        x[x>0]/max(x),
        decreasing = T))

rad.unb <- apply(unb, 1, function(x) sort(
  x[x>0]/max(x),
  decreasing = T))

plot(1:nrow(ann), seq(0.00001, 1, length.out = nrow(ann)),
     ylab = "Relative abundance",
     xlab = "Year",
     type = "n",
     las = 1,
     log = "y"
     )

points(x = (1:length(rad.ann$`1984`))/18,
       y = rad.ann$`1984`, 
       type = "l"
       )

# plot(x = (1:length(rad.ann$`1984`))/18,
#        y = rad.ann$`1984`, log = "y", type = "b")

lapply(rad.ann,
       function(x)
         points(x = (1:length(rad.ann$`1984`))/18,
                y = rad.ann$`1984`, 
                type = "l"
         )
       
