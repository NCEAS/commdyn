# -- Eric's script to calculate diversity metrics for Corinna's fish data set

# -- set working directory
new_wd_path<-'/workflows-and-analysis/diversity_partitioning_script'
setwd('workflows-and-analysis/diversity_partitioning_script')
# -- set working directory

# -- reach Corinna's fish data set
dat.obs<-read.csv('NTLfishmatrix.csv')

# -- make a grouping data frame and a species table
dat.groups<-dat.obs[,c("lakeid","year4")]
spp.list<-names(dat.obs)[c(-1,-2)]
dat.spp<-dat.obs[,spp.list]

# -- load script file with function for calculating diversity metric
source('fn.divpart.groups.R')

# ---------------------------
# -- beta diversity by year
# ---------------------------
# -- call function to calculate a diversity metric (alpha, beta, or gamma) for each group
dat.results<-fn.divpart.groups(
  spp.table=dat.spp, #site by species matrix
  grouping.list=list(year=dat.groups$year4), #list of grouping vectors
  div.metric.type="beta",
  q.order=0)

# -- plot diversity metric by group (year)
x.year<-as.numeric(as.character(dat.results$group_year))
y.diversity<-dat.results$div.metric
par(las=2)
plot(y.diversity~x.year,type="l")

# ---------------------------
# -- mean alpha diversity by year
# ---------------------------
# -- call function to calculate a diversity metric (alpha, beta, or gamma) for each group
dat.results<-fn.divpart.groups(
  spp.table=dat.spp, #site by species matrix
  grouping.list=list(year=dat.groups$year4), #list of grouping vectors
  div.metric.type="alpha",
  q.order=2)

# -- plot diversity metric by group (year)
x.year<-as.numeric(as.character(dat.results$group_year))
y.diversity<-dat.results$div.metric
par(las=2)
plot(y.diversity~x.year,type="l")

# ---------------------------
# -- alpha diversity, nested groups
# ---------------------------
# -- call function to calculate a diversity metric (alpha, beta, or gamma) for each group
dat.results<-fn.divpart.groups(
  spp.table=dat.spp, #site by species matrix
  grouping.list=list(year=dat.groups$year4,
                     site=dat.groups$lakeid), #list of grouping vectors
  div.metric.type="alpha",
  q.order=0)

# -- plot diversity metric by group
plot(div.metric~group_year,data=dat.results)

