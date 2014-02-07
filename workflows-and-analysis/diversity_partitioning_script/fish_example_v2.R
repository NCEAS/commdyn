# -- Eric's script to calculate diversity metrics for Corinna's fish data set

# -- reach Corinna's fish data set
dat.obs<-read.csv('workflows-and-analysis/diversity_partitioning_script/NTLfishmatrix.csv')

# -- load script file with function for calculating diversity metric
source('workflows-and-analysis/diversity_partitioning_script/fn.divpart.groups.v2.R')

# ---------------------------
# -- beta diversity by year
# ---------------------------
# -- call function to calculate a diversity metric (alpha, beta, or gamma) for each group
dat.results<-fn.divpart.groups(
  data.table=dat.obs, #site by species matrix
  group.names="year4", #list of grouping vectors
  var.names.to.exclude="lakeid",
  div.metric.type="alpha",
  q.order=0)

# -- plot diversity metric by group (year)
x.year<-as.numeric(as.character(dat.results$group_year4))
y.diversity<-dat.results$div.metric
par(las=2)
plot(y.diversity~x.year,type="l")

# ---------------------------
# -- alpha diversity, nested groups
# ---------------------------
# -- call function to calculate a diversity metric (alpha, beta, or gamma) for each group
dat.results<-fn.divpart.groups(
  data.table=dat.obs, #site by species matrix
  group.names=c("year4","lakeid"), #list of grouping vectors
  div.metric.type="alpha",
  q.order=0)

# -- plot diversity metric by group
plot(div.metric~group_year4,data=dat.results)

