# -- make a list of data.frames for the cleaned data sets
dat.obs.list<-list()
dat.obs.list[["NTL"]]<-read.csv('workflows-and-analysis/diversity_partitioning_script/NTLfishmatrix.csv')
dat.obs.list[["portal"]]<-read.csv('workflows-and-analysis/portal-rodents/Portal_wide.csv',row.names=1)
dat.obs.list[["CAP"]]<-read.table("workflows-and-analysis/CAP_arthropods/siteXspecies_CAP_arthropods.txt")
dat.obs.list[["BBS"]]<-read.csv('workflows-and-analysis/BBS/BBS_wide.csv',row.names=1)

# -- grouping variables
group.var.list<-list(
  NTL="year4",
  portal="date",
  CAP="sample_date",
  BBS="Year")

# -- other variables to exclude from analysis
group.exclude.list<-list(
  NTL="lakeid",
  portal=c("period","plot"),
  CAP="site_id",
  BBS="Route")

# -- call the diversity partition function
source('workflows-and-analysis/diversity_partitioning_script/fn.divpart.groups.v2.R')

# -------------------------------------
# -- alpha diversity
# -------------------------------------
# -- make a list to store results
results.list<-list()
# -- a loop to calculate beta diversity for each data set
for(i in names(dat.obs.list)){
  results.list[[i]]<-fn.divpart.groups(
    data.table=dat.obs.list[[i]], #site by species matrix
    group.names=group.var.list[[i]], #list of grouping vectors
    var.names.to.exclude=group.exclude.list[[i]],
    div.metric.type="alpha",
    q.order=0)
}
alpha.list<-results.list

# -------------------------------------
# -- beta diversity
# -------------------------------------
results.list<-list()
# -- a loop to calculate beta diversity for each data set
for(i in names(dat.obs.list)){
  results.list[[i]]<-fn.divpart.groups(
    data.table=dat.obs.list[[i]], #site by species matrix
    group.names=group.var.list[[i]], #list of grouping vectors
    var.names.to.exclude=group.exclude.list[[i]],
    div.metric.type="beta",
    q.order=0)
}
beta.list<-results.list



# -- make plots
par(mfrow=c(4,1),mar=c(2,4,1,1))
for(i in names(results.list)){
  plot(beta.list[[i]]$rel.beta.rate,type="l",ylab="beta")
  legend(x="topleft",legend=i,bty="n")
}

