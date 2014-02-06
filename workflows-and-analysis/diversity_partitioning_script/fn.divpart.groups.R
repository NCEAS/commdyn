# --------------------------------------------
# Function to calculate diversity partitions by groups
# --------------------------------------------
fn.divpart.groups<-function(
  spp.table, #site by species matrix
  grouping.list=list(), #list of grouping vectors
  div.metric.type, #diversity metric to use ("alpha", "beta", or "gamma")
  q.order, #order q, (0 is presence absence, 1 is shannon, 2 is simpson diversity metric)
  wts=FALSE){
  # -- load vegetarian package
  require(vegetarian)
  
  # -- handle case with no groups
  if(length(grouping.list)==0) grouping.list[[1]]<-rep(1,nrow(spp.table))
  
  # identify names of grouping variables
  group.names<-names(grouping.list)
  
  # create a vector to group data for analysis
  grouping.vect<-apply(data.frame(grouping.list),
        1,
        paste,
        collapse="_")
  
  # create a matrix to return group ID information
  dat.group.IDs<-data.frame(
    ID=unique(grouping.vect),
    t(data.frame(strsplit(unique(grouping.vect),"_"))),
    row.names=NULL)
  names(dat.group.IDs)<-paste("group",c("ID",group.names),sep="_")
  
  # calculate diversity metric
  div.metric.val<-c(by(data=spp.table,
       INDICES=grouping.vect,
       FUN=d,
       wts=wts,
       lev=div.metric.type,
       q=q.order,
       boot=FALSE))
  
  # return a data table
  return(data.frame(dat.group.IDs,
                    div.metric.type=div.metric.type,
                    q.order=q.order,
                    div.metric=div.metric.val,
                    row.names=NULL))
}