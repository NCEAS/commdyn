# --------------------------------------------
# Function to calculate diversity partitions by groups
# --------------------------------------------
fn.divpart.groups<-function(
  data.table, #data.frame site by species table with grouping variables
  group.names=array(dim=0), #group names, vector of character strings
  var.names.to.exclude=array(dim=0), #non spp names
  div.metric.type, #diversity metric to use ("alpha", "beta", or "gamma")
  q.order, #order q, (0 is presence absence, 1 is shannon, 2 is simpson diversity metric)
  wts=FALSE){
  # -- load vegetarian package
  require(vegetarian)
  
  # -- make a species list
  spp.list<-names(data.table)[!names(data.table)%in%c(group.names,
                                                      var.names.to.exclude)]
  dat.spp<-data.table[,spp.list]
  
  # -- make a list of grouping variables
  grouping.list<-list()
  if(length(group.names)==0){
    grouping.list[[1]]<-rep(1,nrow(dat.spp))}else{
      for (i in group.names){
        grouping.list[[i]]<-data.table[,i]
      }
    }
  
  # create a vector to group data for analysis
  grouping.vect<-apply(data.frame(grouping.list),
                       1,
                       paste,
                       collapse="_")
  
  # create a matrix to return group ID information
  dat.group.IDs<-data.frame(
    ID=unique(grouping.vect),
    t(data.frame(strsplit(unique(as.character(grouping.vect)),"_"))),
    row.names=NULL)
  names(dat.group.IDs)<-paste("group",c("ID",group.names),sep="_")
    
  # calculate diversity metric
  div.metric.val<-c(by(data=dat.spp,
       INDICES=grouping.vect,
       FUN=d,
       wts=wts,
       lev=div.metric.type,
       q=q.order,
       boot=FALSE))
  N.obs<-c(by(data=dat.spp,
                       INDICES=grouping.vect,
                       FUN=nrow))
  if(div.metric.type=="beta"){
    div.metric.std<-div.metric.val/N.obs
  }else{
    div.metric.std=NA
  }
  
  # return a data table
  return(data.frame(dat.group.IDs,
                    div.metric.type=div.metric.type,
                    q.order=q.order,
                    div.metric=div.metric.val,
                    N.obs=N.obs,
                    beta.std=div.metric.std,
                    row.names=NULL))
}