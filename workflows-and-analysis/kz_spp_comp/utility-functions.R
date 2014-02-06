#install.packages("dataone")

# Download a data.frame from a DataONE repository based on its persistent identifier (PID)
download_from_dataone <- function(pid) {
  library(dataone)

  cli <- D1Client()               # Initialize a client to interact with DataONE
  obj0 <- getD1Object(cli, pid)   # Directly fetch our data file from the repository
  d0 <- asDataFrame(obj0)         # Convert it to a data frame (assumes it is well-documented and in CSV format)
  head(d0)
  
  return(d0)
}