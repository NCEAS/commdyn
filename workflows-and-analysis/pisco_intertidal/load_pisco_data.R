library(dataone)

# Initialize a client to interact with DataONE
cli <- D1Client()

# search for data packages, listing both data and metadata relevant, and only request versions that are not obsolete
results <- d1SolrQuery(cli, list(q="id:*pisco_intertidal.50.6 -obsoletedBy:*",fl="identifier,title,author,documents,resourceMap"))
sr <- xmlParse(results)
sr

# A function that downloads the 
getDataFrame <- function(pkg, pid) {
    obj1 <- getMember(pkg, pid)
    d1 <- asDataFrame(obj1)
    return(d1)
}

# Retrieve a whole data package and its contents, and list its identifiers
pkg <- getPackage(cli, "resourceMap_pisco_intertidal.50.6")
getIdentifiers(pkg)

# Create data frames for each of the data objects
d1 <- getDataFrame(pkg, "doi:10.6085/AA/pisco_intertidal.32.8")
head(d1)

d2 <- getDataFrame(pkg, "doi:10.6085/AA/pisco_intertidal.45.2")
head(d2)

d3 <- getDataFrame(pkg, "doi:10.6085/AA/pisco_intertidal.31.8")
head(d3)

d4 <- getDataFrame(pkg, "doi:10.6085/AA/pisco_intertidal.33.3")
head(d4)

d4 <- getDataFrame(pkg, "doi:10.6085/AA/pisco_intertidal.33.3")
head(d4)

# Download the metadata as well
obj5 <- getMember(pkg, "doi:10.6085/AA/pisco_intertidal.50.6")
getFormatId(obj5)
metadata <- xmlParse(getData(obj5))
metadata
