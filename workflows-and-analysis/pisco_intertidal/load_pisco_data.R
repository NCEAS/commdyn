library(dataone)

# Initialize a client to interact with DataONE
cli <- D1Client()

# search for data packages, listing both data and metadata relevant, and only request versions that are not obsolete
# for this PISCO search, it allows us to discover the identifier for the package resource map, used below
results <- d1SolrQuery(cli, list(q="id:*pisco_intertidal.50.6 -obsoletedBy:*",fl="identifier,title,author,documents,resourceMap"))
sr <- xmlParse(results)
sr

# A function that downloads the 
getDataFrame <- function(pkg, pid) {
    obj1 <- getMember(pkg, pid)
    d1 <- asDataFrame(obj1)
    return(d1)
}

# Retrieve the PISCO point count data package, and list its identifiers
pkg <- getPackage(cli, "resourceMap_pisco_intertidal.50.6")
getIdentifiers(pkg)

# Create data frames for each of the data objects
d1 <- getDataFrame(pkg, "doi:10.6085/AA/pisco_intertidal.32.8")
d2 <- getDataFrame(pkg, "doi:10.6085/AA/pisco_intertidal.45.2")
d3 <- getDataFrame(pkg, "doi:10.6085/AA/pisco_intertidal.31.8")
d4 <- getDataFrame(pkg, "doi:10.6085/AA/pisco_intertidal.33.3")

# Download the metadata as well
obj5 <- getMember(pkg, "doi:10.6085/AA/pisco_intertidal.50.6")
getFormatId(obj5)
metadata <- xmlParse(getData(obj5))
metadata
