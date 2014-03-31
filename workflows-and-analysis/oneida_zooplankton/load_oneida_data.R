library(dataone)

# Initialize a client to interact with DataONE
cli <- D1Client()

# search for data packages, listing both data and metadata relevant, and only request versions that are not obsolete
# for this search, it allows us to discover the identifier for the package resource map, used below
results <- d1SolrQuery(cli, list(q="id:*kgordon.17.* -obsoletedBy:*",fl="identifier,title,author,documents,resourceMap"))
sr <- xmlParse(results)
sr

# A function that downloads the data for a pid and assumes its parseable into a data.frame
getDataFrame <- function(pkg, pid) {
    obj1 <- getMember(pkg, pid)
    d1 <- asDataFrame(obj1)
    return(d1)
}

# Retrieve the PISCO point count data package, and list its identifiers (note that this list was also in the search results above)
pkg <- getPackage(cli, "resourceMap_kgordon.17.56")
getIdentifiers(pkg)

# Create data frames for each of the data objects
d1 <- getDataFrame(pkg, "cbfs.21.6")
d2 <- getDataFrame(pkg, "cbfs.22.14")
d3 <- getDataFrame(pkg, "cbfs.132.5")
d4 <- getDataFrame(pkg, "cbfs.25.11")

# Download the metadata as well
obj5 <- getMember(pkg, "kgordon.17.56")
getFormatId(obj5)
metadata <- xmlParse(getData(obj5))
metadata

# TODO: Aggregate the species counts by site, year, plot, and species
