require(XML)
require(plyr)
#fileName <- system.file("exampleData", "systems.xml", package="XML")
#xmlTreeParse(fileName)

doc<-xmlParse("systems.xml",useInternalNodes = TRUE)

exo_list<-xmlToList(doc)
names(exo_list)
ldply(xmlToList(doc), function(x) {data.frame(x[!names(x)=="name"]) } )

# Very dummy, should be improved

N_p<-c()
for (i in 1:1228){
N_p[i]<-length(which(attr(unlist(exo_list[i]$system),"")=="star.planet.discoverymethod" | attr(unlist(exo_list[i]$system),"")=="binary.binary.star.planet.discoverymethod"|
                    attr(unlist(exo_list[i]$system),"")=="binary.star.planet.discoverymethod" | attr(unlist(exo_list[i]$system),"")=="binary.planet.discoverymethod"|
                     attr(unlist(exo_list[i]$system),"")=="planet.discoverymethod"|
                      attr(unlist(exo_list[i]$system),"")=="binary.binary.planet.discoverymethod"))
}
systemnames<-c()
for (i in 1:1228){
  systemnames[i]<-exo_list[i]$system$name
}

exo_cat<-matrix(nrow=1128,ncol=5)
for (i in 1:1228){
  exo_cat[i,1]<-exo_list[i]$system$name
  exo_cat[i,2]<-exo_list[i]$system$star$metallicity
  exo_cat[i,3]<-exo_list[i]$system$star$mass
  exo_cat[i,4]<-exo_list[i]$system$star$temperature
}


a1<-xmlToDataFrame(
  getNodeSet(doc, "//system/")
)


do.call(rbind, xpathApply(doc, "/system", function(node) {
  
 name <- xmlValue(node[["name"]])
  temperature<- xmlValue(node[["temperature"]])

  if (is.null( temperature))  temperature <- NA
  
  data.frame(name, temperature, stringsAsFactors = FALSE)
  
}))