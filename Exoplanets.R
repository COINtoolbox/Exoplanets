require(XML)
fileName <- system.file("exampleData", "systems.xml", package="XML")
xmlTreeParse(fileName)


doc<-xmlTreeParse("systems.xml")

r <- xmlRoot(doc)

xmlSize(r)

plantcat <- xmlSApply(r, function(x) xmlSApply(x, xmlValue))


# Finally, get the data in a data-frame and have a look at the first rows and columns

plantcat_df <- data.frame(plantcat)
plantcat_df[1:5,1:4]

print(r)[1:2]