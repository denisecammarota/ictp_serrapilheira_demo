# Importing relevant libraries ---------------------------------

library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)

# Retrieving data for a species -----------------------------------
species <- "Myrsine coriacea"

#getting the information for specific species, this takes a while
#so this is why it is limited
occs <- occ_search(scientificName = species,
                   limit = 100000)
dim(occs)
names(occs)

myrsine.data <- occs$data

colnames(myrsine.data)

#checking: 187 characteristics of registry for 5068 registries
myrsine.data

# Exporting raw data ----------------------------------------------

dir.create("data/raw/", recursive = TRUE)
write.csv(myrsine.data,
          "data/raw/myrsine_data.csv",
          row.names = FALSE)

# Checking species taxonomy ---------------------------------------

#seeing that there were so many names for this species in gbif database
sort(unique(myrsine.data$scientificName))

#how many names are accepted
table(myrsine.data$taxonomicStatus)

#which ones are accepted and which not + collections
table(myrsine.data$scientificName, myrsine.data$taxonomicStatus)

#check if taxonomic name changes in gbif are correct with comparison
#to the The Plant List nomenclature checking
species.names <- unique(myrsine.data$scientificName)
dim(species.names)

#doing the checking
#it gives us a lot of warnings related to the authors mostly
tax.check <- TPL(species.names)

#assessing the output
tax.check

#here we can change the modifications that have been done to our data
species.names

#introducing the modifications we checked for
# creating new object w/ original and new names after TPL
new.tax <- data.frame(scientificName = species.names,
                      genus.new.TPL = tax.check$New.Genus,
                      species.new.TPL = tax.check$New.Species,
                      status.TPL = tax.check$Taxonomic.status,
                      scientificName.new.TPL = paste(tax.check$New.Genus,
                                                     tax.check$New.Species))
# now we are merging raw data and checked data
#since now we have a shared column, we can merge
#this shared column is "scientificName"
myrsine.new.tax <- merge(myrsine.data, new.tax, by = "scientificName")

#comment from andrea: this could also be done w/ a left join from dplyr

#exporting our modified data
dir.create("data/processed/", recursive = TRUE)
write.csv(myrsine.new.tax,
          "data/processed/data_taxonomy_check.csv",
          row.names = FALSE)


# Plotting with species coordinates ------------------------------------

#plotting with coordinates and map where species is
#the asp is a sort of rescaling for it to be aesthetically kinda accurate
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE) #actually adds the map

#CoordinateCleaner just cleans and checks for common coordinates errors

#for that, we first have to check we have no NA in latitudes or longitudes
myrsine.coord <- myrsine.data[!is.na(myrsine.data$decimalLatitude)
                              & !is.na(myrsine.data$decimalLongitude),]

#correcting the coordinates
#clean means it only returns the potentionally correct coordinates
geo.clean <- clean_coordinates(x = myrsine.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "species",
                               value = "clean")

#plotting the before vs after cleaning data
par(mfrow = c(1, 2))
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE)
plot(decimalLatitude ~ decimalLongitude, data = geo.clean, asp = 1)
map(, , , add = TRUE)
par(mfrow = c(1, 1))

#flag and save the corrected data
myrsine.new.geo <- clean_coordinates(x = myrsine.coord,
                                     lon = "decimalLongitude",
                                     lat = "decimalLatitude",
                                     species = "species",
                                     value = "spatialvalid")
#merging raw and cleaned data
myrsine.new.geo2 <- merge(myrsine.data, myrsine.new.geo,
                          all.x = TRUE,
                          by = "key")
#plotting after the merge
plot(decimalLatitude ~ decimalLongitude, data = myrsine.new.geo, asp = 1)
map(, , , add = TRUE)

#exporting data after all the coordinate corrections and checks
write.csv(myrsine.new.geo2,
          "data/processed/myrsine_coordinate_check.csv",
          row.names = FALSE)

#homework andrea gave us:
#convert to shapefile, plot like we did yesterday from this data
#steps:
# 1) create the sh object going from decimal coordinates to polygon in df
# 2) having sth like this, how to add it to yesterday's map? (tmap)
# 3) explore tmap_mode
