# Importing relevant libraries ---------------------------------

library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)
# for the homework assignment, the sf and tmap
# we worked with in script 10
library("sf")
library("tmap")
library("dplyr")
library("raster")
# for the homework assignment, World data
data(World)

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

# Homework ----------------------------------------------------------

# Let us first explore myrsine.new.geo2 and myrsine.coords
names(myrsine.new.geo2)
dim(myrsine.new.geo2)

names(myrsine.coord)
dim(myrsine.coord)

# Then we join like we did before with merge to get total correct data
myrsine.new.geo3 <- merge(myrsine.coord, myrsine.new.geo2,
                          all.x = TRUE,
                          by = "key")

dim(myrsine.new.geo3) # consistent with dimensions we should have

# Conversion to sf and set coordinate system
?st_as_sf #library sf
?st_set_crs #library sf

# Need to know the coords, which columns
names(myrsine.new.geo3)

myrsine.shapefile <- st_as_sf(myrsine.new.geo3,coords=c("decimalLongitude","decimalLatitude"))

# Set coordinate system
myrsine.shapefile <- st_set_crs(myrsine.shapefile,value=4326) # checked tutorial to know what epsg value is
st_crs(myrsine.shapefile)

# Now we should save the shapefile like in script 10
dir.create("data/shapefiles", recursive = TRUE)
st_write(obj = myrsine.shapefile, dsn = "data/shapefiles/myrsinesf.shp",
         delete_layer = TRUE)

# Plot with tmap like in script 10
# First plot South America
tmap_mode("plot")
plot_sa <- World %>%
  filter(continent %in% c("South America")) %>%
  tm_shape() +
  tm_borders()

# Now plot our shapefile here
plot_sa + tm_shape(myrsine.shapefile) + tm_bubbles(size = 0.2,
          col = ".summary")

# Finally, they asked us to use the tmap_mode to make an interactive map
?tmap_mode() #seeing that either plot or view are modes

tmap_mode("view") #set to interactive viewing
plot_sa + tm_shape(myrsine.shapefile) + tm_bubbles(size = 0.2,
          col = ".summary")


