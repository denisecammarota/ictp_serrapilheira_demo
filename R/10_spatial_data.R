# Class 10: Introduction to spatial data in R

#Importing the packages and data -----------------------------
library("sf") #sf library to deal with spatial data
library("tmap") #tmap to plot, like ggplot
library("dplyr") #dplyr for data
library("raster") #raster for raster spatial data
data(World) #world map data

#first let us check what type of object is this
class(World) #sf and dataframe
str(World) #177 obs of 16 variables
names(World) #names characteristics
dplyr::glimpse(World) #similar to before characteristics

#Plotting Maps with tmap -----------------------------------------

tm_shape(World) + tm_borders()


#Plotting World Maps with base -------------------------------------------
plot(World) #plots map along with all characteristics
plot(World[1]) #plots characteristic 1: iso_a3
plot(World[,1]) #plots characteristic 1: iso_a3

#Plotting ARG Maps with base -------------------------------------------
plot(World[5,]) #plots for a country with all characteristics
                #in this case, i chose ARG
plot(World[5,1]) #plots for a country with characteristic 1.

#Plotting World Population Estimate (pop_est) with base ----------------------------
plot(World["pop_est"])

#More on exploring geometry data -------------------------------------
World$geometry

class(World$geometry)

#extracting coordinates
head(sf::st_coordinates(World))

#dropping geometry just gives us a dataframe
no_geom <- sf::st_drop_geometry(World)
class(no_geom)

#getting bounding boxes
st_bbox(World)

#Manipulating and plotting sf objects -----------------------------------------------
names(World)
unique(World$continent)

#filtering, only plotting South America
World %>%
  filter(continent == "South America") %>%
  tm_shape() +
  tm_borders()


#create new variables and use them in our maps
#same code, i just added argentina
World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX","ARG"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")


World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX"), "red",if_else(iso_a3 %in% c("ARG"), "blue", "grey"))) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries")

# Loading, plotting and saving shapefiles from the disk ------------------------
library(rnaturalearth)
library(rnaturalearthhires)
bra <- ne_states(country = "brazil", returnclass = "sf")
plot(bra)

dir.create("data/shapefiles", recursive = TRUE)
#the delete belows allows us to rewrite the shapefile since, if not, there's an error
st_write(obj = bra, dsn = "data/shapefiles/bra.shp", delete_layer = TRUE)

#this is for reading a shapefile
bra2 <- read_sf("data/shapefiles/bra.shp")
class(bra2)
plot(bra2)

# Loading, ploting, and saving a raster from the disk -----------------------------
dir.create(path = "data/raster/", recursive = TRUE)

#plotting max temperature over 12 months
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)

#exploring raster data
is(tmax_data) #the data are a raster stack, several rasters piled

dim(tmax_data) #dimension

#some other extra functions
extent(tmax_data)

res(tmax_data)
