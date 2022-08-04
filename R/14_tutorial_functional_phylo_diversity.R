# Tutorial: Exercises on functional and phylogenetic diversity
# As always in case you read: I always comment a lot to explain
# myself what exactly we are doing in each step of the tutorial

library("vegan") #already installed
library("cluster") #base in R
library("FD") #had to install
library("SYNCSA") #had to install

# Reading the data, data cleaning -------------------------------
comm <- read.csv("data/raw/cestes/comm.csv")
traits <- read.csv("data/raw/cestes/traits.csv")

# let us visualize some columns of comms and traits
head(comm[,1:6])
head(traits[,1:6])

# the names of these rows are just 1,2,3,4,etc
rownames(comm)[1:6]

# let us now change this non-informative names
rownames(comm) <- paste0("Site", comm[,1]) # adding site
comm <- comm[,-1] # selecting the first column
head(comm)[,1:6] # checking our output

# exercise: do the same for traits, indicating the corresponding species
rownames(traits) # checking the names are just sp1,sp2,sp3,etc
rownames(traits) <- traits[,1] # no need to paste anything else
                              # we just assign to rownames the first column
traits <- traits[,-1]
head(traits)[,1:6]

# Species richness with vegan ----------------------------------
# calculating simple species richness per site
# package vegan, function specnumber
richness <- vegan::specnumber(comm)
richness

# Taxonomic diversity ------------------------------------------
# calculating taxonomic diversity (related to number of taxa)
# package vegan, function diversity from last class
# we use shannon and simpson indexes to get diversity per site
shannon <- vegan::diversity(comm)
simpson <- vegan::diversity(comm, index = "simpson")
# checking values
shannon #higher values per site
simpson #lower values per site

# Functional diversity -----------------------------------------
# calculating new diversity index: functional diversity (related to traits)
# package cluster, function daisy to compute gower distance
gow <- cluster::daisy(traits, metric = "gower")
gow
# package FD, function gowdis to compute gower distance
gow2 <- FD::gowdis(traits)
gow2
# checking if they're identical in every way
identical(gow,gow2) #they are not

# comparing objects gow and gow2
# they are from different classes
class(gow)
class(gow2)

#however, they have the same values
plot(gow, gow2, asp = 1) #same values


# Rao's quadratic entropy calculations -------------------------
# calculating Rao's quadratic entropy
# package SYNCSA, function rao.diversity
tax <- rao.diversity(comm) #without traits
fun <- rao.diversity(comm, traits = traits) #with traits
# let us see what it returns
tax
fun
# further info on difference and what it returns
?rao.diversity
#plotting the results
plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1) #identity line

#package FD, function dbFD
#we can use the distance matrix to calculate functional diversity indices
FuncDiv1 <- FD::dbFD(x = gow, a = comm, messages = F)
#the returned object has Villéger's indices and Rao calculation
names(FuncDiv1)
#We can also do the calculation using the traits matrix directly
FuncDiv <- FD::dbFD(x = traits, a = comm, messages = F)


