# Class: using taxize and doing classification on cestes database data ----------------
library("taxize") #had to install
library("dplyr") #already had it

# Loading data -----------------------------------------------------
splist <- read.csv("data/raw/cestes/splist.csv")
dim(splist)
head(splist)
splist$TaxonName

# Doing classification according to ncbi --------------------------------
# package taxize, package classification
classification_data <- classification(splist$TaxonName,db="ncbi")
classification_data #list of dataframes
str(classification_data)
length(classification_data) #56 elements = 56 species

# let us explore how to get data on a specific species
classification_data$`Arisarum vulgare`
classification_data[[1]]

# we want to extract the family to match to a larger phylogenetic tree
# filter for first element
tibble_ex <- classification_data[[1]] %>%
             filter(rank == "family") %>%
             select(name) #returns a  data.frame

# to do it for every species, we will use a for loop

# our function to extract family
extract_family <- function(x){
  if(!is.null(dim(x))){ #checking for NAs
    y <- x %>%
        filter(rank == "family") %>%
        pull(name) #returns a  data.frame
    return(y)
  }
}

#the actual extracting and adding to a list
families <- vector() #here we will put family data
for(i in 1:length(classification_data)){ #for each element
  f <- extract_family(classification_data[[i]])
  if(length(f) > 0){ #if the length is zero
    families[i] <- f
  }
}

families





