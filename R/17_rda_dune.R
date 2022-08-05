# Class: Ordinal multivariable methods like PCA/RDA

#Loading data ---------------------------------------------
library(vegan)
library(dplyr)
data("dune")
data("dune.env")

# Standarize data with decostand example -------------------------------------
# package vegan, function decostand

?decostand
chord_distance <- dist(decostand(dune,"norm"))

# this is a distance class object
is(chord_distance)

# Performing RDA on dune -----------------------------------------
norm <- decostand(dune,"norm") # standarize
pca <- rda(norm)

# This returns the position of species and sites on principal axes
# not all of them of course, only axes 1 and 2
plot(pca)

# Tells us the positions of species and sites and importance of each component
# ie how much of the variation the principal components explain
summary(pca)

# Instead, to plot some other 2 principal components (2 and 3, for example)
plot(pca,choices = c(2, 3))

# Performing RDA on environmental data (dune.env) -----------------------------------------
names(dune.env)

# Observing everything is character
apply(dune.env, 2, class)

# Casting to numeric
dune.env$A1 <- as.numeric(dune.env$A1)
dune.env$Moisture <- as.numeric(dune.env$Moisture)
dune.env$Manure <- as.numeric(dune.env$Manure)

# Actually performing RDA
pca_env <- rda(dune.env[,c("A1", "Moisture", "Manure")])

# It's impossible, we have to recode
plot(pca_env,choices = c(2,3))

# Also useful to calculate the correlation with cor
cor(dune.env)
