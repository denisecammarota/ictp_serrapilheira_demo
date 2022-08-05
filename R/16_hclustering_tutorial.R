# Class: Multivariate analysis --------------------------
# Following the tutorial
# As always in case you read: I always comment a lot to explain
# myself what exactly we are doing in each step of the tutorial

library(vegan) # already had installed
library(cluster) # in base R

# Loading the data and env ------------------------------
data("dune")
data("dune.env")
table(dune.env$Management)

# Dissimilarity indexes calculation -----------------------
# package vegan, functions vegandist and dist

# Bray distance
bray_distance <- vegdist(dune)
# Chord distance (ie euclidean, normalized to 1)
chord_distance <- dist(decostand(dune,"norm"))

# Cluster analysis ----------------------------------------
# package cluster, hclust
?hclust #complete is default in this function, change to avg

# Using average, with default clustering method
b_cluster <- hclust(bray_distance,method="average")
c_cluster <- hclust(chord_distance,method="average")

# Now we need to plot the dendograms of how elements were divided
# Note to self: change window size so this looks nicer
par(mfrow = c(1, 2))
plot(b_cluster)
plot(c_cluster)
par(mfrow = c(1, 1))

# Prettier plot
par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
par(mfrow = c(1, 1))







