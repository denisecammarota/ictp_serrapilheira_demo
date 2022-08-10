# Exercises -------------------------------------------------------
library("dplyr")
library(vegan)

#Exercise 1: the 5 most abundant species using cestes
#reading the community matrix
comm <- read.csv("data/raw/cestes/comm.csv")
#summing all rows (sum over the sites)
comm_sp_sum <- colSums(comm)
#sorting with dplyr
sort(comm_sp_sum,decreasing=TRUE)


#Exercise 2: how many species are in each sites (plot)?
#first we change abundance > 1 to 1
comm_st <- data.frame(comm)
comm_st[-1,] <- ifelse(comm_st[-1,]>=1,1,0)
#then we sum by rows instead of columns
comm_st_sum <- rowSums(comm_st)
#and now we put it in a fancy way
res_df <- data.frame(comm$Sites,comm_st_sum)
res_df

#Exercise 3: which is the species that is most abundant in each site?
#first we have the original data comm
comm_nsites <- comm[,2:length(comm)]
m_comm <- as.matrix(comm_nsites)
max.col(m_comm,ties.method='first')


# Exercise 4: Shannon diversity index
# Andrea tips: use a vector at least as an input

shannon_diversity <- function(vec){
  # calculates shannon diversity given a vector
  vec <- vec[vec>0]
  sum_vec <- sum(vec) # total abundance
  pi <- vec/sum_vec # calculating relative abundance
  H <- -sum(pi*log(pi))
  return(H)
}

shannon_diversity_comm <- function(comm){
  # calculates shannon diversity per site given community matrix
  shannon_site <- c()
  for(i in seq(1,nrow(comm))){
    shannon_site[i] <- shannon_diversity(comm[i,2:ncol(comm)])
  }
  return(shannon_site)
}

# calculating shannon index for each site
shannon_site <- shannon_diversity_comm(comm)
shannon_res <- data.frame(comm$Sites,shannon_site)

# visualize the results and sites
shannon_res

# checking with vegan
shannon_res_vegan <- diversity(comm[,2:57])
shannon_res_vegan

# seeing it is actually the same results
all(shannon_site == shannon_res_vegan)

# Exercise 5: Simpson's diversity index
# I will repeat more or less the same reasoning

simpson_diversity <- function(vec){
  # calculates simpson diversity given a vector
  vec <- vec[vec>0]
  sum_vec <- sum(vec) # total abundance
  pi <- vec/sum_vec # calculating relative abundance
  H <- 1-sum(pi*pi)
  return(H)
}

simpson_diversity_comm <- function(comm){
  # calculates simpson diversity per site given community matrix
  simpson_site <- c()
  for(i in seq(1,nrow(comm))){
    simpson_site[i] <- simpson_diversity(comm[i,2:ncol(comm)])
  }
  return(simpson_site)
}

# calculating simpson index per site
simpson_site <- simpson_diversity_comm(comm)
simpson_res <- data.frame(comm$Sites,simpson_site)

# visualize the results and sites
simpson_res

# comparing with vegan package
simpson_res_vegan <- diversity(comm[,2:57],index="simpson")
simpson_res_vegan

# checking they are the exact same for each site
all(simpson_site == simpson_res_vegan)
