# Exercises -------------------------------------------------------
library("dplyr")

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


#Exercise 4: Shannon diversity index
shannon_div <- function(comm){
  n_sp <- length(comm) - 1 #number of species
  total_abundance <- sum(comm) #total abundance (all species)
  relative_abundance <- rowSums(m_comm)/total_abundance #sum all rows for species
  pi <- relative_abundance/total_abundance #probabilities pi
  print(total_abundance)
  H <- 0
  for(j in seq(1,length(pi))){
    H <- H - pi[j]*log(pi[j])
  }
  return(H)
}

shannon_div(comm)
