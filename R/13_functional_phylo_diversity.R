# Class: functional and phylogenic diversity

library("vegan")

#First code from the slides: Shannon and Simpson -------------------------
#just modified it to use my coding style

community_A <- c(10,6,4,1)
community_B <- c(17,rep(1,7))

diversity(community_A,"shannon") #1.172066
diversity(community_B,"shannon") #1.172066
diversity(community_A,"invsimpson") #2.882353
diversity(community_B,"invsimpson") #1.945946

#Second code from the slides: Renyi ------------------------------
#let us note that these are vectors bc of renyi scales
#the scales are given in the help, ie ?renyi

ren_commA <- renyi(community_A)
ren_commB <- renyi(community_B)
ren_AB <- rbind(ren_commA,ren_commB)
ren_AB #let us see what this matrix is

#plotting the scales and renyi index for each community
matplot(t(ren_AB),type="l",axes = F)
box()
axis(side = 2)
axis(side = 1,labels= c(0, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, "Inf"),
     at = 1:11)
legend("topright", legend=c("Community A","Community B"), lty=c(1,2),
       col=c(1,2))


