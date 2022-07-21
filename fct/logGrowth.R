logGrowth <- function(t,y,p){
  # t time
  # y initial condition
  # p is a list of the parameters
  N <- y[1]
  with(as.list(p),{
    dN.dt <- r * N * (1 - a*N)
    return(list(dN.dt))})
}
