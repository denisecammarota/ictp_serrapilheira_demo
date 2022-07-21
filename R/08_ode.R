# Class 9
# A bunch of different things

#importing libraries
#intall.packages(deSolve) i had to do this before to install package
library(deSolve)
library(ggplot2) # because we will plot things
library(tidyr) # because we will manipulate some data

# Loading function from our fct folder
source('fct/logGrowth.R')
source('fct/LVComp.R')

# Logistic Growth Model ---------------------------

#vector of parameters
p<- c(r=1,a=0.001)
#initial condition
y0 <- c(N=10)
#time steps
t <- 1:20

# give the function and the parameters to the ode function
out_log <- ode(y = y0, times = t, func = logGrowth, parms = p)

head(out_log)

#converting to dataframe and plotting with ggplot
df_log <- as.data.frame(out_log)
ggplot(df_log) +
  geom_line(aes(x = time, y = N)) +
  theme_classic()


# Lotka-Volterra competition model --------------------------

# LV parameters
a <- matrix(c(0.02, 0.01, 0.01, 0.03), nrow = 2)
r <- c(1, 1)
p2 <- list(r, a)
N0 <- c(10, 10)
t2 <- c(1:100)

# give the function and the parameters to the ode function
out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
head(out_lv)

#putting this into a tidy data format, before we need to convert to data frame
df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3)

#plotting with ggplot
ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()







