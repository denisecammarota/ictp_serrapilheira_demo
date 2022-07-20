#Model selection

library(bbmle)
library(ggplot2)
# Loading data
cuckoo <- read.csv("data/raw/valletta_cuckoo.csv")

cuckoo

# Create models to represent each hypotesis -----------------

h1 <- glm(Beg ~ Mass, data = cuckoo,
          family = poisson(link = log))

h2 <- glm(Beg ~ Mass + Species, data = cuckoo,
          family = poisson(link = log))

h3 <- glm(Beg ~ Mass * Species, data = cuckoo,
          family = poisson(link = log))

h0 <- glm(Beg ~ 0, data = cuckoo,
          family = poisson(link = log))


summary(h1)
summary(h2)


AIC(h3)
AIC(h0)

bbmle::AICtab(h0,h1,h2,h3)


bbmle::AICtab(h0,h1,h2,h3,base=TRUE,weights=TRUE)


# Predicted values ------------------------------------------
#Calculating the predicted values
newdata <- expand.grid(Mass = seq(min(cuckoo$Mass), max(cuckoo$Mass), length.out = 200),
                       Species = unique(cuckoo$Species)) #all possible combinations
newdata$Beg <- predict(h3, newdata, type = 'response') #filling every line of the last with the predicted values

#explore predict.glm ?predict.glm

p <- ggplot(mapping=aes(x=Mass,y=Beg,colour=Species)) + geom_point(data=cuckoo) + geom_line(data=newdata)+theme_classic()
p
