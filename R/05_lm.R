# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Script to fit linear model in R
# First version 2022-07-18
# --------------------------------------------------#

# loading packages
library(ggplot2)

# reading data
cat <- read.csv("data/raw/crawley_regression.csv")

# Do leaf chemical compounds affect the growth of caterpillars? ----------------

# the response variable
boxplot(cat$growth, col = "darkgreen")

# the predictor variable
unique(cat$tannin)

# creating the lm
mod_cat <- lm(growth ~ tannin, data = cat)

summary(mod_cat)


## ----lm-plot------------------------------------------------------------------
plot(growth ~ tannin, data = cat, bty = 'l', pch = 19,las=1)
abline(mod_cat, col = "red", lwd = 2)
#abline(a=coef[1],b=coef[2],...)

## ----lm-ggplot----------------------------------------------------------------
ggplot(data = cat, mapping = aes(x = tannin, y = growth)) +
  geom_point() +
  geom_smooth(method = lm) + #adding curve from our model, specifying our method is linear model
  theme_classic()


## AOV table
## checking the variance partitioning of our model
summary.aov(mod_cat)


## fitted values
predict(mod_cat)
cat$fitted <- predict(mod_cat)

# Comparing fitted vs. observed values
# after actually calculating each predicted value
# the other abline didn't do that, it jut plotted the line somehow
ggplot(data = cat) +
  geom_point(aes(x = growth, y = fitted)) +
  geom_abline(aes(slope = 1,  intercept = 0)) +
  theme_classic()


## Model diagnostics -----------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod_cat)
# first one checking if residuals are close to zero, homogeneity of the variance
# second one (q-q) shows us if the residuals follow a normal distribution
# third one shows us how the variance of residuals is distributed, horizontal line expected
# fourth one is a measurement of how big is the variance from the residuals, we expect them in an interval not higher than 3
par(mfrow = c(1, 1))


# Comparing statistical distributions ------------------------------------------
library(fitdistrplus)

data("groundbeef")
?groundbeef
str(groundbeef)


plotdist(groundbeef$serving, histo = TRUE, demp = TRUE)

descdist(groundbeef$serving, boot = 1000)

fw <- fitdist(groundbeef$serving, "weibull")
summary(fw)

fg <- fitdist(groundbeef$serving, "gamma")
fln <- fitdist(groundbeef$serving, "lnorm")

par(mfrow = c(2, 2))
plot_legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot_legend)
qqcomp(list(fw, fln, fg), legendtext = plot_legend)
cdfcomp(list(fw, fln, fg), legendtext = plot_legend)
ppcomp(list(fw, fln, fg), legendtext = plot_legend)


gofstat(list(fw, fln, fg))
