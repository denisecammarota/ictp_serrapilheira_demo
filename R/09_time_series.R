# Class 9
# Time series analysis

# Time Series -----------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate) #dealing with dates
library(zoo) #dealing with time series
#install.package(zoo) had to install package actually

# Loading data
covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")
head(covid)

# Checking the class of date column
class(covid$date)

# Formatting into data format for date
covid$date <- as_date(covid$date)
class(covid$date)

# Now we can use several functions to operate with dates such as:

range(covid$date)

summary(covid$date)

# Plotting time series with ggplot

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()

# There are negative numbers for new confirmed cases
# We will replace those for zero

covid$new_confirmed[covid$new_confirmed < 0] <- 0

# Now we will plot again without negative numbers
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  labs(x = "Date", y = "New cases")

# To change how we dispay the date in our dataset
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() + scale_x_date(breaks="4 months",date_labels="%Y-%m")
  labs(x = "Date", y = "New cases")

# Rolling mean for 2 weeks, filling with NA as padding (output and input to have same dimension)

covid$roll_mean <- zoo::rollmean(covid$new_confirmed, 14, fill = NA)

head(covid)

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() + scale_x_date(breaks="4 months",date_labels="%Y-%m") + labs(x = "Date", y = "New cases")

+ geom_line(aes(x=date,y=roll_mean,color='red'))
