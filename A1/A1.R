rm(list= ls())

# Loading Libraries

library(data.table)
library(tidyverse)
library(modelsummary)


# Loading data
data <- fread("https://osf.io/4ay9x/download")


################
# Data Munging #
################

#  Using the occupational code 5400 - Receptionists and information clerks

# Filtering the data to have code 5400 observations only
dt <- data[occ2012 == 5400]

str(dt)


# Creating a new wage per hour variable w
dt <- dt[, w := earnwke/uhours]

# Looking at a quick summary of some of the key variables
datasummary(w + uhours + earnwke + grade92 + age ~ Mean + SD + Min + Max + P25 + P75 + N , data = dt)

# Finding missing values

to_filter <- sapply(dt, function(x) sum(is.na(x)))
to_filter[to_filter > 0]
# We have 1231 NAs in the ethnic column out of the total 1478 observations. 
# It would perhaps make sense to not include this variable in the regression analysis.
# This, however, will take out the aspect of race from our regression analysis. 


# Dropping the variable ethnic

dt <- dt[,ethnic := NULL]

# Looking at the distrbitution of wage per hour variable

ggplot(dt) +
  geom_density(aes(x=w)) +
  ggthemes::theme_economist()

# The distribution has a long right tail, hence also looking at the distribution of log of the variable

ggplot(dt) +
  geom_density(aes(x=log(w))) +
  ggthemes::theme_economist()

# Looking quickly at some relationships between wage per hour and age, and wage per hour and grade92(educational degree)

ggplot(dt, aes(x = age, y = w)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x) + 
  ggthemes::theme_economist()

ggplot(dt, aes(x = grade92, y = w)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x) + 
  ggthemes::theme_economist()

# The two graphs, more or less, show a linear association between w and the two variables

###############
# Regressions #
###############


