rm(list= ls())

# Loading Libraries

library(data.table)
library(tidyverse)


# Loading data
data <- read_csv("https://osf.io/4ay9x/download")

#  Using the occupational code 5400 - Receptionists and information clerks

dt <- setDT(data)

dt <- dt[occ2012 == 5400]

