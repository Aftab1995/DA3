rm(list= ls())

# Loading Libraries

library(data.table)
library(tidyverse)
library(modelsummary)
library(fixest)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(cowplot)

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

# Filtering on uhours to work on full time employee (minimum 40 hours)

dt <- dt[uhours >= 40]

# Looking at a quick summary of some of the key variables
datasummary(w + grade92 + age ~ Mean + SD + Min + Max + P25 + P75 + N , data = dt)

# Finding missing values

to_filter <- sapply(dt, function(x) sum(is.na(x)))
to_filter[to_filter > 0]
# We have 734 NAs in the ethnic column out of the total 901 observations. 
# It would perhaps make sense to not include this variable in the regression analysis.
# This, however, will take out the aspect of ethnicity from our regression analysis. 


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

# The two graphs, more or less, show a linear association between w and the two variables, but using age squared could be helpful

# Creating age squared column

dt <- dt[, agesq := age^2]

# Creating levels for grade92 variable in a new variable educ
# Since the job is receptionist/information clerks, it makes more sense to drill down into lower education 
#levels than higher ones, based on the w and grade92 plot above

dt[grade92 <= 38, educ := "no diploma"] # for education levels of less than 12th grade
dt[grade92 == 39, educ := "high school"] # High school, diploma, GED
dt[grade92 == 40, educ := "some college"] # college education without degree
dt[grade92 == 41 | grade92 == 42, educ := "associate degree"] # Some kind of an associate degree
dt[grade92 == 43, educ := "bachelors"]
dt[grade92 >= 44, educ := "masters or more"] # master, professional degree, PhD

# Creating factor sex variable

dt[sex == 1, gender := "male"]
dt[sex == 2, gender := "female"]

# Creating factor variable for marital variable

dt[marital <= 2, married_status := "married"]
dt[marital <= 6 & marital >= 3, married_status := "separated"]
dt[marital == 7, married_status := "never married"]


### Checking interaction between several variables

datasummary( w*factor(race)*gender ~ N + Percent() + Mean, data = dt ) 
# It seems like wage is different based on race and gender

datasummary( w*factor(educ)*gender ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on education and gender

datasummary( w*factor(educ)*factor(race)*gender ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on education, race and gender

datasummary( w*unionmme*gender ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on being a union member and gender

datasummary( w*married_status*gender ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on marriage status and gender

datasummary(w*stfips*unionmme  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on state and being a union member

datasummary(w*class*unionmme  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on class and being a union member

datasummary(w*prcitshp*unionmme  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on citizenship state and being a union member, 
#especially for non-US citizens and Born in PR or Outlying area

datasummary(w*factor(race)*unionmme  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on race and being a union member

datasummary(w*factor(ownchild)*gender  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on presence of children below 18 and gender, especially for men
# The variables ownchild and chilpres give the same thing more or less, however, childpres has levels based on ages, so we
# will use the ownchild variable

datasummary(w*factor(race)*factor(educ)  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on race and education

datasummary(w*factor(race)*factor(married_status)  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on race and married status


###############
# Regressions #
###############

# Running the most basic regression - wage per hour on education

reg1 <- feols(w ~ educ, data = dt , vcov="hetero")
# This is the most basic regression with education as the prediction variable. Basic understanding suggests
# that wage per hour can be higher for higher education levels

reg2 <- feols(w ~ educ + age + agesq + gender, data = dt , vcov="hetero" )
# This regression contains education, age, and square term of age to factor in the change in wage levels with a higher age.
# It also contains the gender variable as wage may be different for both genders.

reg3 <- feols(w ~ educ + age + agesq + gender + race + ownchild + unionme + married_status + stfips + class + prcitship , data = dt , vcov="hetero" )
# This








