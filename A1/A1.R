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

####Setting up the models

model1 <- as.formula(w ~ educ)
# This is the most basic model with education as the prediction variable. Basic understanding suggests
# that wage per hour can be higher for higher education levels

model2 <- as.formula(w ~ educ + age + agesq + gender)
# This regression contains education, age, and square term of age to factor in the change in wage levels with a higher age.
# It also contains the gender variable as wage may be different for both genders.

model3 <- as.formula(w ~ educ + age + agesq + gender + race + ownchild + unionmme + married_status + stfips + class + prcitshp + ind02 + class )
# This model contains all the variables that we believe may impact the wage of an individual

model4 <- as.formula(w ~ educ + age + agesq + gender + race + ownchild + unionmme + married_status + stfips + class + prcitshp + ind02 + class +
                        ownchild*gender + gender*race + gender*educ + gender*unionmme + gender*married_status + gender*race*educ +
                        race*educ + race*married_status +
                        unionmme*stfips + unionmme*class + unionmme*prcitshp + unionmme*race)
# This model contains everything plus interaction terms for gender, race, and unionmme

### Running the regressions
reg1 <- feols(model1, data = dt , vcov="hetero")
reg2 <- feols(model2, data = dt , vcov="hetero" )
reg3 <- feols(model3, data = dt , vcov="hetero" )
reg4 <- feols(model4, data = dt , vcov="hetero" )



# evaluation of the models: using all the sample
fitstat_register("k", function(x){length( x$coefficients ) - 1}, "No. Variables")           
etable( reg1 , reg2 , reg3 , reg4 , fitstat = c('aic','bic','rmse','r2','n','k'), keepFactors = TRUE )


#####################
# Cross-validation for better evaluation of predictive performance
# Simple k-fold cross validation setup:
# 1) Used method for estimating the model: "lm" - linear model (y_hat = b0+b1*x1+b2*x2 + ...)
# 2) set number of folds to use (must be less than the no. observations)
k <- 4

# We use the 'train' function which allows many type of model training -> use cross-validation
set.seed(111)
cv1 <- train(model1, dt, method = "lm", trControl = trainControl(method = "cv", number = k))

# Check the output:
cv1
summary(cv1)
cv1$results
cv1$resample

set.seed(111)
cv2 <- train(model2, dt, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(111)
cv3 <- train(model3, dt, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(111)
cv4 <- train(model4, dt, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")

# Calculate RMSE for each fold and the average RMSE as well
cv <- c("cv1", "cv2", "cv3", "cv4")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2)/4)
}


# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
                     rbind(cv1$resample[1], rmse_cv[1]),
                     rbind(cv2$resample[1], rmse_cv[2]),
                     rbind(cv3$resample[1], rmse_cv[3]),
                     rbind(cv4$resample[1], rmse_cv[4])
)

colnames(cv_mat)<-c("Resample","Model1", "Model2", "Model3", "Model4")
cv_mat 

# Show model complexity and out-of-sample RMSE performance
m_comp <- c()
models <- c("reg1", "reg2", "reg3", "reg4")
for( i in 1 : length(cv) ){
  m_comp[ i ] <- length( get( models[i] )$coefficient  - 1 ) 
}

m_comp <- tibble( model = models , 
                  complexity = m_comp,
                  RMSE = rmse_cv )

ggplot( m_comp , aes( x = complexity , y = RMSE ) ) +
  geom_point(color='red',size=2) +
  geom_line(color='blue',size=0.5)+
  labs(x='Number of explanatory variables',y='Averaged RMSE on test samples',
       title='Prediction performance and model compexity') +
  theme_bw()

# plotting results
ggplot(dt, aes(x=predict(reg2, dt), y=w)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  scale_x_continuous(limits = c(0,60)) + 
  scale_y_continuous(limits = c(0,60))

