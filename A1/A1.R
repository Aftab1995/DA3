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

# Dropping the observations with PhD education as it doesn't make sense for a doctorate to be a receptionist or an information clerk
dt <- dt[grade92 != 46]

# Also dropping the observations with a professional degree for the same reasons

dt <- dt[grade92 != 45]

# Creating levels for grade92 variable in a new variable educ
# Since the job is receptionist/information clerks, it makes more sense to drill down into lower education 
#levels than higher ones, based on the w and grade92 plot above

dt[grade92 <= 38, educ := "no diploma"] # for education levels of less than 12th grade
dt[grade92 == 39, educ := "high school"] # High school, diploma, GED
dt[grade92 == 40, educ := "some college"] # college education without degree
dt[grade92 == 41 | grade92 == 42, educ := "associate degree"] # Some kind of an associate degree
dt[grade92 == 43, educ := "bachelors"]
dt[grade92 == 44, educ := "masters"] # master, professional degree, PhD

# Creating factor sex variable

dt[sex == 1, gender := "male"]
dt[sex == 2, gender := "female"]

# Creating factor variable for marital variable

dt[marital <= 2, married_status := "married"]
dt[marital <= 6 & marital >= 3, married_status := "separated"]
dt[marital == 7, married_status := "never married"]

# Dividing race into white and other

dt <- dt[race == 1, race_dummy := "white"]
dt <- dt[race != 1, race_dummy := "other"]

### Checking interaction between several variables

datasummary( w*factor(race_dummy)*gender ~ N + Percent() + Mean, data = dt ) 
# It seems like wage is not very different based on race_dummy and gender, so no need for an interaction term for this

datasummary( w*factor(educ)*gender ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on education and gender

datasummary( w*factor(educ)*factor(race_dummy)*gender ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on education, race_dummy and gender

datasummary( w*unionmme*gender ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on being a union member and gender

datasummary( w*married_status*gender ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on marriage status and gender

datasummary(w*stfips*unionmme  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on state and being a union member. However, since the number of states is around 50, 
# adding an interaction between states and uionmme will blow up the number of variable son the right hand side

# Since there are too many states and it is difficult to have interaction terms for those, I am creating regions with multiple states, as per the US BLS

dt <- dt[stfips %in% c("WA", "OR", "MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM", "HI", "AK"), region := "west"]
dt <- dt[stfips %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH"), region := "mid-west"]
dt <- dt[stfips %in% c("OK", "TX", "AR", "LA", "KY", "TN", "MS", "AL", "WV", "VA", "NC", "SC", "GA", "FL", "Dc","MD","DE"), region := "south"]
dt <- dt[stfips %in% c("PA", "NY", "VT", "NH", "ME","MA","RI","CT","NJ"), region := "north-east"]

datasummary(w*region*unionmme  ~ N + Percent() + Mean, data = dt )
# Since there are difference in wages for regions and being a unionmme, we will use an interaction term for this

datasummary(w*class*unionmme  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on class and being a union member

datasummary(w*prcitshp*unionmme  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on citizenship state and being a union member, 
#especially for non-US citizens and Born in PR or Outlying area

# based on above interaction, creating a new dummy for born in PR or outlying US to interact it with unionmme

dt <- dt[prcitshp == "Native, Born in PR or US Outlying Area", pr_born := "yes"]
dt <- dt[prcitshp != "Native, Born in PR or US Outlying Area", pr_born := "no"]

# Checking the interaction of this new variable with unionmme
datasummary(w*pr_born*unionmme  ~ N + Percent() + Mean, data = dt )
# There is significant difference in mean wage based on pr_born and being a union member

datasummary(w*factor(race_dummy)*unionmme  ~ N + Percent() + Mean, data = dt )
# It seems like wage is not very different based on race_dummy and being a union member, so no need for an interaction

datasummary(w*factor(ownchild)*gender  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on presence of children below 18 and gender, especially for men
# The variables ownchild and chilpres give the same thing more or less, however, childpres has levels based on ages, so we
# will use the ownchild variable

# Since the count of observations for ownchild greater than or equal to 4 are very small, will drop those out as outliers
dt <- dt[ownchild <= 3]

datasummary(w*factor(race_dummy)*factor(educ)  ~ N + Percent() + Mean, data = dt )
# It seems like wage is different based on race_dummy and education, especially for higher education levels

datasummary(w*factor(race_dummy)*factor(married_status)  ~ N + Percent() + Mean, data = dt )
# It seems like wage is not very different based on race_dummy and married status, so no need for an interaction for this


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

model3 <- as.formula(w ~ educ + age + agesq + gender + race_dummy + ownchild + unionmme + married_status + class + pr_born + class + region)
# This model contains all the variables that we believe may impact the wage of an individual

model4 <- as.formula(w ~ educ + age + agesq + gender + race_dummy + ownchild + unionmme + married_status + class + pr_born + class + region + 
                        ownchild*gender + gender*educ + gender*unionmme + gender*married_status + gender*race_dummy*educ +
                        race_dummy*educ +
                        unionmme*class + pr_born*unionmme + region*unionmme)
# This model contains everything plus interaction terms for gender, race_dummy, and unionmme

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
  ggthemes::theme_economist()

# plotting results
ggplot(dt, aes(x=predict(reg2, dt), y=w)) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  scale_x_continuous(limits = c(0,30)) + 
  scale_y_continuous(limits = c(0,60)) +
  ggthemes::theme_economist()

