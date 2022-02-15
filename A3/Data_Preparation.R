# Set up environment ------------------------------------------------------

rm(list=ls())

# Please change dir to your own and unzip bisnode firm panel data in data/raw folder

dir <- "C:/Users/Aftab/Courses/DA3/Git-DA3/DA3/"


# Import libraries
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
#install.packages("gmodels")
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)
library(tidyverse)
library(lubridate)
library(RColorBrewer)

# load functions
source(paste0(dir, "A2/theme_bg.R"))
source(paste0(dir,"A2/da_helper_function.R"))
color <- c(brewer.pal( 3, "Set2" )[1], brewer.pal( 3, "Set2" )[2], brewer.pal( 3, "Set2" )[3], brewer.pal( 3, "Set2" )[5])

# Import Data -------------------------------------------------------------

data <- read_csv("A3/cs_bisnode_panel.csv")

# drop variables with too many NAs more than 200k and filter years between 2012-2014 
data <- data %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages, D)) %>%
  filter(year >= 2010,
         year <= 2015)

# Label Engineering -------------------------------------------------------

# generate status_alive to check the firm is still alive
data  <- data %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>%
           as.numeric(.))


# Create log sales and sales in million
# We have negative sales values
summary(data$sales)

data <- data %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales = ifelse(sales > 0, log(sales), 0),
         sales_mil=sales/1000000,
         sales_mil_log = ifelse(sales > 0, log(sales_mil), 0))


# Filter out non-alive firms
data <- data %>%
  filter(status_alive == 1) %>%
  # look at firms below 10m euro revenues and above 1000 euros
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.001))

# Keep only firms with data for the 3 years
data <- data %>% group_by(comp_id) %>% filter(n() == 4)

# Change in sales
data <- data %>%
  group_by(comp_id) %>%
  mutate(d1_sales_mil_log = sales_mil_log - Lag(sales_mil_log, 1) ) %>%
  ungroup()


# replace w 0 for new firms + add dummy to capture it
data <- data %>%
  mutate(age = (year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .),
         d1_sales_mil_log = ifelse(new == 1, 0, d1_sales_mil_log),
         new = ifelse(is.na(d1_sales_mil_log), 1, new),
         d1_sales_mil_log = ifelse(is.na(d1_sales_mil_log), 0, d1_sales_mil_log))

data <- data %>%
  mutate(flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < -1.5, 1, 0),
         flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
         d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 1.5, 1.5, d1_sales_mil_log)))

# CAGR sales change in the last 2 years
data <- data %>%
  group_by(comp_id) %>%
  mutate(cagr_sales = ((lead(sales_mil,2) / sales_mil)^(1/2)-1)*100)

ggplot(data)+
  geom_histogram(aes(cagr_sales))

data <- data %>%
  filter(year == 2012,
         cagr_sales != is.na(cagr_sales),
         cagr_sales <= 3000)

describe(data$cagr_sales)
describe(data$comp_id)

ggplot(data=data, aes(x=cagr_sales)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
                 color = "black", fill = "deepskyblue4") +
  coord_cartesian(xlim = c(-100, 200)) +
  labs(x = "CAGR growth",y = "Percent")+
  #scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  #scale_x_continuous(expand = c(0.00,0.00),limits=c(0,500), breaks = seq(0,500, 50)) +
  theme_bw() 

# Create fast growth dummy
data <- data %>%
  group_by(comp_id) %>%
  mutate(fast_growth = (cagr_sales > 40) %>%
           as.numeric(.)) %>%
  ungroup()

describe(data$fast_growth)

data <- data %>%
  mutate(age = (year - founded_year))

