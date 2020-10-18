# Women Clothing Analysis
# Amjad Altuwayjiri
# 14-Oct-2020


library(tidyverse)
clothes <- read.csv("Womens_Clothing.csv")
summary(clothes)
glimpse(clothes)
getwd()
setwd("C:/Users/amjad/Documents/Misk/Amjad")



# What is the mean of the rate?
## Change from integer to numeric 
Rating <- as.numeric(clothes$Rating)

## calculate the mean
rating_mean <-mean(sum(Rating)/length(Rating))


# Does age of buyers affect their rating?
##change values into numeric
Age <- as.numeric(clothes$Age)

##group the age
  
age_group <- clothes %>%
                  group_by(Age) %>% 
                  cut(Age, breaks=c(20, 30, 40, 50, 60, 70, 80), right = FALSE)
                  cut(Age, breaks=c(20, 30, 40, 50, 60, 70, 80), right = FALSE, labels = FALSE)
class(age_group) 


# Does the type of purchase affect the rating?









