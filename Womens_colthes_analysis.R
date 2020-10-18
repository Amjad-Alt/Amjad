# Women Clothing Analysis
# Amjad Altuwayjiri
# 14-Oct-2020


library(tidyverse)
clothes <- read.csv("Womens_Clothing.csv")
summary(clothes)
glimpse(clothes)
getwd()
setwd("C:/Users/amjad/Documents/Misk/Amjad")



# What is the mean rate?
## Change from integer to numeric 
Rating <- as.numeric(clothes$Rating)

## calculate the mean
rating_mean <-mean(sum(Rating)/length(Rating))


# How age of buyers affect their rating?
##change values into numeric
Age <- as.numeric(clothes$Age)

##group the age
age_group <- clothes %>%
                   mutate(Age = case_when(Age >= 80  & Age <= 89 ~ 'eighties',
                                          Age >= 70  & Age <= 79 ~ 'seventies',
                                          Age >= 60  & Age <= 69 ~ 'sixties',
                                          Age >= 50  & Age <= 59 ~ 'fifties',
                                          Age >= 40  & Age <= 49 ~ 'fourties',
                                          Age >= 30  & Age <= 39 ~ 'thirties',
                                          Age >= 20  & Age <= 29 ~ 'twenties'))

age_group$'thirties'
  
 
# Most purchase and its rate.




# Divide bad reviews into categorys 




