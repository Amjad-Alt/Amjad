# Women Clothing Analysis
# Amjad Altuwayjiri
# 14-Oct-2020


library(tidyverse)
clothes <- read.csv("Womens_Clothing.csv")
summary(clothes)
glimpse(clothes)


# What is the mean of the rate?
## Change from integer to numeric 
Rating <- as.numeric(clothes$Rating)

## calculate the mean
rating_mean <-mean(sum(Rating)/length(Rating))


# Does age of buyers affect their rating?
##change values into numeric
Age <- as.numeric(clothes$Age)

##group the age

Ages_group <- clothes$Agecat1<-cut(clothes$Age, c(15,20,30,40,50,60,70,80,90,100))

              
## Plot shows the rate and the age.

ggplot(Ages_group, aes(x = Agecat1, y = Rating))+
  geom_point()


# Does the type of purchase affect the rating?









