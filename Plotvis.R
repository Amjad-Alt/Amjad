#Data viz 
#Amjad Altuwayjiri
#19-Oct-2020

# load packages, (preinstalled in RStudio Cloud)
library(ggplot2)
library(tidyverse)
# These are not strictly necessary, but we'll use them for looking at some specific examples 
library(munsell)
library(RColorBrewer)
library(Hmisc)

# See the colors we got 
display.brewer.all(type = "seq")
display.brewer.all(type = "qual")
display.brewer.pal(9, "BuPu")
myBuPu <- brewer.pal(9, "BuPu")[c(4,5,7)]
myBuPu

#My data.

clothes <- read.csv("Womens_Clothing.csv")

#Plots
# Bar chart show the age of customers rate 5. 
rating_five <- clothes %>% 
               filter(Rating == 5)

  ggplot(rating_five, aes(Age)) +
  geom_bar()
  
##Univariate plot - Histogram
  ggplot(rating_five, aes(Age)) +
    geom_histogram(binwidth = 1, boundary = 1)

# Plot shows wich department is favered acoording to age
  
  ggplot(rating_five, aes(Age, Department.Name )) +
    geom_point(shape = 16, alpha = 0.6,
               size = 3, color = "pink")  
  
  