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
display.brewer.pal(6,"Set2")
# need to learn how to have your own color
mycol <- brewer.pal(3, "Set2")
#mycol <- plot_hex(#66c2a5
  #fc8d62
  #8da0cb)
  
munsell::plot_hex(myBuPu)

#My data.

clothes <- read.csv("Womens_Clothing.csv")

#Plots
# Bar chart shows the age of customers rated 1. 
#need a tittle
bad_rating <- clothes %>% 
               filter(Rating == 1)

  ggplot(bad_rating, aes(Age)) +
    geom_bar(fill= "dark red", color="white") +
    
    labs(x ="Age of customers",
         y = "Number of customers") +
    theme_test()
  
  
# Plot shows witch department favored by 20th age
  age_20 <- clothes %>% 
    filter(Age == 20)
  age_20 %>% head
  
#Need to add colors
  #organize the background

  ggplot(age_20, aes(Department.Name , Division.Name)) +
    geom_jitter(shape = 1,width = 0.6, alpha = 0.5,
                size = 2 )+
    scale_colour_manual("Department.Name", values = "Set2")+
    scale_x_discrete(labels = "Bottoms",
                              "Dresses",
                              "Intimate",
                              "Jackets",
                              "Tops",
                              "Trend") +
    theme_bw()
 
  
# split same last data
  #needs colors
  #get red of squares
  #organize the names
  #choose better shape and alpha
  
  ggplot(age_20, aes(Department.Name , Division.Name, color = vore)) +
    geom_jitter(shape = 1,width = 0.6, alpha = 0.5,
                size = 2)+
    facet_wrap(~ Department.Name)+
    theme_bw()
  
  
  
# plot shows the connection between positive feedback and rating
  
 dresses <- clothes %>% 
    filter(Department.Name == "Dresses") 
 
#maybe its better to show it in normal distribution

  ggplot(dresses, aes(Rating, Positive.Feedback.Count)) +
    geom_point(position = position_jitter(seed = 300))
    
  
#   