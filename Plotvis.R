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
install.packages("lattice")
install.packages("survival")
install.packages("Formula")

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

## Bar chart shows the age of customers rated 1. 

bad_rating <- clothes %>% 
               filter(Rating == 1)

  ggplot(bad_rating, aes(Age)) +
    geom_bar(fill= "dark red", color="white") +
    labs(title = "Angry customers",
         x ="Age of customers",
         y = "Number of customers") +
    theme_test() +
    theme(rect = element_blank())
  
  
# Plot shows witch department favored by 20th age
  age_20 <- clothes %>% 
    filter(Age == 20)
  age_20 %>% head
  
  posn_j <- position_jitter(seed = 136)

  ggplot(age_20, aes(Department.Name , Division.Name, color = Department.Name)) +
    geom_point(position = posn_j, shape = 16, alpha = 0.3,
               size = 3) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45))
 
  
  
  
  
  
    
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
  
  
#plot shows the mean avrage of buyers of each catogry  
    
  
#   