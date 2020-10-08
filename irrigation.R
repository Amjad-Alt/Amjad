#Irrigation Analysis.
#Amjad Altuwayjiri.
# oct 1, 2020
#I have to say that I haven't save my R project when I was typing with you,
#and it suddenly shut down,
#so I tried to write my own...though it is simple I know 
#but I have to say that I have learned programming two months ago
#and I am developing my skills.

#upload tidyverse----
library(tidyverse)

# Get a play data set----
irrigation <- read_csv("data/irrigation_long.csv")


#get to know your data----
summary(irrigation)
glimpse(irrigation)

#Mean development of irrigation in the same region through years----
Africa_num <- irrigation[1:4, 3]
mean_Africa <- mean(sum(Africa_num))
mean_Africa

Europe_num <- irrigation[5:8, 3]
mean_Europe <- mean(sum(Europe_num))
mean_Europe

NAmerica_num <- irrigation[9:12, 3]
mean_NAmerica <- mean(sum(NAmerica_num))
mean_NAmerica

SAmerica_num <- irrigation[5:8, 3]
mean_SAmerica <- mean(sum(SAmerica_num))
mean_SAmerica

#The highest development?
max(mean_Africa, mean_SAmerica, mean_Europe, mean_NAmerica)

# the less ----

min(mean_Africa, mean_SAmerica, mean_Europe, mean_NAmerica)
 





   
   
