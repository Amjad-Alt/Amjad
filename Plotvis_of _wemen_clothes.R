#Data viz 
#Amjad Altuwayjiri
#19-Oct-2020

# load packages
library(tidyverse)

# These are not strictly necessary, but we'll use them for looking at some specific examples 
library(munsell)
library(RColorBrewer)
library(Hmisc)
library(ggridges)

# See the colors we got 
display.brewer.all(type = "seq")
display.brewer.all(type = "qual")
display.brewer.pal(6,"Set2")

# need to learn how to have your own color
mycol <- brewer.pal(3, "Set2")

myBuPu <- c("#66c2a5", "#fc8d62", "#8da0cb")

plot_hex(myBuPu)

#My data.
# Data from: <put website here>
clothes <- read.csv("Womens_Clothing.csv")

clothes %>% 
  head()

#Plots

## Bar chart shows the age of customers rated 1. 

bad_rating <- clothes %>% 
  filter(Rating == 1)

ggplot(bad_rating, aes(Age)) +
  geom_histogram(fill= "dark red") +
  labs(title = "Angry customers",
       x ="Age of customers",
       y = "Number of customers") +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  theme_test() +
  theme(rect = element_blank())


# Plot shows witch department favored by 20th age
age_20 <- clothes %>% 
  filter(Age == 20)
age_20 %>% 
  head()

posn_j <- position_jitter(width = 0.2, seed = 136)

ggplot(age_20, aes(Department.Name , Division.Name)) +
  stat_sum() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# split the same last data

ggplot(age_20, aes(Department.Name , Division.Name, color = Department.Name)) +
  geom_jitter(shape = 16,width = 0.6, alpha = 0.5,
              size = 2) +
  facet_wrap(~ Department.Name) +
  theme_bw() +
  theme(legend.position = "none", 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank()) 



#plot shows the positive feedback of customers of each department  
#one of ys is missing!
#need better shape and color of the tittle

ggplot(clothes, aes(x = Positive.Feedback.Count ,y = Department.Name,
                    fill = Department.Name , color = Department.Name )) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=10)+
  labs(title = "Positive feedback of each department",
       x = "Feedback Count") +  
  theme_ridges() + 
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.text = element_text(colour = "dark blue"))


##__ Not Finished Yet __##

#plot shows each department recommended in colored bar chart 
#have to manage colors and not stacking!
nrows <- clothes %>% nrow()

ggplot(clothes, aes(Department.Name  ,nrows, fill =Recommended.IND)) +
  geom_bar(position="stack", stat="identity")


#plot shows the mean and distribution of feedback count of each category .

departments <- clothes %>% 
  group_by(Department.Name)  

## change the feedback from integer to numeric
feedback <- as.numeric(departments$Positive.Feedback.Count)

## getting the mean 
smean.sdl(feedback, mult = 1)


#needs some colors 
#one missing x!
#I don't know what does it measure?!!
ggplot(departments, aes(Department.Name, feedback, color = feedback)) +
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 1),
               position = position_dodge(0.3)) +
  scale_color_brewer("Department.Name", palette = "Dark2")
labs(title = "customers feedback") + 
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme_classic(base_size = 10) +
  theme(axis.title.x=element_blank())



# plot shows the connection between positive feedback and rating   