# Silver price
# Amjad Altuayjiri
# 10-Oct-2020


# Load the library----
library(tidyverse)
library(Quandl)

#My data----
silver <- Quandl("LBMA/SILVER", api_key="--54GNetvPhzHjJBva6Z", start_date="2020-04-09")

# knowing the data----
summary(silver)
glimpse(silver)

# mean----
USD_mean <- silver %>% 
  summarise(avg = sum(USD)/ length(USD))

# Can we can the moving average:
zoo::rollmean(silver$EURO, 7)
# mean(silver$EURO[1:7])
# mean(silver$EURO[2:8])

GBP_mean <- silver %>% 
  summarise(avg = sum(GBP)/ length(GBP))


EURO_mean <- silver %>% 
  summarise(avg = mean(EURO))
EURO_mean



#variance----
USD_var <- var(silver$USD)
      
GBP <- silver$GBP
GBP_var <- sum((GBP - GBP_mean)^2)/(length(GBP) - 1)
var(GBP)

EURO <- silver$EURO
EURO_var <- sum((EURO - EURO_mean)^2)/(length(EURO) - 1)
var(EURO) 
            


#standard deviation----
USD_sd <- sqrt(USD_var)
sd(USD)

GBP_sd <- sqrt(GBP_var)
sd(GBP)

EURO_sd <- sqrt(EURO_var)
sd(EURO)

# We can do all of the above in simple commands:
silver %>% 
  pivot_longer(-Date) %>% 
  group_by(name) %>% 
  summarise(avg = mean(value),
            stdev = sd(value))







#linear model ----
# silver_lm <- lm(formula = Date ~ USD, data = silver)
# silver_lm

USD_lm <- lm(Date ~ USD, data = silver)
plot(silver$Date, silver$USD)
abline(model)


# tidy data----
tidy_silver <- silver %>% 
  pivot_longer(c(USD, GBP, EURO), names_to = "currency", values_to = "price")
tidy_silver

#cheapest price ----
min(tidy_silver$price)

#highest price ----
max(tidy_silver$price)


#plot ----
ggplot(tidy_silver) + 
  geom_line(aes(x = Date , y = price, color = currency))
