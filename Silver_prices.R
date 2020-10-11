# Silver price
# Amjad Altuayjiri
# 10-Oct-2020


# Load the library----
library(tidyverse)

install.packages("Quandl")
library(Quandl)
library(xts)
library(zoo)


#My data----
silver <- Quandl("LBMA/SILVER", api_key="--54GNetvPhzHjJBva6Z", start_date="2020-10-04")

# knowing the data----
summary(silver)
glimpse(silver)


# mean----
USD_mean <- silver %>% 
  summarise(avg = sum(USD)/ length(USD))


GBP_mean <- silver %>% 
  summarise(avg = sum(GBP)/ length(GBP))


EURO_mean <- silver %>% 
  summarise(avg = sum(EURO)/ length(EURO))
EURO_mean



#variance----
USD <- silver$USD
USD_var <- sum((USD - USD_mean)^2)/(length(USD) - 1)
      var(USD)
      
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


#linear model ----
silver_lm <- lm(formula = Date ~ USD, data = silver)
silver_lm

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
