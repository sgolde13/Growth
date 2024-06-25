####################################################################
####################################################################
## Outline
##
## 1) Packages and Setup
## 2) Data Preparation
## 3) Exploratory Data Analysis
##      a) Tables
##      a) Plots
## 4) 



####################################################################
## 1) Packages and Setup
library("readxl")
library("tidyverse")
library("ggplot2") 
library("ggpubr")
library("smplot2")
library("moments")

"%!in%" <- function(x,y)!('%in%'(x,y))



####################################################################
## 2) Data Preparation
df <- read_excel("Growth.xlsx", 
                  col_types = c("text", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric")) %>% 
                  as.data.frame()


# Round variables to two digits. This rounds rgdp60 to the nearest whole number
df[, c("growth", "rgdp60", "tradeshare", "rev_coups", "assasinations")] <- 
        round(df[, c("growth", "rgdp60", "tradeshare", "rev_coups", "assasinations")], digits = 2)



####################################################################
## 3) Exploratory Data Analysis

##########################
##########################
## 3a) Tables

# Check that all countries are represented once
all(table(df$country_name) == 1)

table(df$oil)

summary(df$tradeshare)
summary(df$yearsschool)


df[df$rev_coups == 0 & df$assasinations == 0, ] %>% nrow()


# Moments of y to measure normal shape
skewness(df$growth)
kurtosis(df$growth)


##########################
##########################
## 3b) Plots

ggplot(df, aes(x = growth)) + theme_minimal() +
  geom_histogram(aes(y=..density..), fill = "grey", alpha = 0.75) +
  geom_density(alpha = 0.3) +
  labs(title = "Gross Domestic Product (GDP) Density Plot", 
       x ="GDP", y = "Density",
       caption = "")


ggplot(df, aes(x = tradeshare, y = growth)) + theme_minimal() +
  geom_point() + #xlim(0, 5) +
  scale_fill_discrete(name ="Modeling Method") +
  labs(title = "Average Gross Domestic Product (GDP) Increase", 
       x ="Trade Share", y = "GDP",
       caption = "")


ggplot(df, aes(x = rev_coups, y = assasinations)) +
  geom_point() +
  sm_statCorr(corr_method = "spearman", linetype = "dashed") +
  #stat_smooth(method = "lm", formula = y ~ x, geom = "smooth") +
  #stat_cor(method = "spearman", label.x = 0, label.y = 2.2) +
  theme_minimal() +
  scale_fill_discrete(name ="Modeling Method") +
  labs(title = "Correlation Between Assasinations and Political Unrest", 
       x ="Political Unrest\n", y = "Assasinations",
       caption = "Correlation calculated using the Spearman method. The 'Political Unrest' variable represents the 
       average annual number of revolutions, insurrections (successful or not) and coup d’etats")



####################################################################
## 4)











