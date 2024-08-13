################################################################################
#
#                  Gender Data Lab Workshop in Rwanda
#
#                       Shu Fu (SJTU) and PARIS21
#
#                             Aug 13, 2024
#
################################################################################

### TOPIC: Reproducible Data, Data Cleaning, Tidy Data


# load packages
library(tidyverse)

# Data Subsetting

## Create a dataset by vectors
name <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Terrestrial planet", 
          "Gas giant", "Gas giant", "Gas giant", "Gas giant")
diameter_ratio <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

planets <- data.frame(name, type, diameter_ratio, rings, stringsAsFactors = F)
planets

## Subsetting with []
planets[1, 3] # Print out diameter of Mercury (row 1, column 3)

planets[4, ] # Print out data for Mars (entire fourth row)

planets[1:3, 1:2] # Print first three rows of the first two columns

## Subsetting with $
planets$rings # Print out variable of rings

## Conditional Subsetting
diameter_ratio <- planets$diameter_ratio
diameter_ratio > 3

planets[planets$diameter_ratio > 3, ] # chooose planets whose diameter_ratio is larger than 3




# Data Tranformation with pipes %>%

## celect
planets2 <- planets %>% 
  select(name, diameter_ratio)

planets2

## filter
planets3 <- planets %>% 
  filter(rings == FALSE) %>%
  select(name, diameter_ratio)

planets3

## mutate
planets4 <- planets %>% 
  mutate(diameter_ratio2 = round(diameter_ratio, 2))

planets4

planets5 <- planets %>% 
  mutate(radius = 4000 * diameter_ratio) %>%
  mutate(volume = (4 / 3) * pi * (radius ^ 2))

planets5

## ifelse
planets6 <- planets %>%
  mutate(earth = ifelse(name == "Earth", 1, 0))

planets6






# Tidy Data

## create a wide data on legislators' gender
wide <- data.frame(
  year = c(1990, 1994, 2001, 2003, 2008, 2011, 2013, 2018, 2021, 2023),
  deputiesF = c(17, 14, 23, 49, 56, 56, 64, 61, 61, 61),
  deputiesM = c(83, 86, 77, 51, 44, 44, 36, 39, 39, 39)
)
wide

## create a long data on legislators' gender
long <- data.frame(
  year = c(1990, 1994, 2001, 2003, 2008, 2011, 2013, 2018, 2021, 2023,
           1990, 1994, 2001, 2003, 2008, 2011, 2013, 2018, 2021, 2023),
  sex = c("F", "F", "F", "F", "F", "F", "F", "F",  "F", "F",
          "M", "M", "M", "M", "M", "M", "M", "M", "M", "M"),
  deputies = c(17, 14, 23, 49, 56, 56, 64, 61, 61, 61,
               83, 86, 77, 51, 44, 44, 36, 39, 39, 39)
)
long

## Transform wide to long
wideF <- wide %>%
  select(year, deputiesF) %>%
  rename(deputies = deputiesF) %>%
  mutate(sex = "F") %>%
  select(year, sex, deputies)

wideM <- wide %>%
  select(year, deputiesM) %>%
  rename(deputies = deputiesM) %>%
  mutate(sex = "M") %>%
  select(year, sex, deputies)

long <- rbind(wideF, wideM) # rbind means row bind
long















