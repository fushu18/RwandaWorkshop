################################################################################
#
#                  Gender Data Lab Workshop in Rwanda
#
#                       Shu Fu (SJTU) and PARIS21
#
#                             Aug 13, 2024
#
################################################################################

### TOPIC: Visualization

# Dataset: gapminder

## load package
library(tidyverse)
library(gapminder)
library(ggthemes)

## take a look at the data by head()
gap <- gapminder 
head(gap)

gap_rwanda <- gap %>%
  filter(country == "Rwanda")

## relations between GDP per capita and life expectency
ggplot(data = gap, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()

## color by continent
ggplot(data = gap, aes(x = gdpPercap, y = lifeExp, color = continent)) + 
  geom_point() +
  scale_color_manual(values = c("gold", "lightblue", "red", "lightgreen", "pink"))

## point with opaque
ggplot(data = gap, aes(x = gdpPercap, y = lifeExp, color = continent, shape = continent)) + 
  geom_point(alpha = 0.5)

## Layers

ggplot(data = gap, aes(x = year, y = lifeExp, by = country, color = continent)) + 
  geom_line() + 
  geom_point()

ggplot(data = gap, aes(x = year, y = lifeExp, by = country)) + 
  geom_line(aes(color = continent)) + 
  geom_point()

## Labels
ggplot(data = gap, aes(x = gdpPercap, y = lifeExp, color=continent)) + 
  geom_point(alpha = 0.5) + 
  labs(x = "GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development, by Continent")

## themes
ggplot(data = gap, aes(x = gdpPercap, y = lifeExp, color = continent)) + 
  geom_point(alpha = 0.5) + 
  labs(x = "GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development, by Continent") +
  theme_few()

## transformation
ggplot(data = gap, aes(x = log(gdpPercap), y = lifeExp, color = continent)) + 
  geom_point(alpha = 0.5) + 
  labs(x = "Logged GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development, by Continent") +
  theme_few()

## with data manipulation
gap %>%
  filter(continent == "Africa") %>% 
  mutate(rwanda = ifelse(country == "Rwanda", "RW", "Others")) %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(aes(shape = rwanda), alpha = 0.5) + 
  geom_smooth(method = "lm") + # we can even add a regression line
  facet_wrap(~year) +
  labs(x = "Logged GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development in Africa") +
  theme_few()

## shape
gap %>%
  filter(continent == "Africa" & year == 2007) %>% 
  mutate(rwanda = ifelse(country == "Rwanda", "Rwanda", "Others")) %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(aes(shape = rwanda, color = rwanda), alpha = 0.5) + 
  geom_smooth(method = "lm", show.legend = FALSE) +
  scale_color_manual(values = c("black", "red")) +
  scale_shape_manual(values = c(16, 8)) +
  labs(x = "Logged GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development in Africa") +
  theme_few()

## facet_wrap
ggplot(data = gap, aes(x = log(gdpPercap), y = lifeExp, color = continent)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ continent) +
  labs(x = "Logged GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development, by Continent") +
  theme_few()

ggplot(data = gap, aes(x = log(gdpPercap), y = lifeExp, color = continent)) + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ continent, ncol = 5) +
  labs(x = "Logged GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development, by Continent") +
  theme_few()

## by year
gap %>%
  filter(continent == "Africa") %>% 
  mutate(rwanda = ifelse(country == "Rwanda", "Rwanda", "Others")) %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(aes(shape = rwanda, color = rwanda), alpha = 0.5) + 
  geom_smooth(method = "lm") +
  facet_wrap(~year) +
  scale_color_manual(values = c("black", "red")) +
  scale_shape_manual(values = c(16, 8)) +
  labs(x = "Logged GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development in Africa") +
  theme_few()

# legend
gap %>%
  filter(continent == "Africa") %>% 
  mutate(rwanda = ifelse(country == "Rwanda", "Rwanda", "Others")) %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(aes(shape = rwanda, color = rwanda), alpha = 0.5, show.legend = FALSE) + # HERE!
  geom_smooth(method = "lm") +
  facet_wrap(~year) +
  scale_color_manual(values = c("black", "red")) +
  scale_shape_manual(values = c(16, 8)) +
  labs(x = "Logged GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development in Africa") +
  theme_few() 

gap %>%
  filter(continent == "Africa") %>% 
  mutate(rwanda = ifelse(country == "Rwanda", "Rwanda", "Others")) %>%
  ggplot(aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(aes(shape = rwanda, color = rwanda), alpha = 0.5) + 
  geom_smooth(method = "lm") +
  facet_wrap(~year) +
  scale_color_manual(values = c("black", "red")) +
  scale_shape_manual(values = c(16, 8)) +
  labs(x = "Logged GDP per capita (in US$)", y = "Life Expectancy (in years)", 
       title = "Relations of Life Expectancy and Ecomonic Development in Africa",
       color = "", shape = "") +
  theme_few() +
  theme(legend.position = "bottom") # position



# Visualization Using Rwanda Gender Data

### 1 Legislators by gender

# load data
setwd("/Users/ShuFu/Desktop/RwandaWorkshop/data/")

parliament <- read_csv("parliament_tidy.csv")

ggplot(data = parliament, aes(x = year, y = legislators, color = sex)) +
  geom_line(size = 0.5) +       
  geom_point(size = 3) +      
  facet_wrap(~chamber) +
  labs(title = "Legislators by Year, Sex, and Chamber",
       x = "Year", y = "Percentage") +
  scale_color_manual(name = "Gender", values = c(F = 'pink', M = 'royalblue')) +
  theme_few()

## challenge: How can you make it better?




### 2 Population, Age, and Gender by District

# load data
setwd("/Users/ShuFu/Desktop/RwandaWorkshop/data/")

population <- read_csv("population_age_gender.csv")

# Make age as ordered catagories
age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                "30-34", "35-39", "40-44", "45-49", "50-54", 
                "55-59", "60-64", "65-69", "70-74", "75-79", 
                "80-84", "85+")

population <- population %>%
  mutate(age_and_district = factor(age_and_district, levels = age_groups, ordered = TRUE))

population %>%
  filter(district == "Bugesera") %>%
  ggplot(aes(x = age_and_district, y = population, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(name = "Gender", values = c("pink", "royalblue")) +
  scale_y_continuous(labels = abs) +
  labs(x = "", y = "Population", 
       title = "Population Distribution by Age and Gender") +
  theme_few() +
  theme(legend.position="bottom")

# challenge: pick "Bugesera", "Nyarugenge", "Gasabo", "Kicukiro" and facet_wrap by district

