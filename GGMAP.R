library(sf) #turn longtitude and latidude into a new variable, a point
library(dplyr)
library(tidyverse)
library(ggthemes)
library(tidygeocoder)#download longtitude and latitude by the name of place you provide
library(terra) #a lot of functions. we will only use unwarp here, but you can explore this package by yourself
library(geodata) # download maps from internet.

setwd("D:")
library(readxl)
data <- read_excel(path = "D:/Data examples/data4theworkshop/Copy of Final Table(Gender)_(1).xlsx", sheet = 8, skip = 1)
View(data)

# As we can see, the data we read just now has 2 rows that we don't want: Rwanda and the Source. We need to drop them.
data_districts <- data[c(2:31), ]
# Also, the column names are not that satisfying. Change them into a more readable way.
colnames(data_districts) <- c("district", "Both", "Male_Headed_HHs", "Female_Headed_HHs")

# Use the package of tidygeocoder, it automatically adds lon and lat to the data
data_coor <- data_districts %>%
  geocode(district)

# some error happens, like the geo-data in Burera, Nyanza and Ngoma. We need to fix them manually.
# make sure you check the data all the times. R is not always correct, some times it can make mistakes.
View(data_coor)

# Nyanza, which is also a name of a province in Kneya.
data_coor[4,5] <- -2.21
data_coor[4,6] <- 29.44

# Burera
data_coor[22,5] <- -1.4905
data_coor[22,6] <- 29.8104
 
# NGOMA
data_coor[29,5] <- -2.11
data_coor[29,6] <- 30.28

# Kirehe
data_coor[28,5] <- -2.15
data_coor[28,6] <- 30.44

#Create a new data table to store the geo-points of the districts
first_column <- data_coor[, 1]
last_two_columns <- data_coor[, (ncol(data_coor)-1):ncol(data_coor)]
geo_data <- data.frame(first_column, last_two_columns)


library(geodata)
gadm('Rwanda', level = 0, path = 'C:/Users/蒋君柳/Desktop/Rwanda', version = 'latest', resolution = 1)
# country name, use three-letter ISO code or full name.
# level, 0 = whole country, 1 = first administrative level (Province for Rwanda), 2 = second admin level (Districts for Rwanda)......
# You need to specify a path to store the data
# version, you can choose from latest or other GADM version, search the version-year online
# resolution, 1 = high detail, 2 = low detail
# gadm() provides you with geography-administrative boundaries, with all countries in the world. The data is stored in a rds file. 

a <- read_rds('C:/Users/蒋君柳/Desktop/Rwanda/gadm/gadm41_RWA_2_pk.rds')
# rds is a special data file for R, you need to use read_rds to get it into the envirionment, but you can't process it immediately.

a <- terra::unwrap(a)
# you need to unwrap the rds file before processing it. 
# Actually, rds is a combination of many different types of data, we can't use it immediately because the R can't handle different types of data in one time.

summary(a)
# Take a look at what the rds file looks like

df <- as.data.frame(a)
# To make a more processable, we need to transfrom it into dataframe
head(df)
# The column names of a (or df)
# "NAME_2"is the name of the 2nd administratice names (Of course, if you choose to load other level of admin boundaries, you can find that the NAME_X is in accordance to the admin level)

plot(a)
# We can plot a now to have a blank admin boundaries map

# make sure that the name_2 is the same as districts in data_coor
unique(data_coor$district)
unique(a$NAME_2)

# standerize the districts' names.
data_coor$district <- toupper(data_coor$district)
a$NAME_2 <- toupper(a$NAME_2)

# merge the two datasets

a_merged <- merge(a, data_coor, by.x = "NAME_2", by.y = "district", all.x = TRUE)

head(a_merged)

# transform to sf data, to make the points on the map
sf_data <- st_as_sf(a_merged)
#SF package and function can make the lon and lat into one data.
# csr, the 'Coordinate Reference System', 4326 means that the EPSG code is WGS 84（World Geodetic System 1984）.
# WGS 84 uses (longitude&latitude), counts as degrees.


# Plot with ggplot. The code is almost the same as before, just change the type of plot.
plot1 <- ggplot(data = sf_data) +
  geom_sf(aes(fill = Female_Headed_HHs), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Female_Headed_HHs",
       fill = "Counts") +
  theme_minimal() 
plot1

#ggsave can save the picture to where you want, into different forms, and specify which plot to save.
#also, it can indicate the size of picture you save.
#if you don't indicate which plot to save, it will automatically save the plot drawn most recently.
ggsave("path_where_you_want_to_save.png", plot1, width = 10, height = 8)

plot2 <- ggplot(data = sf_data) +
  geom_sf(aes(fill = Male_Headed_HHs), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Male_Headed_HHs",
       fill = "Counts") +
  theme_minimal() 
plot2
ggsave("path_where_you_want_to_save.png", plot2, width = 10, height = 8)


plot3 <- ggplot(data = sf_data) +
  geom_sf(aes(fill = Both), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Both",
       fill = "Counts") +
  theme_minimal() 
plot3
ggsave("path_where_you_want_to_save.png", plot3, width = 10, height = 8)



library(tidyr)
library(ggplot2)

# The sf_data includes three columns: Female_Headed_HHs, Male_Headed_HHs, Both 
# Transform the data into long-table
sf_data_long <- sf_data %>%
  pivot_longer(cols = c(Female_Headed_HHs, Male_Headed_HHs, Both),  #the 3 columns we want
               names_to = "Household_Type",  #how can they aggregated under 1 same type
               values_to = "Counts") #the values of these columns

#Draw the plot by the aggregated data.
ggplot(data = sf_data_long) +
  geom_sf(aes(fill = Counts), color = "black") + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Household Headed by Gender",
       fill = "Counts") +
  facet_wrap(~ Household_Type) +  # Panneled by Household_Type
  theme_minimal()

#--------------------------------------------------------------------------------
# Copy of Final Table(Gender)_(1)
# Sheet 14: Table 11. Distribution (count and Percentage) of Households by Main source of lighting   by sex of Hhs Head 

data_Lighting <- read_excel(path = "D:/Data examples/data4theworkshop/Copy of Final Table(Gender)_(1).xlsx", sheet = 14, skip = 2)

data_Lighting_Male <- data_Lighting[c(2:31), ]
data_Lighting_Female <- data_Lighting[c(37:66), ]
data_Lighting_Both <- data_Lighting[c(72:101), ]
#Select the data we want

names <- colnames(data_Lighting)
names[1:2] <- c('districts', 'n')
colnames(data_Lighting_Both) <- names
colnames(data_Lighting_Male) <- names
colnames(data_Lighting_Female) <- names
#Change the column names into more practice way

data_Lighting_Both <- merge(data_Lighting_Both, geo_data, by.x = "districts", by.y = "district", all.x = TRUE)
data_Lighting_Male <- merge(data_Lighting_Male, geo_data, by.x = "districts", by.y = "district", all.x = TRUE)
data_Lighting_Female <- merge(data_Lighting_Female, geo_data, by.x = "districts", by.y = "district", all.x = TRUE)

unique(data_Lighting_Both$districts)
unique(data_Lighting_Male$districts)
unique(data_Lighting_Female$districts)


# standerize the districts' names.
data_Lighting_Both$districts <- toupper(data_Lighting_Both$districts)
data_Lighting_Male$districts <- toupper(data_Lighting_Male$districts)
data_Lighting_Female$districts <- toupper(data_Lighting_Female$districts)

#Standerize the Electricity column, by changing it into numeric and remove the % sign.
data_Lighting_Both$Electricity <- as.numeric(gsub("%", "", data_Lighting_Both$Electricity))
data_Lighting_Male$Electricity <- as.numeric(gsub("%", "", data_Lighting_Male$Electricity))
data_Lighting_Female$Electricity <- as.numeric(gsub("%", "", data_Lighting_Female$Electricity))

#Merge the 3 data with a
Lighting_Both_merged <- merge(a, data_Lighting_Both, by.x = "NAME_2", by.y = "districts", all.x = TRUE)
Lighting_Both_merged_sf_data <- st_as_sf(Lighting_Both_merged)

data_Lighting_Male_merged <- merge(a, data_Lighting_Male, by.x = "NAME_2", by.y = "districts", all.x = TRUE)
data_Lighting_Male_merged_sf_data <- st_as_sf(data_Lighting_Male_merged)

data_Lighting_Female_merged <- merge(a, data_Lighting_Both, by.x = "NAME_2", by.y = "districts", all.x = TRUE)
data_Lighting_Female_merged_sf_data <- st_as_sf(data_Lighting_Female_merged)


plot4 <- ggplot(data = Lighting_Both_merged_sf_data ) +
  geom_sf(aes(fill = Electricity), color = "black") +
  scale_fill_viridis_c(option = "plasma", limits = c(40, 100)) +
  labs(title = "Both",
       fill = "Percentage") +
  theme_minimal() 

plot4


plot5 <- ggplot(data = data_Lighting_Male_merged_sf_data) +
  geom_sf(aes(fill = Electricity), color = "black") +
  scale_fill_viridis_c(option = "plasma", limits = c(40, 100)) +
  labs(title = "data_Lighting_Male_merged_sf_data",
       fill = "Percentage") +
  theme_minimal() 

plot5


plot6 <- ggplot(data = data_Lighting_Female_merged_sf_data) +
  geom_sf(aes(fill = Electricity), color = "black") +
  scale_fill_viridis_c(option = "plasma", limits = c(40, 100)) +
  labs(title = "data_Lighting_Female_merged_sf_data",
       fill = "Percentage") +
  theme_minimal() 

plot6


#-----------------------------------------------------------------
#summary of Data  received from Partners.xlsx
Abunzi_data <- read_xlsx('D:/Data examples/data4theworkshop/summary of Data  received from Partners.xlsx', sheet = 7, skip = 3)

#Choose and drop the columns and rows we want an
Abunzi_data <- Abunzi_data[(1:30), ]
Abunzi_data <- Abunzi_data[ , -2]

View(Abunzi_data)

# the districts' names are not in standard way. we need to change it.
Abunzi_data$District <- sub("^[0-9]+ ", "", Abunzi_data$District)

#Now merge the data with a
Abunzi_data_merged <- merge(a, Abunzi_data, by.x = "NAME_2", by.y = "District", all.x = TRUE)
Abunzi_data_merged_sf_data <- st_as_sf(Abunzi_data_merged)

ggplot(data = Abunzi_data_merged_sf_data) +
  geom_sf(aes(fill = MALE), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Abunzi_data_merged_sf_data_MALE",
       fill = "Number") +
  theme_minimal() 

ggplot(data = Abunzi_data_merged_sf_data) +
  geom_sf(aes(fill = FEMALE), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Abunzi_data_merged_sf_data_FEMALE",
       fill = "Number") +
  theme_minimal() 



#Just like before, change the pattern of the data, make it aggregated by sex and plot them in one map
Abunzi_data_merged_sf_data_long <- Abunzi_data_merged_sf_data %>%
  pivot_longer(cols = c(FEMALE, MALE), 
               names_to = "Sex", 
               values_to = "Number")

ggplot(data = Abunzi_data_merged_sf_data_long) +
  geom_sf(aes(fill = Number), color = "black") + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Abunzi_data_By_Sex",
       fill = "Number") +
  facet_wrap(~ Sex) +  # Panneled by Sex
  theme_minimal()




#---------------------------------------------------------------
#Tables_2024 Season A
Argri_data <- read_xlsx('D:/Data examples/data4theworkshop/Tables_2024 Season A.xlsx', sheet = 8)

#make the column names into good pattern.
names1 <- Argri_data[1,1:4 ]
names2 <- Argri_data[2, (ncol(names2)-1):ncol(names2)]
names <- cbind(names1, names2)
colnames(Argri_data) <- names

#Select the rows we want
Argri_data <- Argri_data[3:32 , ]

#the data type of these columns are character, we need them in numeric to plot on map
Argri_data[ ,2:6 ] <- lapply(Argri_data[,2:6 ], function(x) as.numeric(as.character(x)))

#Change the district names into upper
Argri_data $ District <- toupper(Argri_data $ District)

#The variable names should not contain space
names <- colnames(Argri_data)
names[2:6] <- c('Modern_irrigated', 'under_erosion_control', 'under_agroforestry_trees', 'Inorganic_fertilizer', 'Organic_fertilizer')
colnames(Argri_data) <- names

Argri_data_merged <- merge(a, Argri_data, by.x = "NAME_2", by.y = "District", all.x = TRUE)
head(Argri_data_merged)

Argri_data_merged_sf_data <- st_as_sf(Argri_data_merged)

ggplot(data = Argri_data_merged_sf_data) +
  geom_sf(aes(fill = Organic_fertilizer), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Organic fertilizer",
       fill = "Number") +
  theme_minimal() 

ggplot(data = Argri_data_merged_sf_data) +
  geom_sf(aes(fill = Inorganic_fertilizer), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Inorganic_fertilizer",
       fill = "Number") +
  theme_minimal() 

Argri_data_merged_sf_data_long <- Argri_data_merged_sf_data %>%
  pivot_longer(cols = c(Organic_fertilizer, Inorganic_fertilizer), 
               names_to = "Fertilizer", 
               values_to = "Number")

ggplot(data = Argri_data_merged_sf_data_long) +
  geom_sf(aes(fill = Number), color = "black") + 
  scale_fill_viridis_c(option = "plasma") + 
  labs(title = "Argri_data_Fertilized_Type",
       fill = "Number") +
  facet_wrap(~ Fertilizer) +   # Panneled by Fertilizer
  theme_minimal()














