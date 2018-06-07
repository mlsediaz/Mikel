setwd("/Users/sediaz/Documents/Ubiqum/Proyectos alumnos/Mikel")

#### 0. Creating my own functions ####


createWorldMap <- function(my_table, my_title){
# Must contain two columns: Country and Value  
  names(my_table) <- c("country", "value")
  my_table$value <- log10(my_table$value)
  spdf <- joinCountryData2Map(my_table, joinCode="NAME", nameJoinColumn="country")
  my_world_map <- mapCountryData(spdf, nameColumnToPlot="value", 
                 colourPalette = "heat", 
                 borderCol = "azure",
                 addLegend = TRUE,
                 oceanCol = "cadetblue3",
                 catMethod="fixedWidth",
                 missingCountryCol = "darkgrey",
                 mapTitle = my_title)
  
  return(my_world_map)
  
}


#### 1. Reading Libraries ####
library(caret)
library(readxl)
library(rworldmap)
library(dplyr)

#### 2. Reading the data ####
onlineretail <- as.data.frame(read_excel("Online Retail.xlsx"))

# Exploring the data structure
head(onlineretail)

#### Question 1: Number of purchases per country ####
my_df <- as.data.frame(table(onlineretail$Country))
createWorldMap(my_df, "Countries with more purchases")

#### Question 2: spent per country ####

# Creating copy of original data jic 
totalPerCountry <- onlineretail

totalPerCountry <- mutate(totalPerCountry, TotalSpent = UnitPrice * Quantity)


# Calculating total spent per country
my_group <- group_by(totalPerCountry, Country)
totalPerCountry <- dplyr::summarize(my_group, Value = sum(TotalSpent))

totalPerCountry$Value <- round(totalPerCountry$Value)
createWorldMap(totalPerCountry, "Total money spent per country")



#### Question 3: Number of unique users per country ####

# Creating copy of original data jic 
usersPerCountry <- onlineretail

# Calculating number of unique users per country
my_group <- group_by(usersPerCountry, Country, CustomerID)
usersPerCountry <- dplyr::summarize(my_group, Value = n())

usersPerCountry <- as.data.frame(table(usersPerCountry$Country))
names(usersPerCountry) <- c("Country", "Value")

createWorldMap(usersPerCountry, "Number of users per Country")
