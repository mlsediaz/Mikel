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

createEuropeMap <- function(my_table, my_title){
  # Must contain two columns: Country and Value  
  names(my_table) <- c("country", "value")
  my_table$value <- log10(my_table$value)
  spdf <- joinCountryData2Map(my_table, joinCode="NAME", nameJoinColumn="country")
  my_world_map <- mapCountryData(spdf, nameColumnToPlot="value", 
                                 colourPalette = "heat", 
                                 mapRegion = "Europe",
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

#### Question 4: Average spent per user ####

# Creating copy of original data jic 
spentPerUser <- onlineretail
spentPerUser <- mutate(spentPerUser, TotalSpent = UnitPrice * Quantity)

# We need to clean the missing values
spentPerUser <- spentPerUser[- which(is.na(spentPerUser$CustomerID)), ]


# First we need the total amount per user
my_group <- group_by(spentPerUser, Country, CustomerID)
spentPerUser <- dplyr::summarize(my_group, Value = sum(TotalSpent))

# Now we calculate the average per user per country
my_group <- group_by(spentPerUser, Country)
spentPerUser <- dplyr::summarise(my_group, Value = mean(Value))


# Annnnnnd plotting
createWorldMap(spentPerUser, "Average spent per user")

# Detail of Europe
createEuropeMap(spentPerUser, "Average spent per user")

#### Question 5: Relationship between price and units sold ####

# Creating copy of original data jic 
unitsVsPrice <- onlineretail

# Now we calculate how many units of every product has been sold
my_group  <- group_by(unitsVsPrice, StockCode, UnitPrice)
unitsVsPrice <- dplyr::summarise(my_group, Value = sum(Quantity))

ggplot() + geom_point(data = unitsVsPrice, aes(x = UnitPrice, y = Value)) + xlim(0,25) +
  ggtitle("Units sold vs UnitPrice") + xlab("Unit price (Pounds)") + ylab("Units sold") +
  theme(plot.title = element_text(hjust = 0.5))

