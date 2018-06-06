setwd("/Users/sediaz/Documents/Ubiqum/Proyectos alumnos/Mikel")

#### 0. Creating my own functions ####


createWorldMap <- function(my_table, my_title){
# Must contain a column called Country  
  ddf <- as.data.frame(table(my_table$Country))
  names(ddf) <- c("country", "value")
  ddf$value <- log(ddf$value)
  spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="country")
  my_world_map <- mapCountryData(spdf, nameColumnToPlot="value", 
                 colourPalette = "white2Black", 
                 borderCol = "white",
                 addLegend = TRUE,
                 oceanCol = "steelblue3",
                 catMethod="fixedWidth",
                 missingCountryCol = "navajowhite3",
                 mapTitle = my_title)
  
  return(my_world_map)
  
}


#### 1. Reading Libraries ####
library(caret)
library(readxl)
library(rworldmap)

#### 2. Reading the data ####
onlineretail <- as.data.frame(read_excel("Online Retail.xlsx"))

# Exploring the data structure
head(onlineretail)


# Plotting world map

createWorldMap(onlineretail, "Countries with more purchases")







