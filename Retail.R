setwd("/Users/sediaz/Documents/Ubiqum/Proyectos alumnos/Mikel")

#### 1. Reading Libraries ####
library(caret)
library(readxl)
library(rworldmap)

#### 2. Reading the data ####
onlineretail <- as.data.frame(read_excel("Online Retail.xlsx"))

# Exploring the data structure
head(onlineretail)


# Plotting world map
ddf <- as.data.frame(table(onlineretail$Country))
names(ddf) <- c("country", "value")
ddf$value <- log(ddf$value)
spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="country")
mapCountryData(spdf, nameColumnToPlot="value", 
               catMethod="fixedWidth", 
               mapTitle = "Countries with more purchases")




