#### Environment  Settings

# Packages required for this proyect
pkgs <-c("tidyverse",   "caret","kernlab",  "randomForest","knitr")

# If a package is missing it will added in the missing packages list
missing_pkgs <-pkgs[!(pkgs %in% installed.packages())]

# The packages in the list will be installed
if (length(missing_pkgs)) {
  install.packages(missing_pkgs, repos = "http://cran.rstudio.com")
}
#Load the required libraries
library(caret)
library(tidyverse)
library(kernlab)
library(randomForest)
library(knitr)

#Download data ZIP file
if (!file.exists("PRSA2017_Data_20130301-20170228.zip")){
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00501/PRSA2017_Data_20130301-20170228.zip", "PRSA2017_Data_20130301-20170228.zip")
}

# Unzip the data file
if (!file.exists("Data")){
  zip_file<- ".\\PRSA2017_Data_20130301-20170228.zip"
  outDir<-"Data"
  unzip(zip_file,exdir=outDir)
}

#Read the 12 csv files into a variable
PRSA <- list.files(path = ".\\Data\\PRSA_Data_20130301-20170228",pattern = "*.csv", full.names = TRUE) %>% lapply(read_csv) %>% bind_rows                                                        

### Exploratory Data Analysis

# Structure of the data (data type, numbers of rows, number of attributes)
str(PRSA)

#Near zero variance ( identify the attributes that do not give us valuable data )
nzv <- nearZeroVar(PRSA)

# Dimensions of the original data (rows,columns) 
dim(PRSA)
## [1] 420768 18

#Remove the nzv columns and save in another variable
data <- PRSA[,-nzv]

# Dimensions of the valuable data (rows,columns) 
dim(data)
## [1] 420768 17

# Show the data in a transposed version to see more data
glimpse(data)

#Remove the "no" variable because is a consecutive (also do not give us valuable data)
data <- data[,-c(1)]

# Cast the attributes year, month, day, hour, wd and station from "dbl" type to "factor" type
data$year <- as.factor(data$year) 
data$month <- as.factor(data$month) 
data$day <- as.factor(data$day) 
data$hour <- as.factor(data$hour) 
data$wd <- as.factor(data$wd) 
data$station <- as.factor(data$station) 

# Analyse if the the attributes PM (2.5 and 10) and CO can be cast to Integer
unique(data$PM2.5)
unique(data$PM10)
unique(data$CO)

# Only the attribute CO can be casted to integer
data$CO <- as.integer(data$CO) 

# Review the previous changes in a transposed version to see more data
glimpse(data)

# Review the statistics of each attribute
summary(data)

# Review the first 6 rows
head(data)

# Histogram of Carbon Monoxide
data %>%
  ggplot(aes(CO)) + geom_histogram() +
  labs(title = "China Contamination", x = "Carbon Monoxide")


