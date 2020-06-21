#### Environment  Settings

# Packages required for this proyect
pkgs <-c("tidyverse", "caret","kernlab", "caTools","ranger",  "randomForest","knitr")

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
library(caTools)
library(ranger)
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

#Remove the nzv columns, NA data and save in another variable
data <- PRSA[,-nzv]
data<- na.omit(data)

# Dimensions of the valuable data (rows,columns) 
dim(data)

# Show the data in a transposed version to see more data
glimpse(data)

#Remove the "no" variable because is a consecutive (also do not give us valuable data)
data <- data[,-c(1)]

# Cast the attributes year, month, day, hour, wd and station from "dbl" type to "factor" type
data$year <- as.factor(data$year) 
data$month <- as.factor(data$month) 
data$day <- as.integer(data$day) 
data$hour <- as.integer(data$hour) 
data$wd <- as.factor(data$wd) 
data$station <- as.factor(data$station) 

# Analyse if the the attributes PM (2.5 and 10) and CO can be cast to Integer
unique(data$PM2.5)
unique(data$PM10)
unique(data$CO)

# Only the attribute CO can be casted to integer
data$CO <- as.integer(data$CO) 

# Finaly add the attribute date in a single column
data <- data %>% mutate(date = ISOdate(year, month, day,hour))

# Filtered for performance reasons: half days and only 3 rows per day
data <-data %>% filter(day %%2==0,hour %%6==0) %>% select(date,CO,PM2.5,PM10,SO2,NO2,O3,TEMP,PRES,DEWP,wd,WSPM,station)

#And convert de tibble to data.frame
data<-as.data.frame(data)

# Review the previous changes in a transposed version to see more data
glimpse(data)

# Review the statistics of each attribute
summary(data)

# Review the first 6 rows
head(data)

# Histogram of Carbon Monoxide
data %>% ggplot(aes(CO)) + geom_histogram() +
  labs(title = "China Contamination", x = "Carbon Monoxide")

########### Exploratory Data Analisys ####################################

###### Data Throught Time
# Carbon Monoxide
data_grph <- data %>%
  select(date,CO) %>%
  gather(key = "Indexes", value = "value", -date)

data_grph %>%
  ggplot(aes(x=date,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Carbon Monoxide", x = "Year",
       y = "concentration (ug/mˆ3)")
# Particulate Matter and Ozone.
data_grph <- data %>%
  select(date,PM10,PM2.5,O3) %>%
  gather(key = "Indexes", value = "value", -date)

data_grph %>%
  ggplot(aes(x=date,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Particulate Matter and Ozone.", x = "Year",
       y = "concentration (ug/mˆ3)")
# Nitrogen Dioxide and Sulfur Dioxide
data_grph <- data %>%
  select(date, NO2, SO2) %>%
  gather(key = "Indexes", value = "value", -date)

data_grph %>%
  ggplot(aes(x=date,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Nitrogen Dioxide and Sulfur Dioxide", x = "Year",
       y = "concentration (ug/mˆ3)")

##### Data Throught Temperature
# Carbon Monoxide
data_grph <- data %>%
  select(TEMP,CO) %>%
  gather(key = "Indexes", value = "value", -TEMP)

data_grph %>%
  ggplot(aes(x=TEMP,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Carbon Monoxide", x = "Temperature (°C)",
       y = "concentration (ug/mˆ3)")

# Particulate Matter and Ozono
data_grph <- data %>%
  select(TEMP,PM10,PM2.5,O3) %>%
  gather(key = "Indexes", value = "value", -TEMP)

data_grph %>%
  ggplot(aes(x=TEMP,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Particulate Matter and Ozone.", x = "Temperature (°C)",
       y = "concentration (ug/mˆ3)")
# Nitrogen Dioxide and Sulfur Dioxide
data_grph <- data %>%
  select(TEMP, NO2, SO2) %>%
  gather(key = "Indexes", value = "value", -TEMP)

data_grph %>%
  ggplot(aes(x=TEMP,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Nitric Dioxide and Sulfur Dioxide", x = "Temperature (°C)",
       y = "concentration (ug/mˆ3)")

### Data Throught Pressure

# Carbon Monoxide
data_grph <- data %>%
  select(PRES,CO) %>%
  gather(key = "Indexes", value = "value", -PRES)

data_grph %>%
  ggplot(aes(x=PRES,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Carbon Monoxide", x = "Pressure (hPa)",
       y = "concentration (ug/mˆ3)")

# Particulate Matter and Ozono
data_grph <- data %>%
  select(PRES,PM10,PM2.5,O3) %>%
  gather(key = "Indexes", value = "value", -PRES)

data_grph %>%
  ggplot(aes(x=PRES,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Particulate Matter and Ozone.", x = "Pressure (hPa)",
       y = "concentration (ug/mˆ3)")
# Nitrogen Dioxide and Sulfur Dioxide
data_grph <- data %>%
  select(PRES, NO2, SO2) %>%
  gather(key = "Indexes", value = "value", -PRES)

data_grph %>%
  ggplot(aes(x=PRES,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Nitric Dioxide and Sulfur Dioxide", x = "Pressure (hPa)",
       y = "concentration (ug/mˆ3)")

### Data Throught Wind Speed
# Carbon Monoxide
data_grph <- data %>%
  select(WSPM,CO) %>%
  gather(key = "Indexes", value = "value", -WSPM)

data_grph %>%
  ggplot(aes(x=WSPM,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Carbon Monoxide", x = "Wind Speed (m/s)",
       y = "concentration (ug/mˆ3)")
# Particulate Matter and Ozono
data_grph <- data %>%
  select(WSPM,PM10,PM2.5,O3) %>%
  gather(key = "Indexes", value = "value", -WSPM)

data_grph %>%
  ggplot(aes(x=WSPM,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Particulate Matter and Ozone.", x = "Wind Speed (m/s)",
       y = "concentration (ug/mˆ3)")
# Nitrogen Dioxide and Sulfur Dioxide
data_grph <- data %>%
  select(WSPM, NO2, SO2) %>%
  gather(key = "Indexes", value = "value", -WSPM)

data_grph %>%
  ggplot(aes(x=WSPM,y=value)) +
  geom_point(aes(color = Indexes)) + 
  labs(title = "Nitric Dioxide and Sulfur Dioxide", x = "Wind Speed (m/s)",
       y = "concentration (ug/mˆ3)")

### Data Throught Station
# Carbon Monoxide
data_grph <- data %>%
  select(date,station,CO) %>%
  gather(key = "Indexes", value = "value", "CO")

data_grph %>%
  ggplot(aes(x=date,y=value)) +
  geom_point(aes(color = Indexes)) + 
  facet_wrap(~ station) +
  labs(title = "Carbon Monoxide", x = "Year",
       y = "concentration (ug/mˆ3)")
# Particulate Matter and Ozono
data_grph <- data %>%
  select(date,station,PM10,PM2.5,O3) %>%
  gather(key = "Indexes", value = "value", "PM10":"O3")
data_grph %>%
  ggplot(aes(x=date,y=value)) +
  geom_point(aes(color = Indexes)) +  
  facet_wrap(~ station) +
  labs(title = "Particulate Matter and Ozone.", x = "Year",
       y = "concentration (ug/mˆ3)")
# Nitrogen Dioxide and Sulfur Dioxide
data_grph <- data %>%
  select(date,station, NO2, SO2) %>%
  gather(key = "Indexes", value = "value", "NO2":"SO2")

data_grph %>%
  ggplot(aes(x=date,y=value)) +
  geom_point(aes(color = Indexes)) +  
  facet_wrap(~ station) +
  labs(title = "Nitric Dioxide and Sulfur Dioxide", x = "Year",
       y = "concentration (ug/mˆ3)")


###### Models ##########################################################
#Defining training and test data set
set.seed(2000, sample.kind="Rounding")

## Next, create the partition
test_prt <- createDataPartition(data$CO, times = 1, p = 0.1, list = FALSE)
train_dts <- data[-test_prt,]
test_dts <- data[test_prt,]
dim(train_dts)
dim(test_dts)
real_CO <- test_dts$CO
RMSE <- function(real_data, predicted_data){
  sqrt(mean((real_data - predicted_data)^2))
}
##### Simple Average #############################
## The average is calculated
avg <-mean(train_dts$CO)

# Calculation of the RMSE with the predicted and test set
rmse_avg <- RMSE(real_CO, avg)

# Saving the results
options(pillar.sigfig = 6)
models_result <- tibble(Model = "Simple Average", RMSE = rmse_avg)
models_result


##### k Nearest Neighbours (kNN) #############################

#### identifying the best "k" 
set.seed(2000, sample.kind="Rounding")
knn_k <- train(CO ~ date,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(1, 11, 2)),
                   data = train_dts)

##Getting the best "K":
best_k<-knn_k$bestTune
best_k
## Plot the K's calculated
ggplot(knn_k, highlight = TRUE)

#### Training the model
set.seed(2000, sample.kind="Rounding")

train_knn <- train(CO ~ date,
                   method = "knn",
                   tuneGrid = data.frame(k = best_k),
                   data = train_dts)

## Getting the prediction
pred_knn <- predict(train_knn, test_dts)

# Calculation of the RMSE with the predicted and test set
rmse_knn <- RMSE(real_CO, pred_knn)

# Saving the results
models_result <- bind_rows(models_result,
                           tibble(Model = "K-Nearest Neighbors", RMSE = rmse_knn))
models_result

##### Random Forest #############################
#### Training the model
set.seed(2000, sample.kind="Rounding")
train_rnd <- ranger(CO ~ date,
                  data = train_dts,
                  num.trees = 400,
                  respect.unordered.factors = "order",
                  seed = 2000)
print(train_rnd)

## Getting the prediction
pred_rnd <- predict(train_rnd, test_dts)$predictions

# Calculation of the RMSE with the predicted and test set
rmse_rnd <- RMSE(real_CO, pred_rnd)

# Saving the results
models_result <- bind_rows(models_result,
                           tibble(Model = "Random Forest", RMSE = rmse_rnd))
models_result


##### Neural Network #############################
#### Training the model
set.seed(2000, sample.kind="Rounding")
train_nnt <- train(CO ~ date,
                   method = "nnet",
                   data = train_dts)

## Getting the prediction
pred_nnt <- predict(train_nnt, test_dts)

# Calculation of the RMSE with the predicted and test set
rmse_nnt <- RMSE(real_CO, pred_nnt)

# Saving the results
models_result <- bind_rows(models_result,
                           tibble(Model = "Neural Network", RMSE = rmse_nnt))
models_result

