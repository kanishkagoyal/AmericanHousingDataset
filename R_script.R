
#************************************ Setup ************************************

# setwd("replace with the path of your working directory")

# data files - data files located in the same folder as this R script. 

# Check and install the required packages---------------------------------------

if(!("readr" %in% rownames(installed.packages()))){
  install.packages("readr")
}

if(!("tidyverse" %in% rownames(installed.packages()))){
  install.packages("tidyverse")
}

if(!("GGally" %in% rownames(installed.packages()))){
  install.packages("GGally")
}

if(!("gridExtra" %in% rownames(installed.packages()))){
  install.packages("gridExtra")
}

if(!("modelr" %in% rownames(installed.packages()))){
  install.packages("modelr")
}

library(readr)     # Data import
library(tidyverse) # Data wrangle and visualization
library(ggplot2)   # Data Visualization
library(GGally)    # Correlation and plotting  
library(gridExtra) # Plot arrangement 
library(Metrics)   # Data modeling and evaluation
library(modelr)

# Loading data sets into working directory--------------------------------------

train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")

# Setting my standard theme-----------------------------------------------------

my_theme <- theme(
  panel.background = element_rect( fill = "ivory2" , color ="gray"),
  plot.background = element_rect( color = "black"),
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  axis.title.x = element_text(size = 12, hjust = 0.5),
  axis.title.y = element_text(size = 12, hjust = 0.5))

# ************************** Data Pre-processing *******************************

# Plotting the number of NA values for different variables----------------------

# create a data frame of count of na values
na_values_cols <- data.frame()

for(i in 1:length(train_data)-1){
  na_values_cols[i,1] <- names(train_data)[i]
  na_values_cols[i,2] <- colSums(is.na(train_data[i]))
}
names(na_values_cols) <- c("Col_name", "N_nas")
j<- 0
for(i in na_values_cols$N_nas){
  if(i>0){
    j <- j+1
  }
}

# plot Na values 
na_values_cols %>% 
  arrange(desc(N_nas)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = reorder(Col_name, -N_nas), y = N_nas)) +
  geom_text(aes(y = N_nas, label = N_nas, vjust = -0.5)) +
  geom_col(fill = "coral") +
  labs(
    x = "Variable names",
    y = "No. of na Values",
    title = "Highest No. of Na values variables"
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  my_theme

# Remove variables if 100% empty------------------------------------------------

# list storing the indexes of empty columns
empty_cols <- numeric()

# find the columns which are all empty and store indexes in empty_cols
for(i in 2:length(train_data)){
  if(colSums(is.na(train_data[i])) == nrow(train_data)){
    empty_cols <- c(empty_cols,i)
  }
}

# check and remove columns that are all empty
if(length(empty_cols) > 0){
  train_data <- train_data[,-(empty_cols)]
  print("All the empty columns removed.")
}else{
  print("There are no columns that are all empty")
}

# Replacing NA with "None" for all those variables where NA stand for missing 
# feature rather than missing information -------------------------------------- 

na_correction <- c("BsmtExposure","BsmtFinType1", "BsmtFinType2", "Alley",
                   "GarageType", "GarageFinish",  "Fence" ,  "MiscFeature",
                   "PoolQC", "FireplaceQu", "GarageQual" , "GarageCond", 
                   "BsmtQual" , "BsmtCond")

for(i in 1:length(na_correction)){
  for(j in 1:nrow(train_data)){
    if(is.na(train_data[j,na_correction[i]])){
      train_data[j,na_correction[i]] <- "None"
    }
  }
}

# Replacing NA with median for Lot Frontage-------------------------------------

# Lot Frontage is Linear feet of street connected to property
# Replace NA values with median grouped on type of street

train_data <- train_data %>%
  group_by(Street) %>%
  mutate( LotFrontage = ifelse(is.na(LotFrontage), 
                               median(LotFrontage, na.rm = TRUE),
                               LotFrontage))

# Omitting rows for the columns where less than 0.01% data is missing-----------


# Masonry Veneer and electrical 

empty_var <- numeric()

for(i in 1:nrow(train_data)){
  if(is.na(train_data$MasVnrType[i])){
    empty_var <- c(empty_var,i)
  }
}

for(i in 1:nrow(train_data)){
  if(is.na(train_data$Electrical[i])){
    empty_var <- c(empty_var,i)
  }
}

train_data <- train_data[-(empty_var),]

# Changing Quality & Condition variables----------------------------------------

var_list <- c(
  "ExterQual", "ExterCond" , "BsmtQual" , "BsmtCond" , "HeatingQC" , "KitchenQual" ,
  "FireplaceQu" , "GarageQual" , "GarageCond", "PoolQC"
)

var_index_list <- c(
  28,29,31,32,41,54,58,64,65,73
)

for( i in var_index_list){
  for(j in 1:nrow(train_data)){
      if(train_data[j,i] == "Ex"){ 
        train_data[j,i] <- "5" }
      else if(train_data[j,i] == "Gd"){ 
        train_data[j,i] <- "4" }
      else if(train_data[j,i] == "TA"){ 
        train_data[j,i] <- "3" }
      else if(train_data[j,i] == "Fa"){ 
        train_data[j,i] <- "2" }
      else if(train_data[j,i] == "Po"){ 
        train_data[j,i] <- "1" }
      else{ 
      train_data[j,i] <- "0" }
  }
}

for(i in var_index_list){
  train_data[[i]]  <- as.numeric(train_data[[i]]) 
}

# Log Price---------------------------------------------------------------------

# If price needs to be logged

train_data %>%
  ggplot(aes(x = SalePrice, fill = ..count..)) +
  geom_histogram() +
  labs(
    x = "Price",
    y = "Frequency",
    ggtitle = "Histogram for Sale Price"
  ) +
  my_theme

# Due to big stretch

train_data <- train_data %>%
  mutate(log_price = log(SalePrice))



# ********************************** EDA ***************************************


# ******************************************************************************
# Problem 1 - Analyze if age of the House has an impact on sales price across 
#             different types of foundation. 
# ******************************************************************************

train_data$Age <- train_data$YrSold - train_data$YearBuilt

train_data %>% 
  ggplot(aes(x = Age, y = log_price, color = Foundation)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Age (years)",
    y = "Sale Price (Log) ",
    title = "Sale price vs. Age across foundations"
  ) +
  my_theme

# ******************************************************************************
# Problem 2 - Analyze whether Basement/ Garage has any effect on sale price.
# ******************************************************************************

# --------------Test if Basement has an impact on sale price -------------------

# Correlation plot on numeric variables related to basement---------------------

ggcorr(train_data[,c(31,32,35,37,38,39,82)], label = T, name = "rho")

# Total sq ft. area of basement and Basement quality seems to impact 
# sale price.

# Plotting Total Sq. ft. area of basement --------------------------------------

train_data %>% 
  ggplot(aes(x = TotalBsmtSF, y = log_price)) +
  geom_point(alpha = 0.7) +
  geom_smooth() +
  labs(
    x = " Total Basement area (sq. ft.)",
    y = " Sale Price (Log)",
    title = "Sale price vs Basement area"
  ) +
  my_theme

# There is one extreme outlier in the data which is tampering the trend line.
# Also, the variable which do not have a basement hinder in the analysis. 

# Removing outlier

train_data <- train_data[-(which.max(train_data$TotalBsmtSF)),]

# Plotting again without considering the data that does not have a basement-----

train_data %>% 
  filter(TotalBsmtSF > 0) %>% 
  ggplot(aes(x = TotalBsmtSF, y = log_price)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  labs(
    x = " Total Basement area (sq. ft.)",
    y = " Sale Price (Log)",
    title = "Sale price vs Basement area"
  ) +
  my_theme

# Checking basement Quality in relation to Price--------------------------------

train_data %>% 
  filter(BsmtQual > 0, BsmtExposure!= "None") %>% 
  group_by(BsmtQual, BsmtExposure) %>% 
  summarize(Median_price = median(SalePrice)) %>% 
  ggplot(aes(x = BsmtQual, y = Median_price, fill = BsmtExposure)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Basement Quality",
    y = " Average Sale price",
    title = " Average Sale price vs Basement Quality"
  ) +
  my_theme

# Analyzing categorical variables in relation to basement-----------------------

# Sale price/ Log_price vs Basement Exposure  ----------------------------------

train_data %>% 
  ggplot(aes(x = BsmtExposure, y = log_price, fill = BsmtExposure)) +
  geom_boxplot() +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "white",
               fill ="blue") +
  scale_x_discrete(name = " Bsmt Exposure",
                    labels = c("Average","Good","Minimum","No","None")) +
  scale_fill_manual(name = " Bsmt Exposure",
                   labels = c("Average","Good","Minimum","No","None"),
                   values = c("skyblue","olivedrab3", "tan1", "yellow2","plum")) +
  labs(
    x = "Basement Exposure",
    y = "Sale price",
    title = " Sale price vs. Basement Exposure"
  ) +
  my_theme

# --------------Test if garage has an impact on sale price ---------------------

# Correlation plot on numeric variables related to GARAGE-----------------------

ggcorr(train_data[,c(60,62,63,64,65,82)], label = T, name = "rho")

# Garage area, Garage Cars has some good correlation with sale price.
# Garage year built has some relation too with a correlation of 0.5 

# Plot Sale price vs Garage Area plotted across age of garage-------------------

train_data %>% 
  filter( GarageArea > 0 , GarageArea < 1050) %>% 
  mutate(Garage_age = YrSold - GarageYrBlt) %>% 
  ggplot(aes(x = GarageArea, y = log_price, color = Garage_age)) +
  geom_jitter() +
  scale_color_gradient(low ="coral", high = "brown") +
  labs(
    x = "Garage area(sq. ft.)",
    y = "Sale price (Log)",
    title = "Sale price vs Garage area plotted across age of the garage"
  ) +
  my_theme

# Check for categorical variables related to garage-----------------------------

# Impact of garage type on Sale Price-

train_data %>% 
  ggplot(aes(x = GarageType, y = log_price, fill = GarageType)) +
  geom_violin() +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "white",
               fill ="black") +
  labs(x = " Garage Type",
       y = "Sale Price",
       title = "Sale Price vs Garage Type") +
  my_theme

# Impact of Garage Finish on Sale Price -

train_data %>% 
  filter(GarageFinish != "None") %>% 
  ggplot(aes(x = GarageFinish, y = log_price, fill = GarageFinish)) +
  geom_boxplot() + 
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "white",
               fill ="blue") +
  scale_x_discrete(name = "Garage Finish",
                   labels = c("Finished","Rough Finish","Unfinished")) +
  scale_fill_manual(name = " Garage Finish",
                    labels = c("Finished","Rough Finished","Unfinished"),
                    values = c("skyblue","olivedrab3", "tan1")) +
  labs(
    x = "Garage Finish",
    y = "Sale price (Log) ",
    title = " Sale price vs. Garage Finish"
  ) +
  my_theme

# ******************************************************************************
# Problem 3 - How has the Average Sale Price changed from year 2006 to 2010  
#             across different neighborhoods? 
# ******************************************************************************

train_data %>% 
  group_by(YrSold, Neighborhood) %>% 
  summarize(Average_price = median(log_price)) %>% 
  ggplot(aes(x = YrSold, y = Average_price, color = Average_price)) +
  geom_point()  +
  geom_line() +
  scale_color_gradient(low ="coral", high = "black") +
  facet_wrap(~ Neighborhood) +
  labs(
    x = "Year Sold",
    y = " Average Sale price (Log)",
    title = "Average Sale price vs Year Sold across different Locations"
  ) +
  theme(axis.text.x = element_text(angle = 90),
        plot.background = element_rect( color = "black"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5))

# ******************************************************************************
# Problem 4 - How are quality/ condition features effecting sale price? 
# ******************************************************************************

# Correlation plot for Quality/ Condition variables-----------------------------

ggcorr(train_data[,c(18,19,28,29,41,54,58,73,82)], label = T, name = "rho")

# Here, Overall Quality, Exterior Quality, Kitchen Quality, Fireplace Quality  
# have a reasonable correlation with Sale Price---------------------------------

# Analyzing Overall Quality in relation to Sale price---------------------------

a <- train_data %>% 
  group_by(OverallQual) %>% 
  summarize( Average_price = median(SalePrice)) %>% 
  ggplot(aes(x = OverallQual, y = Average_price, fill = OverallQual)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Overall Quality",
    y = " Average Price",
    title = "Overall Quality"
  ) +
  my_theme

# Analyzing Exterior Quality in relation to Sale price--------------------------

b <- train_data %>% 
  group_by(ExterQual) %>% 
  summarize( Average_price = median(SalePrice)) %>% 
  ggplot(aes(x = ExterQual, y = Average_price, fill = ExterQual)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Exterior material Quality",
    y = " Average Price",
    title = "Exterior material Quality"
  ) +
  my_theme


# Analyzing Kitchen Quality in relation to Sale price---------------------------

c <- train_data %>% 
  group_by(KitchenQual) %>% 
  summarize( Average_price = median(SalePrice)) %>% 
  ggplot(aes(x = KitchenQual, y = Average_price, fill = KitchenQual)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Kitchen Quality",
    y = " Average Price",
    title = "Kitchen Quality"
  ) +
  my_theme

# Analyzing Fireplace Quality in relation to Sale price-------------------------

d <- train_data %>% 
  filter(FireplaceQu > 0) %>% 
  group_by(FireplaceQu) %>% 
  summarize( Average_price = median(SalePrice)) %>% 
  ggplot(aes(x = FireplaceQu, y = Average_price, fill = FireplaceQu)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Fireplace Quality",
    y = " Average Price",
    title = "Fireplace Quality"
  ) +
  my_theme

# Plotting Quality variables on single screen-----------------------------------

grid.arrange(a,b,c,d)

# ******************************************************************************
# Problem 5 - Does the sale price fluctuate across seasons of the year?
# ******************************************************************************

# Variable creation for analyzing seasonal trend--------------------------------

train_data$seasonSold <- ifelse(
  train_data$MoSold == 12 | train_data$MoSold == 1 | train_data$MoSold == 2, "Winter", ifelse(
    train_data$MoSold == 3 | train_data$MoSold == 4 | train_data$MoSold == 5, "Spring", ifelse(
      train_data$MoSold == 6 | train_data$MoSold == 7 | train_data$MoSold == 8, "Summer", ifelse(
        train_data$MoSold == 9 | train_data$MoSold == 10 | train_data$MoSold == 11, "Autumn", NA
      ) 
    )
  ))

# Plotting

train_data %>% 
  group_by(seasonSold, YrSold) %>% 
  summarize(Sale_freq = n()) %>% 
  ggplot(aes(x = YrSold, y = Sale_freq, fill = seasonSold)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(YrSold,Sale_freq, label = Sale_freq), 
            vjust = -0.5 ,
            position = position_dodge(width = 1)) +
  labs(
    x = " Year Sold",
    y = "No.of Sales",
    title = "No. of Sales across different seasons from 2006-2010"
  ) +
  my_theme
  
# ******************************************************************************
# Problem 6 - Analyze if Lot size, or various other area measurements make 
#             an impact on sale price.
# ******************************************************************************

# All the area variables are divided into two sets of variables and 
# two Correlation plots created--------------------------------------------

# 1st set
ggcorr(train_data[,c(27,4,5,47,44,45,46,82)], label = T, name = "rho") +
  ggtitle("Scatter Plot Matrix for Area's vs Sale price") +
  my_theme

# 2nd set
ggcorr(train_data[,c(67,68,69,70,71,72,82)], label = T, name = "rho")+
  ggtitle("Scatter Plot Matrix for Area's vs Sale price") +
  my_theme

# Created scatter plot matrix for those variable that showed some 
# relation with Sale price------------------------------------------------------

ggpairs(train_data, columns = c(44,47,82)) +
  ggtitle("Scatter Plot Matrix for Area's vs Sale price") +
  my_theme

# Living area has a moderate relationship with Sale price-----------------------

# Living area vs. Sale Price----------------------------------------------------

train_data %>% 
  ggplot(aes(x = GrLivArea, y = log_price, color = as.factor(TotRmsAbvGrd))) +
  geom_jitter() +
  scale_color_discrete( name = "No. of rooms") +
  labs(
    x = "Above ground living area (sq. ft.)",
    y = "Sale Price (Log)",
    title = "Sale Price vs. Living area and number of rooms"
  ) +
  my_theme


# ******************************************************************************
# Problem 7 - Analyze other key features to find what best predicts the sale 
# price.
# ******************************************************************************

# Correlation plot for variables like No. of bedrooms/Kitchen/ Bathroom 
# with Sale price---------------------------------------------------------------

ggcorr(train_data[,c(48,49,50,51,52,53,55,57,82)], label = T, name = "rho")

# No. of full bathrooms and First floor area has moderate correlation-----------

# Sale Price vs. No. of full Bathrooms -----------------------------------------

train_data %>% 
  filter(FullBath > 0) %>% 
  ggplot(aes(x = factor(FullBath), y = log_price, fill = factor(FullBath))) +
  geom_boxplot() +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "white",
               fill ="blue")+
  scale_color_discrete(name = "No. of bathrooms") +
  labs(
    x = "Number of Full Bathrooms",
    y = "Sale price (Log)",
    title = "Sale price vs. Full Bathrooms"
  ) +
  my_theme

# Sale price vs MS Zoning-------------------------------------------------------

train_data %>% 
  ggplot(aes(x = MSZoning, y = log_price, fill = MSZoning)) +
  geom_boxplot() +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 5,
               color = "white",
               fill ="blue")+
  scale_color_discrete(name = "MS Zoning") +
  labs(
    x = "MS Zoning",
    y = "Sale price (Log)",
    title = "Sale price vs. MS Zoning"
  ) +
  my_theme

# Sale Price vs Neighborhood----------------------------------------------------

# 1. Box plot

train_data %>% 
  ggplot(aes(x = Neighborhood, y = log_price, fill = Neighborhood)) +
  geom_boxplot() +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 4,
               color = "white",
               fill ="blue")+
  scale_color_discrete(name = "Neighborhood") +
  labs(
    x = "Neighborhood",
    y = "Sale price (Log)",
    title = "Sale price vs. Neighborhood"
  ) +
  my_theme +
  coord_flip()

# 2. Bar Plot

train_data %>% 
  group_by(Neighborhood) %>% 
  summarize(Average_price = median(SalePrice)) %>% 
  ggplot(aes(x = Neighborhood, y = Average_price, fill = Neighborhood)) +
  geom_bar(stat = "identity") +
  labs(
    x = "Neighborhood",
    y = " Average Price",
    title = "Average price vs. Neighborhood"
  ) +
  my_theme +
  coord_flip()

# Sale Price vs Foundation------------------------------------------------------

train_data %>% 
  ggplot(aes(x = Foundation, y = log_price, fill = Foundation)) +
  geom_boxplot() +
  stat_summary(fun = mean,
               geom = "point",
               shape = 20,
               size = 4,
               color = "white",
               fill ="blue")+
  geom_jitter(alpha = 0.5, size = 0.6) +
  scale_color_discrete(name = "Foundation") +
  labs(
    x = "Foundation",
    y = "Sale price (Log)",
    title = "Sale price vs. Foundation"
  ) +
  my_theme 


# ******************************************************************************
# Problem 8 - How are Number of Sales distributed among neighborhoods/ Building
#             type across time? 
# ******************************************************************************

# Among Neighbors

train_data %>% 
  filter(YrSold != 2010) %>% 
  group_by(YrSold, Neighborhood) %>% 
  summarize(N_Sale = n()) %>% 
  ggplot(aes(x = YrSold, y = N_Sale, color = N_Sale)) +
  geom_point()  +
  geom_line() +
  scale_color_gradient(low ="coral", high = "black") +
  facet_wrap(~ Neighborhood) +
  labs(
    x = "Year Sold",
    y = " Number of Sales",
    title = "No. of Sales vs. Year Sold across different Locations"
  ) +
  theme(axis.text.x = element_text(angle = 90),
        plot.background = element_rect( color = "black"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5))

# Among Building type

train_data %>% 
  group_by(YrSold, BldgType) %>% 
  summarize(N_Sale = n()) %>% 
  ggplot(aes(x = YrSold, y = N_Sale, fill = YrSold)) +
  geom_bar(stat = "identity")  +
  scale_fill_gradient(low ="coral", high = "black") +
  facet_wrap(~ BldgType) +
  labs(
    x = "Year Sold",
    y = " Number of Sales",
    title = "No. of Sales vs. Year Sold across different Locations"
  ) +
  theme(axis.text.x = element_text(angle = 90),
        plot.background = element_rect( color = "black"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5))


# ********************* Further Pre-processing *********************************

# Processing Test data set------------------------------------------------------

# Replacing NA with "None" for all those variables where NA stand for missing 
# feature rather than missing information -------------------------------------- 

na_correction <- c("BsmtExposure", "GarageFinish", "FireplaceQu", 
                   "GarageQual" , "BsmtQual" )

for(i in 1:length(na_correction)){
  for(j in 1:nrow(test_data)){
    if(is.na(test_data[j,na_correction[i]])){
      test_data[j,na_correction[i]] <- "None"
    }
  }
}

# Omitting rows for the columns where less than 0.01% data is missing-----------

empty_1 <- numeric()

for(i in 1:nrow(test_data)){
  if(is.na(test_data$MSZoning[i])){
    empty_1 <- c(empty_1,i)
  }
}

for(i in 1:nrow(test_data)){
  if(is.na(test_data$GarageArea[i])){
    empty_1 <- c(empty_1,i)
  }
}

for(i in 1:nrow(test_data)){
  if(is.na(test_data$KitchenQual[i])){
    empty_1 <- c(empty_1,i)
  }
}

for(i in 1:nrow(test_data)){
  if(is.na(test_data$TotalBsmtSF[i])){
    empty_1 <- c(empty_1,i)
  }
}

test_data <- test_data[-(empty_1),]

# Changing Quality & Condition variables----------------------------------------

var_index_list <- c(
  28,31,54,58
)

for( i in var_index_list){
  for(j in 1:nrow(test_data)){
    if(test_data[j,i] == "Ex"){ 
      test_data[j,i] <- "5" }
    else if(test_data[j,i] == "Gd"){ 
      test_data[j,i] <- "4" }
    else if(test_data[j,i] == "TA"){ 
      test_data[j,i] <- "3" }
    else if(test_data[j,i] == "Fa"){ 
      test_data[j,i] <- "2" }
    else if(test_data[j,i] == "Po"){ 
      test_data[j,i] <- "1" }
    else{ 
      test_data[j,i] <- "0" }
  }
}

for(i in var_index_list){
  test_data[[i]]  <- as.numeric(test_data[[i]]) 
}

# Log price  and Age -----------------------------------------------------------

test_data <- test_data %>%
  mutate(log_price = log(SalePrice),
         Age = YrSold - YearBuilt) %>% 
  filter( Age >= 0)

# Processed test and train data ------------------------------------------------

# Removing variables that are no longer required--------------------------------

new_train_data <- train_data[, c("Age", "TotalBsmtSF","BsmtQual", "BsmtExposure",
                                 "GarageArea", "GarageFinish", "OverallQual", "ExterQual",
                                 "KitchenQual", "FireplaceQu", "GrLivArea", "FullBath",
                                 "MSZoning", "Neighborhood" , "Foundation", "log_price", 
                                 "SalePrice")]

new_test_data <- test_data[, c("Age", "TotalBsmtSF","BsmtQual", "BsmtExposure",
                               "GarageArea", "GarageFinish", "OverallQual", "ExterQual",
                               "KitchenQual", "FireplaceQu", "GrLivArea", "FullBath",
                               "MSZoning", "Neighborhood" , "Foundation", "log_price", 
                               "SalePrice")]


# ********************* Modeling *********************************************


# ******************************************************************************
# Problem 9 - Develop models to predict Sale Price based on the analysis 
#             performed in previous steps.
# ******************************************************************************

# Model 1- Model build on the basis of Quality variables and Age----------------

model_1 <- lm(log_price ~ OverallQual + BsmtQual + ExterQual + 
                KitchenQual + FireplaceQu + Age + BsmtExposure,
         new_train_data)

summary(model_1)

# Model 2- Model build on top of Size and Area variables------------------------

model_2 <- lm(log_price ~ TotalBsmtSF + GarageArea + GrLivArea + FullBath ,
              new_train_data)

summary(model_2)

# Model 3- Model build on top of variables with high Correlation values---------

model_3 <- lm(log_price ~ OverallQual + TotalBsmtSF + Age + 
                Neighborhood + GrLivArea + GarageFinish + Foundation,
              new_train_data)

summary(model_3)

# ******************************************************************************
# Problem 10 - Evaluate the models created and select the fit that best 
#              predicts the sales price.
# ******************************************************************************

# Evaluation model 1 -----------------------------------------------------------

rmse(model_1, new_train_data)
rmse(model_1, new_test_data)

# Evaluation model 2 -----------------------------------------------------------

rmse(model_2, new_train_data)
rmse(model_2, new_test_data)

# Evaluation model 3 -----------------------------------------------------------

rmse(model_3, new_train_data)
rmse(model_3, new_test_data)


# Model 3 is best fit, with a RMSE value of *0.147*
# Residual plot for model 3

model_pred <- predict(model_3, newdata = new_test_data)

model_data <- new_test_data %>% 
  mutate( y = log_price ,
          yBar = model_pred,
          diff = abs( y - yBar)) 

bad_model_data <- model_data %>% 
  filter( diff > 0.75) %>% 
  arrange(desc(diff))
  
model_residuals <- ggplot(model_data, aes(x = y, y = yBar, col = diff)) +
  geom_point(aes(x = y, y = yBar, color = diff)) +
  geom_point(data = bad_model_data, color = "red", size = 2) +
  scale_color_gradient(name = "|y - yBar|", limits = c(0, 1.5)) +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    x = "y",
    y = "yBar",
    title = "Linear model residuals"
  ) +
  my_theme

model_residuals


# ************************** Thank You *****************************************







