library(caret)

train <- read.csv('train.csv')

# Missing value
View(colSums(is.na(train))) # Checking the missing value
sum(is.na(train$LotFrontage)) # Checking the missing value by feature
train[is.na(train$LotFrontage),'LotFrontage'] <- mean(train$LotFrontage,na.rm = TRUE)

# Normalization
std_scaler <- preProcess(train['LotArea'],method = c('range'))
train['LotArea'] <- predict(std_scaler,train['LotArea'])
train$LotArea

# Data Transformation with log
SalePrice_transformation <- log(train$SalePrice)
hist(SalePrice_transformation)

# Outlier handling
# First iteration 
SalePrice <- train$SalePrice
boxplot(SalePrice)

# IQR Method 
# Find the upper bound and lower bound
q1 <- quantile(SalePrice,0.25)
q3 <- quantile(SalePrice,0.75)

iqr <- q3 - q1

upper_bound <- q3 + 1.5 * iqr
lower_bound <- q1 - 1.5 * iqr

# Filter 

filter_train_iqr <- SalePrice > lower_bound & SalePrice < upper_bound
train_iqr <- train[filter_train_iqr,]
boxplot(train_iqr$SalePrice)

# Second iteration,
# remove all the outliers with 5 times iteration
SalePrice <- train_iqr$SalePrice
boxplot(SalePrice)

# IQR Method 
# Find the upper bound and lower bound
q1 <- quantile(SalePrice,0.25)
q3 <- quantile(SalePrice,0.75)

iqr <- q3 - q1

upper_bound <- q3 + 1.5 * iqr
lower_bound <- q1 - 1.5 * iqr

# Filter 
filter_train_iqr <- SalePrice > lower_bound & SalePrice < upper_bound
train_iqr <- train_iqr[filter_train_iqr,]
SalePrice_non_outlier <- train_iqr$SalePrice
boxplot(SalePrice_non_outlier)

# One-hot Encoding
table(train$HouseStyle)

dummy <- dummyVars("~.",data=train['HouseStyle'])
dummy_var <- data.frame(predict(dummy, newdata = train['HouseStyle']))
View(dummy_var)

# Combine all the data that have processed to new data frame
LotFrontage_non_na <- train$LotFrontage
LotArea_normalization <- train$LotArea
SalePrice_transformation
one_hot_encoding_HouseStyle <- dummy_var
SalePrice_non_outlier

hasil <- data.frame(LotFrontage_non_na, LotArea_normalization, SalePrice_transformation, dummy_var)
View(colSums(is.na(hasil)))

SalePrice_non_outlier <- data.frame(SalePrice_non_outlier)
boxplot(SalePrice_non_outlier)

# Create the new data frame to csv
write.csv(hasil, 'Data_Cleaning_Aliriza_HM_DS.7.csv')
write.csv(SalePrice_non_outlier, 'SalePrice_non_outlier_Aliriza_HM_DS.7.csv')

### Check csv file here ###
# File csv SalePrice_non_outlier : https://drive.google.com/file/d/1w2JZZVd7Wl4u0K7l8DZwSdc8YDBGoYW-/view?usp=sharing
# File csv hasil : https://drive.google.com/file/d/1_j4tO82zvBk-5YGvii28Purhh7pxmVTA/view?usp=sharing