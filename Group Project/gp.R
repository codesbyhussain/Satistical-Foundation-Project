data <- read.csv("D:/Work/Master's College/Statistical Foundations/Group Project/Bias_correction_ucl.csv")
dim(data)
# Drop rows
data <- na.omit(data)
dim(data)
# Sort the data by date
sorted_data <- data[order(data$Date), ]

# Calculate the row indices for the splits
n <- nrow(sorted_data)
train_indices <- 1:round(0.6 * n)
validation_indices <- (round(0.6 * n) + 1):round(0.8 * n)
test_indices <- (round(0.8 * n) + 1):n

# Split the data based on the calculated indices
train_data <- sorted_data[train_indices, ]
validation_data <- sorted_data[validation_indices, ]
test_data <- sorted_data[test_indices, ]

# View the dimensions of the split datasets
dim(train_data)
dim(validation_data)
dim(test_data)

# Separate 'Next_Tmax' from train_data and remove it from train_data
response_train <- train_data$Next_Tmax
train_data <- train_data[, !(names(train_data) %in% c('Date', 'station', 'Next_Tmin'))]

# Separate 'Next_Tmax' from validation_data and remove it from validation_data
response_validation <- validation_data$Next_Tmax
validation_data <- validation_data[, !(names(validation_data) %in% c('Date', 'station', 'Next_Tmin'))]

# Separate 'Next_Tmax' from test_data and remove it from test_data
response_test <- test_data$Next_Tmax
test_data <- test_data[, !(names(test_data) %in% c('Date', 'station', 'Next_Tmin'))]

# List of columns from 'data'
column_list <- names(train_data)

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data)

# Calculate RMSE
rmse <- sqrt(sum((validation_data$Next_Tmax - predictions)^2) / nrow(validation_data))

# Display RMSE
rmse
summary(model)

########################################################################################################## Drop lat and lon


# Separate 'Next_Tmax' from train_data and remove it from train_data
response_train <- train_data$Next_Tmax
train_data_1 <- train_data[, !(names(train_data) %in% c('lat', 'lon'))]

# Separate 'Next_Tmax' from validation_data and remove it from validation_data
response_validation <- validation_data$Next_Tmax
validation_data_1 <- validation_data[, !(names(validation_data) %in% c('lat', 'lon'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_1)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data_1)

# Calculate RMSE
rmse <- sqrt(sum((validation_data_1$Next_Tmax - predictions)^2) / nrow(validation_data_1))

# Display RMSE
rmse
summary(model)

########################################################################################################## Drop all PPT

# Separate 'Next_Tmax' from train_data and remove it from train_data
response_train <- train_data$Next_Tmax
train_data_2 <- train_data[, !(names(train_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4'))]

# Separate 'Next_Tmax' from validation_data and remove it from validation_data
response_validation <- validation_data$Next_Tmax
validation_data_2 <- validation_data[, !(names(validation_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_2)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data_2)

# Calculate RMSE
rmse <- sqrt(sum((validation_data_2$Next_Tmax - predictions)^2) / nrow(validation_data_2))

# Display RMSE
rmse
summary(model)

###################################################### Sum and mean Cloud Covers
# Make copies of train_data and validation_data
train_data_3 <- train_data
validation_data_3 <- validation_data

# Sum the columns 'LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', and 'LDAPS_CC4' in the copies
train_data_3$summed_columns <- rowSums(train_data_3[, c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4')])

validation_data_3$summed_columns <- rowSums(validation_data_3[, c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4')])

# Drop the columns 'LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4' from the copies
train_data_3 <- train_data_3[, !(names(train_data_3) %in% c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]
validation_data_3 <- validation_data_3[, !(names(validation_data_3) %in% c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_3)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data_3)

# Calculate RMSE
rmse <- sqrt(sum((validation_data_3$Next_Tmax - predictions)^2) / nrow(validation_data_3))

# Display RMSE
rmse
summary(model)
#################################################################

# Make copies of train_data and validation_data
train_data_4 <- train_data
validation_data_4 <- validation_data

# Average the columns 'LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', and 'LDAPS_CC4' in the copies
train_data_4$averaged_columns <- rowMeans(train_data_4[, c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4')], na.rm = TRUE)

validation_data_4$averaged_columns <- rowMeans(validation_data_4[, c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4')], na.rm = TRUE)

# Drop the columns 'LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4' from the copies
train_data_4 <- train_data_4[, !(names(train_data_4) %in% c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]
validation_data_4 <- validation_data_4[, !(names(validation_data_4) %in% c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_4)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data_4)

# Calculate RMSE
rmse <- sqrt(sum((validation_data_4$Next_Tmax - predictions)^2) / nrow(validation_data_4))

# Display RMSE
rmse
summary(model)

################################################################################# Drop PPT, CC

# Separate 'Next_Tmax' from train_data and remove it from train_data
response_train <- train_data$Next_Tmax
train_data_5 <- train_data[, !(names(train_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]

# Separate 'Next_Tmax' from validation_data and remove it from validation_data
response_validation <- validation_data$Next_Tmax
validation_data_5 <- validation_data[, !(names(validation_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_5)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data_5)

# Calculate RMSE
rmse <- sqrt(sum((validation_data_5$Next_Tmax - predictions)^2) / nrow(validation_data_5))

# Display RMSE
rmse
summary(model)

################################################################# PCA on CC + drop PPT

# Selecting only the cloud cover columns for PCA
tcloud_cover_data <- train_data[, c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4')]
vcloud_cover_data <- validation_data[, c('LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4')]

# Make copies
train_data_6 = train_data
validation_data_6 = validation_data

# Applying PCA
tpca_result <- prcomp(tcloud_cover_data, scale. = TRUE)
vpca_result <- prcomp(vcloud_cover_data, scale. = TRUE)

# Extracting the first principal component
tpca <- tpca_result$x[, 1]
vpca <- vpca_result$x[, 1]
# Adding the first principal component as a new feature
train_data_6$PCA_CC <- tpca
validation_data_6$PCA_CC <- vpca

# Drop the columns 'LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4' from the copies
train_data_6 <- train_data_6[, !(names(train_data_6) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]
validation_data_6 <- validation_data_6[, !(names(validation_data_6) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_6)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data_6)

# Calculate RMSE
rmse <- sqrt(sum((validation_data_6$Next_Tmax - predictions)^2) / nrow(validation_data_6))

# Display RMSE
rmse
summary(model)




######################################### Test Normal

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data)

# Make predictions on validation set
predictions <- predict(model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(sum((test_data$Next_Tmax - predictions)^2) / nrow(test_data))

# Display RMSE
rmse
summary(model)

#################################################### Test dropping PPT, and CC

# Separate 'Next_Tmax' from train_data and remove it from train_data
response_train <- train_data$Next_Tmax
train_data_5 <- train_data[, !(names(train_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]

# Separate 'Next_Tmax' from validation_data and remove it from validation_data
response_validation <- validation_data$Next_Tmax
test_data_5 <- test_data[, !(names(validation_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_5)

# Make predictions on validation set
predictions <- predict(model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(sum((test_data_5$Next_Tmax - predictions)^2) / nrow(test_data_5))

# Display RMSE
rmse
summary(model)

###############################################################

