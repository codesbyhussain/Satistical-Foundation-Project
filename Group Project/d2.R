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


################## First validation

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data)

# Calculate RMSE
rmse <- sqrt(sum((validation_data$Next_Tmax - predictions)^2) / nrow(validation_data))

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

################################################################################# Drop PPT, CC, and more

# Separate 'Next_Tmax' from train_data and remove it from train_data
response_train <- train_data$Next_Tmax
train_data_5 <- train_data[, !(names(train_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4', 'Solar.radiation', 'Present_Tmin'))]

# Separate 'Next_Tmax' from validation_data and remove it from validation_data
response_validation <- validation_data$Next_Tmax
validation_data_5 <- validation_data[, !(names(validation_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4',  'Solar.radiation', 'Present_Tmin'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_5)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data_5)

# Calculate RMSE
rmse <- sqrt(sum((validation_data_5$Next_Tmax - predictions)^2) / nrow(validation_data_5))

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
########################################################################### test dropping PPT, and CC
# Separate 'Next_Tmax' from train_data and remove it from train_data
response_train <- train_data$Next_Tmax
train_data_5 <- train_data[, !(names(train_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4', 'Solar.radiation', 'Present_Tmin'))]

# Separate 'Next_Tmax' from validation_data and remove it from validation_data
response_validation <- validation_data$Next_Tmax
test_data_5 <- test_data[, !(names(validation_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4', 'Solar.radiation', 'Present_Tmin'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_5)

# Make predictions on validation set
predictions <- predict(model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(sum((test_data_5$Next_Tmax - predictions)^2) / nrow(test_data_5))

# Display RMSE
rmse
summary(model)

############################################################## Statistically speaking: drop cc2, ppt3, ppt4, solar radiation

train_data_1 <- train_data[, !(names(train_data) %in% c('LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC2', 'Solar.radiation', 'Present_Tmin'))]
model <- lm(Next_Tmax ~ ., data = train_data_1)
summary(model)

validation_data_1 <- validation_data[, !(names(validation_data) %in% c('LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC2', 'Solar.radiation', 'Present_Tmin'))]
test_data_1 <- test_data[, !(names(test_data) %in% c('LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC2', 'Solar.radiation', 'Present_Tmin'))]

predictions <- predict(model, newdata = validation_data_1)
rmse <- sqrt(sum((validation_data_1$Next_Tmax - predictions)^2) / nrow(validation_data_1))
rmse

# Make predictions on validation set
predictions <- predict(model, newdata = test_data_1)
# Calculate RMSE
rmse <- sqrt(sum((test_data_1$Next_Tmax - predictions)^2) / nrow(test_data_1))
rmse


########################################################################### V2

# Scale all attributes between 0 and 1 using scale function
train_data_2 <- train_data[, !(names(train_data_2) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_2)

# Make predictions on validation set
validation_data_2 <- validation_data[, !(names(validation_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]
predictions <- predict(model, newdata = validation_data_2)

# Calculate RMSE
rmse <- sqrt(sum((validation_data_2$Next_Tmax - predictions)^2) / nrow(validation_data_2))

# Display RMSE
cat("Scaled and omitted validation: ", rmse)

# Make predictions on validation set
test_data_2 <- test_data[, !(names(test_data) %in% c('LDAPS_PPT1',	'LDAPS_PPT2',	'LDAPS_PPT3','LDAPS_PPT4','LDAPS_CC1', 'LDAPS_CC2', 'LDAPS_CC3', 'LDAPS_CC4'))]
predictions <- predict(model, newdata = test_data_2)

# Calculate RMSE
rmse <- sqrt(sum((test_data_2$Next_Tmax - predictions)^2) / nrow(test_data_2))

# Display RMSE
cat("Scaled and omitted test: ", rmse)
