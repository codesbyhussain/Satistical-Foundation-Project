data <- read.csv("D:/Work/Master's College/Statistical Foundations/Group Project/Bias_correction_ucl.csv")

# Drop rows
data <- na.omit(data)

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

# Separate 'Next_Tmax' from train_data and remove it from train_data
train_data <- train_data[, !(names(train_data) %in% c('Date', 'station', 'Next_Tmin'))]

# Separate 'Next_Tmax' from validation_data and remove it from validation_data
validation_data <- validation_data[, !(names(validation_data) %in% c('Date', 'station', 'Next_Tmin'))]

# Separate 'Next_Tmax' from test_data and remove it from test_data
test_data <- test_data[, !(names(test_data) %in% c('Date', 'station', 'Next_Tmin'))]

########################################################################## V1
# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data)

# Make predictions on validation set
predictions <- predict(model, newdata = validation_data)

# Calculate RMSE
rmse <- sqrt(sum((validation_data$Next_Tmax - predictions)^2) / nrow(validation_data))

# Display RMSE
cat("Basic Training and validation output: ",rmse)


# Make predictions on validation set
predictions <- predict(model, newdata = test_data)

# Calculate RMSE
rmse <- sqrt(sum((test_data$Next_Tmax - predictions)^2) / nrow(test_data))

# Display RMSE
cat("Basic Training and testing output: ",rmse)
summary(model)
############################################################## Statistically speaking: drop cc2, ppt3, ppt4, solar radiation, Present_Tmin

train_data_1 <- train_data[, !(names(train_data) %in% c('LDAPS_PPT3','LDAPS_PPT4'))]
model <- lm(Next_Tmax ~ ., data = train_data_1)

validation_data_1 <- validation_data[, !(names(validation_data) %in% c('LDAPS_PPT3','LDAPS_PPT4'))]
test_data_1 <- test_data[, !(names(test_data) %in% c('LDAPS_PPT3','LDAPS_PPT4'))]

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
train_data_1 <- as.data.frame(scale(train_data))
train_data_1 <- train_data_1[, !(names(train_data_1) %in% c('LDAPS_PPT3','LDAPS_PPT4'))]

# Fit multiple linear regression on training data
model <- lm(Next_Tmax ~ ., data = train_data_1)

# Make predictions on validation set
validation_data_1 <- as.data.frame(scale(validation_data))
validation_data_1 <- validation_data_1[, !(names(validation_data_1) %in% c('LDAPS_PPT3','LDAPS_PPT4'))]
predictions <- predict(model, newdata = validation_data_1)

# Calculate RMSE
rmse <- sqrt(sum((validation_data_1$Next_Tmax - predictions)^2) / nrow(validation_data_1))

# Display RMSE
cat("Scaled and omitted validation: ", rmse)

# Make predictions on validation set
test_data_1 <- as.data.frame(scale(test_data))
test_data_1 <- test_data_1[, !(names(test_data_1) %in% c('LDAPS_PPT3','LDAPS_PPT4'))]
predictions <- predict(model, newdata = test_data_1)

# Calculate RMSE
rmse <- sqrt(sum((test_data_1$Next_Tmax - predictions)^2) / nrow(test_data_1))

# Display RMSE
cat("Scaled and omitted test: ", rmse)

# Plotting residuals
plot(model, which = 1)  # Residuals vs Fitted values
plot(model, which = 2)  # Normal Q-Q plot of residuals
plot(model, which = 3)  # Scale-Location plot (sqrt(|residuals|) vs Fitted values)
plot(model, which = 4)  # Residuals vs leverage

