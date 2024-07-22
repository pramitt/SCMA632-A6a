# Set the working directory
setwd("C://Users//prami//Desktop//SCMA 632//SCMA 632//assignments//A6")

# Read the data from a local CSV file
data <- read.csv("TESLAdata.csv")

# Convert the date column to Date type if necessary
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")


# Install and load necessary packages
if (!requireNamespace("xts", quietly = TRUE)) {
  install.packages("xts")
}
if (!requireNamespace("quantmod", quietly = TRUE)) {
  install.packages("quantmod")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("forecast", quietly = TRUE)) {
  install.packages("forecast")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

library(xts)
library(quantmod)
library(ggplot2)
library(forecast)
library(caret)
library(rpart)
library(randomForest)

# Ensure the data is in time series format
df <- xts(data$Adj.Close, order.by = data$Date)

# Display first few rows of data
head(df)

# Select the target variable (Adjusted Close) and check for missing values
print("Missing values:")
print(sum(is.na(df)))

# Plot the data
ggplot(data.frame(Date = index(df), Adj.Close = df), aes(x = Date, y = Adj.Close)) +
  geom_line() +
  ggtitle('tesladata.NS Adj Close Price') +
  xlab('Date') +
  ylab('Adj Close Price') +
  theme_minimal()

# Decompose the time series
decomposed <- decompose(ts(df, frequency = 12), type = "multiplicative")
autoplot(decomposed)

# Split the data into training and test sets
trainIndex <- createDataPartition(y = df, p = 0.8, list = FALSE)
train_data <- df[trainIndex,]
test_data <- df[-trainIndex,]

# Resample data to monthly frequency
monthly_data <- to.monthly(df, indexAt = "lastof", OHLC = FALSE)
trainIndex <- createDataPartition(y = monthly_data, p = 0.8, list = FALSE)
train_data <- monthly_data[trainIndex,]
test_data <- monthly_data[-trainIndex,]

# Fit Holt-Winters model and forecast
holt_winters_model <- HoltWinters(ts(train_data, frequency = 12), seasonal = "multiplicative")
holt_winters_forecast <- forecast(holt_winters_model, h = 12)

# Convert xts objects to data frames
train_data_df <- data.frame(Date = index(train_data), Cl = coredata(train_data))
test_data_df <- data.frame(Date = index(test_data), Cl = coredata(test_data))

# Inspect the structure to confirm the columns
str(train_data_df)
str(test_data_df)

# Remove any NA values
train_data_df <- na.omit(train_data_df)
test_data_df <- na.omit(test_data_df)

# Convert xts objects to data frames
train_data_df <- data.frame(Date = index(train_data), Cl = coredata(train_data))
test_data_df <- data.frame(Date = index(test_data), Cl = coredata(test_data))

# Inspect the structure to confirm the columns
str(train_data_df)
str(test_data_df)

# Ensure Cl is numeric and there are no factors/characters in the data
train_data_df$Cl <- as.numeric(train_data_df$Cl)
test_data_df$Cl <- as.numeric(test_data_df$Cl)

# Remove any NA values
train_data_df <- na.omit(train_data_df)
test_data_df <- na.omit(test_data_df)

# Check for any non-numeric columns in train_data_df excluding 'Date'
non_numeric_cols <- sapply(train_data_df[, -1], function(x) !is.numeric(x))
if (any(non_numeric_cols)) {
  stop("There are non-numeric columns in the training data.")
}


str(train_data_df)
summary(train_data_df)
train_data_df$Cl <- as.factor(train_data_df$Cl)
sum(is.na(train_data_df))
dt_model <- rpart(Cl ~ ., data = train_data_df)


# Fit Decision Tree model
dt_model <- rpart(Cl ~ ., data = train_data_df[, -1, drop = FALSE])  # Exclude the Date column
y_pred_dt <- predict(dt_model, test_data_df[, -1, drop = FALSE])    # Exclude the Date column

dt_model <- rpart(Cl ~ ., data = train_data_df[, -1, drop = FALSE])  # Exclude the Date column
Error in 1:numclass : result would be too long a vector
In addition: Warning message:
  In max(y[!is.na(y)]) : no non-missing arguments to max; returning -Inf
> y_pred_dt <- predict(dt_model, test_data_df[, -1, drop = FALSE])    # Exclude the Date column
Error in eval(predvars, data, env) : object 'Date' not found


# Load required libraries
library(rpart)

# Step 1: Check the structure of the data
print(str(train_data_df))

# Step 2: Ensure the response variable 'Cl' is a factor
if (!is.factor(train_data_df$Cl)) {
  train_data_df$Cl <- as.factor(train_data_df$Cl)
}

# Step 3: Check for missing values in the response variable 'Cl'
if (any(is.na(train_data_df$Cl))) {
  stop("Response variable 'Cl' contains missing values.")
}

# Step 4: Exclude the Date column from the training data
# Adjust column index if Date is not the first column
train_data_df_clean <- train_data_df[, -1, drop = FALSE]

# Step 5: Check if the remaining columns are numeric or factors
print(sapply(train_data_df_clean, class))

# Step 6: Fit the rpart model
dt_model <- rpart(Cl ~ ., data = train_data_df_clean)

# Step 7: Ensure the test data is formatted si

# Fit Random Forest model
rf_model <- randomForest(Cl ~ ., data = train_data_df[, -1, drop = FALSE], ntree = 100)  # Exclude the Date column
y_pred_rf <- predict(rf_model, test_data_df[, -1, drop = FALSE])   # Exclude the Date column

# Load required libraries
library(randomForest)

# Step 1: Check the structure of the data
print(str(train_data_df))

# Step 2: Ensure the response variable 'Cl' is a factor
if (!is.factor(train_data_df$Cl)) {
  train_data_df$Cl <- as.factor(train_data_df$Cl)
}

# Step 3: Check for missing values in the response variable 'Cl'
if (any(is.na(train_data_df$Cl))) {
  stop("Response variable 'Cl' contains missing values.")
}

# Step 4: Exclude the Date column from the training data
# Adjust column index if Date is not the first column
train_data_df_clean <- train_data_df[, -1, drop = FALSE]

# Step 5: Verify column names and classes
print(names(train_data_df_clean))
print(sapply(train_data_df_clean, class))

# Step 6: Fit the randomForest model
rf_model <- randomForest(Cl ~ ., data = train_data_df_clean, ntree = 100)

# Step 7: Prepare the test data similarly
test_data_df_clean <- test_data_df[, -1, drop = FALSE]
print(sapply(test_data_df_clean, class))

# Step 8: Predict on the cleaned test data
y_pred_rf <- predict(rf_model, test_data_df_clean)

# Output the predictions
print(y_pred_rf)

# Calculate Mean Squared Error
mse_dt <- mean((test_data_df$Cl - y_pred_dt)^2)
mse_rf <- mean((test_data_df$Cl - y_pred_rf)^2)
print(paste("Decision Tree Mean Squared Error:", mse_dt))
print(paste("Random Forest Mean Squared Error:", mse_rf))

# Create a data frame for plotting
plot_df <- data.frame(
  Date = test_data_df$Date,
  True_Values = test_data_df$Cl,
  DT_Predictions = y_pred_dt,
  RF_Predictions = y_pred_rf
)

# Plot using ggplot2
library(ggplot2)

ggplot(plot_df, aes(x = Date)) +
  geom_line(aes(y = True_Values, color = "True Values")) +
  geom_line(aes(y = DT_Predictions, color = "Decision Tree Predictions")) +
  geom_line(aes(y = RF_Predictions, color = "Random Forest Predictions")) +
  ggtitle("Decision Tree & Random Forest: Predictions vs True Values") +
  xlab("Time") + ylab("Close Price") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("True Values" = "black", "Decision Tree Predictions" = "blue", "Random Forest Predictions" = "red"))
