# Load required libraries
library(tidyverse)

# Read the dataset
airbnb_data <- read.csv("C:/Users/Bhargav Dasari/Documents/SPRING-2024/STAT-515/project/cleaned_data.csv")  

#+ fig.width=7, fig.height=5.5, dpi=600 
# Visualize the relationship between number of listings managed by a host and price variation across different room types
ggplot(airbnb_data, aes(x = calculated_host_listings_count, y = price, color = room_type)) +
  geom_point() +
  facet_wrap(~ room_type) +
  labs(x = "Number of Listings Managed by Host", y = "Price", title = "Price Variation by Number of Listings and Room Type")

# Linear regression to quantify the relationship
lm_model <- lm(price ~ calculated_host_listings_count * room_type, data = airbnb_data)
summary(lm_model)

# Build Poisson regression model
poisson_model <- glm(number_of_reviews ~ calculated_host_listings_count + room_type, data = airbnb_data, family = poisson)

# Summarize the model
summary(poisson_model)



# Preprocess the data
# Encode categorical variables
airbnb_data <- airbnb_data %>%
  mutate(location_encoded = as.factor(neighbourhood_group),
         room_type_encoded = as.factor(room_type))

# Handle missing values if any

# Split the data into train and test sets
set.seed(123)
train_indices <- sample(1:nrow(airbnb_data), 0.8 * nrow(airbnb_data))
train_data <- airbnb_data[train_indices, ]
test_data <- airbnb_data[-train_indices, ]

# Train regression model
regression_model <- lm(number_of_reviews ~ location_encoded + room_type_encoded + price, data = train_data)
summary(regression_model)

# Predictions on test set
predictions <- predict(regression_model, newdata = test_data)

# Evaluate model performance
RMSE <- sqrt(mean((test_data$number_of_reviews - predictions)^2))
MAE <- mean(abs(test_data$number_of_reviews - predictions))

RMSE
MAE


# Load required library
library(randomForest)

# Encode categorical variables
airbnb_data$location_encoded <- as.factor(airbnb_data$neighbourhood_group)
airbnb_data$room_type_encoded <- as.factor(airbnb_data$room_type)

# Handle missing values if any

# Split the data into train and test sets
set.seed(123)
train_indices <- sample(1:nrow(airbnb_data), 0.8 * nrow(airbnb_data))
train_data <- airbnb_data[train_indices, ]
test_data <- airbnb_data[-train_indices, ]

# Train Random Forest model
rf_model <- randomForest(number_of_reviews ~ location_encoded + room_type_encoded + price, data = train_data, ntree = 500)
print(rf_model)

# Predictions on test set
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate model performance
rf_rmse <- sqrt(mean((test_data$number_of_reviews - rf_predictions)^2))
rf_mae <- mean(abs(test_data$number_of_reviews - rf_predictions))

print(paste("Random Forest RMSE:", rf_rmse))
print(paste("Random Forest MAE:", rf_mae))

# Feature Importance
varImpPlot(rf_model)

