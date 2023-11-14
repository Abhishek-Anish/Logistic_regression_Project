# Loading necessary libraries
library(dplyr)
library(caret)

data <- read.csv("C:/Users/rocka/OneDrive/Documents/cleaned_hepatitis.csv", as.is=FALSE)


# Converting Class (Live or Die) to a factor
data$Class <- as.factor(data$Class)

# Splitting the data into training and testing sets
set.seed(123)
training.samples <- createDataPartition(data$Class, p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

# Training the model
model <- glm(Class ~ ., family = binomial(), data = train.data)


# Make predictions using the model
predictions <- predict(model, newdata = test.data, type = "response")

# Converting probabilities to class labels
predictions <- ifelse(predictions > 0.5, "Live", "Die")

predictions <- factor(predictions, levels = levels(test.data$Class))

# Evaluating the model
confusionMatrix(predictions, test.data$Class)
