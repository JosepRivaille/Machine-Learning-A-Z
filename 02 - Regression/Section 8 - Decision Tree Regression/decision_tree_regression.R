# Decision Tree regression

# Import the dataset
dataset <- read.csv('Position_Salaries.csv')
dataset <- dataset[2:3]

# Split dataset into train and test dataset
# library("caTools")
# set.seed(123)
# split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
# training_set <- subset(dataset, split == TRUE)
# test_set <- subset(dataset, split == FALSE)

# Fitting Decision Tree Regression to the dataset
library(rpart)
regressor <- rpart(formula = Salary ~ .,
                   data = dataset,
                   control = rpart.control(minsplit = 1))
summary(regressor)

# Predict new data with Polynomial Regression
y_pred <- predict(regressor, data.frame(Level = 6.5))

# Visualize Polynomial Regression result
library(ggplot2)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, data.frame(Level = x_grid))), colour = 'blue') +
  ggtitle('Salary vs Level (Polynomial)') + xlab('Level') + ylab('Salary')

