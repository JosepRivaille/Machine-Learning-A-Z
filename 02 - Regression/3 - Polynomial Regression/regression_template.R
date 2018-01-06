# Polynomial regression

# Import the dataset
dataset <- read.csv('Dataset.csv')
dataset <- dataset[,]

# Split dataset into train and test dataset
library("caTools")
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# Fitting Polynomial Regression to the dataset
regressor <- lm(formula = Salary ~ poly(dataset$Level, degree = 4, raw=TRUE),
                data = dataset)
summary(regressor)

# Predict new data with Polynomial Regression
poly_values = poly(6.5, degree = 4, raw = TRUE)
y_pred <- predict(regressor, data.frame(Level = poly_values))

# Visualize Polynomial Regression result
library(ggplot2)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, data.frame(Level = x_grid))), colour = 'blue') +
  ggtitle('Salary vs Level (Polynomial)') + xlab('Level') + ylab('Salary')

