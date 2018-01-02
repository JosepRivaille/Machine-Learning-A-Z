# Support Vector regression

# Import the dataset
dataset <- read.csv('Position_Salaries.csv')
dataset <- dataset[2:3]

# Fitting SV Regression to the dataset
library(e1071)
regressor <- svm(formula = Salary ~ .,
                 data = dataset,
                 type = 'eps-regression')
summary(regressor)

# Predict new data with Polynomial Regression
y_pred <- predict(regressor, data.frame(Level = 6.5))

# Visualize Polynomial Regression result
library(ggplot2)
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, data.frame(Level = x_grid))), colour = 'blue') +
  ggtitle('Salary vs Level (Polynomial)') + xlab('Level') + ylab('Salary')

