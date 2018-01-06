# Polynomial regression

# Import the dataset
dataset <- read.csv('Position_Salaries.csv')
dataset <- dataset[2:3]

# Fitting linear Regression to the dataset
lin_reg <- lm(formula = Salary ~ .,
              data = dataset)
summary(lin_reg)

# Fitting Polynomial Regression to the dataset
poly_reg <- lm(formula = Salary ~ poly(dataset$Level, degree = 4, raw=TRUE),
                data = dataset)
summary(poly_reg)

# Results visualization
library(ggplot2)

# Visualize Linear Regression results
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, dataset)), colour = 'blue') +
  ggtitle('Salary vs Level (Linear)') + xlab('Level') + ylab('Salary')
  
# Visualize Polynomial Regression result
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, dataset)), colour = 'blue') +
  ggtitle('Salary vs Level (Polynomial)') + xlab('Level') + ylab('Salary')

# Predict new data with Linear Regression
y_pred_lin <- predict(lin_reg, data.frame(Level = 6.5))

# Predict new data with Polynomial Regression
poly_values = poly(6.5, degree = 4, raw = TRUE)
y_pred_poly <- predict(poly_reg, data.frame(Level = poly_values))
