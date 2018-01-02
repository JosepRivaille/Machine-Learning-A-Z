# Multiple linear regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encode categorical columns
dataset$State = factor(dataset$State,
                       levels = levels(dataset$State),
                       labels = seq(1, length(state_levels)))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting the Multiple Linear Regression to the Training Set by Backward Elimination.
# In formula, dot === R.D.Spend + Administration + Marketing.Spend + State
regressor = lm(formula = Profit ~ .,
               data = training_set)
summary(regressor)

# (P> 0.05)

# States are the least statistical significant predictors
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = training_set)
summary(regressor)

# Administration is the least statistical significant predictor
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = training_set)
summary(regressor)

# Marketing Spend is the least statistical significant predictor
regressor = lm(formula = Profit ~ R.D.Spend,
               data = training_set)
summary(regressor)

# Predict Test Set
y_pred = predict(regressor, newdata = test_set)

# Display results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$R.D.Spend, y = test_set$Profit), colour = 'red') +
  geom_line(aes(x = test_set$R.D.Spend, y = y_pred), colour = 'blue') +
  ggtitle('RD Spend vs Profit') + xlab('RD Spend') + ylab('Profit')