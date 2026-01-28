## Installation

Install the development version from GitHub

``` r
devtools::install_github("imbs-hl/SimData")
```
Example: Simulating Data and Fitting Models
This example demonstrates how to simulate data and fit a random forest
and a logistic regression model.
library(SimData)
library(ranger)

set.seed(123456)

# Define block structure
block_info <- data.frame(
  block_id    = c("A", "B", "C"),
  block_size  = c(5, 5, 100),
  rho         = c(0.9, 0.9, 0),
  effect_size = c(1, 1, 0)
)

# Simulate data
res_sim <- simulate_data_flexible(
  n = 600,
  block_info = block_info
)

sim_data <- res_sim$data

# Train / test split
train <- sim_data[1:400, ]
test  <- sim_data[401:600, ]

# Random forest
rf <- ranger(
  data = train,
  dependent.variable.name = "y",
  probability = TRUE,
  importance = "impurity_corrected"
)

# Logistic regression
log_reg <- glm(
  y ~ .,
  data = train,
  family = binomial
)

# Predictions
pred <- data.frame(
  truth = test$y,
  pred_rf = predict(rf, test)$predictions[, 2],
  pred_log_reg = predict(log_reg, test, type = "response")
)

# Visualization
boxplot(pred$pred_rf ~ pred$truth,
        main = "Random Forest Predictions",
        xlab = "True class",
        ylab = "Predicted probability")

boxplot(pred$pred_log_reg ~ pred$truth,
        main = "Logistic Regression Predictions",
        xlab = "True class",
        ylab = "Predicted probability")

# Variable importance
imp <- importance(rf)
barplot(imp, horiz = TRUE, las = 1,
        main = "Random Forest Variable Importance")
