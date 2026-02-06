## Installation

Install the development version from GitHub

``` r
devtools::install_github("imbs-hl/SimData")
```
##  Example: Simulating Data and training random forest models
This example demonstrates how to simulate data and train random forest
models.

``` r
library(SimData)
library(ranger)

set.seed(123456)

# Define block structure (2 effect blocks with high correlation, 1 null block 
# with independent predictors)
block_info <- data.frame(
  block_id    = c("A", "B", "C"),
  block_size  = c(5, 5, 10),
  rho         = c(0.9, 0.9, 0),
  effect_size = c(1, 1, 0)
)

# Simulate data with binary outcome
res_sim <- simulate_data_flexible(
  n = 600,
  block_info = block_info,
  type = "binary"
)

# Extract data
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

# Predictions
pred <- data.frame(
  truth = test$y,
  pred_rf = predict(rf, test)$predictions[, 2]
)

# Visualization
boxplot(pred$pred_rf ~ pred$truth,
        main = "Random Forest Predictions",
        xlab = "True class",
        ylab = "Predicted probability")

# Variable importance
imp <- importance(rf)
print(sort(imp, decreasing = TRUE))
barplot(imp, horiz = TRUE, las = 1,
        main = "Random Forest Variable Importance")


# Simulate data with continuous outcome
res_sim <- simulate_data_flexible(
  n = 600,
  block_info = block_info,
  type = "continuous"
)

# Extract data
sim_data <- res_sim$data

# Train / test split
train <- sim_data[1:400, ]
test  <- sim_data[401:600, ]

# Random forest
rf <- ranger(
  data = train,
  dependent.variable.name = "y",
  importance = "impurity_corrected"
)

# Predictions
pred <- data.frame(
  truth = test$y,
  pred_rf = predict(rf, test)$predictions
)

# Visualization
plot(pred$pred_rf ~ pred$truth,
  main = "Random Forest Predictions",
  xlab = "True values",
  ylab = "Predicted values")

# Variable importance
imp <- importance(rf)
print(sort(imp, decreasing = TRUE))
barplot(imp, horiz = TRUE, las = 1,
        main = "Random Forest Variable Importance")

```
