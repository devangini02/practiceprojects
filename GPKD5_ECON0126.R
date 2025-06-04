rm(list = ls())

library(glmnet)
library(caret)
library(dplyr)
library(ggplot2)

setwd("C:/Users/vanal/OneDrive/Desktop/Uni Work/Machine Learning/Assignment")
data <- read.csv("bruhn2016.csv")
head(data)
data <- na.omit(data)

data$interaction_Female <- data$treatment_status * data$is_female
data$interaction_mother <- data$treatment_status * data$mother_went_to_secondary_school

#OLS
ols_model <- lm(test_score ~ ., data = data)
summary(ols_model)

# Splitting the data into training and testing sets
set.seed(123)
n <- nrow(data)
trainSize <- floor(0.7 * n)
trainingIndex <- sample(1:n, trainSize)
trainingData <- data[trainingIndex, ]
testingData <- data[-trainingIndex, ]

# Define predictor matrix and response vector for training data
x_matrix <- model.matrix(trainingData$test_score ~ . - 1, data = trainingData)

# Fit the Lasso model using cross-validation to find optimal lambda
cv_fit <- cv.glmnet(x_matrix, trainingData$test_score, alpha = 1, family = "gaussian")
plot(cv_fit)

cv_fit$lambda.min

coef(cv_fit, s = "lambda.min")

# Predicting the test scores
x_matrix_test <- model.matrix(testingData$test_score ~ . - 1, data = testingData)
predictions <- predict(cv_fit, newx = x_matrix_test, s = "lambda.min")

# Calculate Mean Squared Error (MSE)
mse <- mean((testingData$test_score - predictions)^2)
print(paste("MSE:", mse))


#compare with elastic net model
elastic_net_model <- glmnet(as.matrix(trainingData[, -1]), trainingData$test_score, alpha = 0.5, family = "gaussian")
plot(elastic_net_model)

set.seed(123)

cv_fit_elastic <- cv.glmnet(as.matrix(trainingData[, -1]), trainingData$test_score, alpha = 0.5, family = "gaussian")
plot(cv_fit_elastic)
cv_fit_elastic

cv_fit_elastic$lambda.min

coef(cv_fit_elastic, s = "lambda.min")

predictions_elastic <- predict(cv_fit_elastic, newx = as.matrix(testingData[, -1]), s = "lambda.min")
mse_elastic <- mean((testingData$test_score - predictions_elastic)^2)
print(paste("MSE Elastic Net:", mse_elastic))

###RANDOM FOREST#####
set.seed(123)
library(randomForest)
rf_model <- randomForest(test_score ~ ., data = trainingData, ntree = 500, mtry = 2)
rf_model

#predicting the test scores
rf_predictions <- predict(rf_model, testingData)
rf_mse <- mean((testingData$test_score - rf_predictions)^2)
print(paste("MSE Random Forest:", rf_mse))

#plotting the importance of the variables
importance(rf_model)
varImpPlot(rf_model)

#comparing forest, lasso and elastic net 
results <- data.frame(
  Model = c("Lasso", "Elastic Net", "Random Forest"),
  MSE = c(mse, mse_elastic, rf_mse)
)

print(results)

#plotting the results

ggplot(results, aes(x = Model, y = MSE, fill = Model)) + 
  geom_col() +  
  geom_text(aes(label = MSE), vjust = -0.5, position = position_dodge(width = 0.9), size = 3.5) +
  labs(title = "Comparison of MSE for Different Models",
       x = "Model",
       y = "Mean Squared Error") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Pastel1")


###Causal forest###
library(grf)
# Train a causal forest
cf <- causal_forest(X = data[, !names(data) %in% c("test_score", "treatment_status")],
                    Y = data$test_score,
                    W = as.numeric(data$treatment_status) - 1) 

# Estimate CATE for each observation
cate_estimates <- predict(cf)$predictions


# Estimate the average treatment effect
ate <- predict(cf, estimate_variance = TRUE)
ate <- mean(ate$predictions)
print(paste("Average Treatment Effect:", ate))

#CATE 
cate <- predict(cf, X = data[, !names(data) %in% c("is_female", "treatment_status")])
cate <- mean(cate$predictions)
print(paste("CATE:", cate))

data$cate_estimates <- cate_estimates  # Assuming cate_estimates is your vector of estimates

ggplot(data, aes(x = cate_estimates)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of CATE Estimates",
       x = "CATE Estimate",
       y = "Frequency") +
  theme_classic()

high_impact_threshold <- quantile(data$cate_estimates, 0.75) # Top 25%
data$high_impact <- ifelse(data$cate_estimates > high_impact_threshold, TRUE, FALSE)

data %>%
  group_by(high_impact) %>%
  summarize(across(c(is_female, has_computer_with_internet_at_home, mother_went_to_secondary_school, father_went_to_secondary_school), mean, na.rm = TRUE))

lm_model <- lm(test_score ~ treatment_status * is_female + treatment_status * has_computer_with_internet_at_home, data = data)
summary(lm_model)

high_impact_data <- data %>% filter(high_impact == TRUE)

# Explore high-impact sub-population characteristics
summary(high_impact_data)

# CATE Distribution
ggplot(data, aes(x = cate_estimates)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Distribution of CATE Estimates", x = "CATE Estimate", y = "Frequency")

# Characteristics of High-Impact Sub-Populations
ggplot(high_impact_data, aes(x = is_female, fill = as.factor(high_impact))) +
  geom_bar(position = "dodge") +
  labs(title = "Characteristics of High-Impact Sub-Populations", x = "Is Female", y = "Count") +
  scale_fill_discrete(name = "High Impact")

#CATE table for presentation
cate_table <- data %>%
  group_by(is_female, has_computer_with_internet_at_home) %>%
  summarize(cate = mean(cate_estimates, na.rm = TRUE))

cate_table

#average treatment effect by subgroup
ate_by_subgroup_extended <- data %>%
  group_by(is_female, has_computer_with_internet_at_home) %>%
  summarise(
    Treatment_Effect = mean(test_score[treatment_status == 1], na.rm = TRUE) - 
      mean(test_score[treatment_status == 0], na.rm = TRUE),
    .groups = 'drop' 
  )
ate_by_subgroup_extended
