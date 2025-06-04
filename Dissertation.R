#Load all the packages and set working directory 
rm(list = ls())
library(ggplot2)
library(tidyverse)
library(caret)
library(grplasso)
library(gglasso)
library(dplyr)
library(glmnet)
library(glmnetUtils)
library(leaps)
library(boot)
library(tidyverse)
library(xgboost)
library(parallel)
library(tictoc)
library(rstudioapi)
library(penalized)
library(readxl)
library(lubridate)
setwd("C:/Users/vanal/Desktop/Uni Work/Dissertation/Data")

#Load the data
data_healthcare_expen <- read.csv("Personal consumption expenditure - Healthcare.csv")
data_sourceoffunds <- read_excel("Table 03 National Health Expenditures, by Source of Funds.xlsx", sheet = 1, skip = 2)
data_mat_mort <- read_excel("Maternal mortality ratio - US (WHO and NHE).xlsx")

#clean data
data_healthcare_expen <- data_healthcare_expen %>% 
  rename("Personal Healthcare Expenditure" = DHLCRC1Q027SBEA) %>%
  rename(Year = DATE) 

#only keep data from 1960, 1970, 1980, 1990, 2001-2022
data_healthcare_expen <- data_healthcare_expen[-c(1:40),]
data_healthcare_expen <- data_healthcare_expen %>%
  mutate(Year = year(Year)) %>%
  group_by(Year) %>%
  summarise(Annual_Healthcare_Expenditure = sum(`Personal Healthcare Expenditure`))
new_data <- data.frame(
  Year = 2024,
  Annual_Healthcare_Expenditure = 11197 # Replace with the actual value for 2024
)

data_healthcare_expen <- rbind(data_healthcare_expen, new_data)

#delete last 4 rows
data_sourceoffunds <- data_sourceoffunds[-c(13:29),]

#change column to year (1960 to 2022) for data

#rename first column to source of funds
data_sourceoffunds <- data_sourceoffunds %>%
  rename("Source of Funds" = ...1) %>%
  rename('1960' = "Amount in Billions") %>%
  rename('1970' = "...3") %>%
  rename('1980' = "...4") %>%
  rename('1990' = "...5") %>%
  rename('2000' = "...6") %>%
  rename('2001' = "...7") %>%
  rename('2002' = "...8") %>%
  rename('2003' = "...9") %>%
  rename('2004' = "...10") %>%
  rename('2005' = "...11") %>%
  rename('2006' = "...12") %>%
  rename('2007' = "...13") %>%
  rename('2008' = "...14") %>%
  rename('2009' = "...15") %>%
  rename('2010' = "...16") %>%
  rename('2011' = "...17") %>%
  rename('2012' = "...18") %>%
  rename('2013' = "...19") %>%
  rename('2014' = "...20") %>%
  rename('2015' = "...21") %>%
  rename('2016' = "...22") %>%
  rename('2017' = "...23") %>%
  rename('2018' = "...24") %>%
  rename('2019' = "...25") %>%
  rename('2020' = "...26") %>%
  rename('2021' = "...27") %>%
  rename('2022' = "...28")
  

#change data format from wide to long format

colnames(data_sourceoffunds) <- paste("X", colnames(data_sourceoffunds), sep = "")

#rename first column
colnames(data_sourceoffunds)[1] <- "Source of Funds"

data_sourceoffunds <- data_sourceoffunds %>%
  mutate(across(starts_with("X"), as.numeric))
long_data <- pivot_longer(data_sourceoffunds, cols = starts_with("X"), names_to = "Year", values_to = "Value")

# Remove the leading 'X' from the year values
long_data$Year <- as.numeric(sub("X", "", long_data$Year))

#subset the long data to only include federal
long_data_federal <- long_data[long_data$`Source of Funds` == "National Health Expenditures",]

# New data for the year 2024
new_data <- data.frame(
  `Source of Funds` = 'National Health Expenditure',
  Year = c(2023, 2024),
  Value = c(4666.2, 4897.7) # Replace with the actual value for 2024
)

#rename column
colnames(new_data) <- c("Source of Funds", "Year", "Value")

long_data_federal <- rbind(long_data_federal, new_data)

#import number of abortion data
data_abortion <- read_excel("Number of abortions - US.xlsx")

merge_data <- merge(data_mat_mort, long_data_federal, by = "Year")

final_data <- merge(merge_data, data_healthcare_expen, by = "Year")

final_data <- merge(final_data, data_abortion, by = "Year")
#rename value to Nat_health_exp
final_data <- final_data %>%
  rename("Nat_health_exp" = Value) %>%
  rename("Maternal_mortality_ratio" = `Maternal Mortality Ratio per 100,000 live births`) %>%
  rename("Number_of_abortions" = `No. of legal abortions reported in the U.S. from 1973 to 2021 (in 1,000s)`)

#import gdp data 
data_gdp <- read.csv("GDP.csv")
data_gdp <- data_gdp %>%
  rename("GDP" = GDP) %>%
  rename("Year" = DATE)

data_gdp <- data_gdp %>%
  mutate(Year = year(Year)) %>%
  group_by(Year) %>%
  summarise(Annual_GDP = sum(GDP))

#import government expenditure data
data_gov_exp <- read.csv("FGEXPND.csv")
data_gov_exp <- data_gov_exp %>%
  rename("Gov_exp" = FGEXPND) %>%
  rename("Year" = DATE)
data_gov_exp <- data_gov_exp %>%
  mutate(Year = year(Year)) %>%
  group_by(Year) %>%
  summarise(Annual_Gov_exp = sum(Gov_exp))

#import inflation data
data_inflation <- read.csv("FPCPITOTLZGUSA.csv")
data_inflation <- data_inflation %>%
  rename("Inflation" = FPCPITOTLZGUSA) %>%
  rename("Year" = DATE)
#change date format from yyyy-mm-dd to yyyy
data_inflation$Year <- format(as.Date(data_inflation$Year, format = "%d/%m/%Y"), "%Y")


#merge data 
non_predictors <- merge(data_gdp, data_gov_exp, by = "Year")
non_predictors <- merge(non_predictors, data_inflation, by = "Year")

#reduce time period to 2000-2022
non_predictors <- non_predictors[non_predictors$Year >= 2000,]

'
Data distribution
'
#distribution of data
ggplot(data = data_healthcare_expen, aes(x = Year, y = Annual_Healthcare_Expenditure)) +
  geom_point() +
  labs(title = "Personal Healthcare Expenditure over the years", x = "Year", y = "Personal Healthcare Expenditure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  #make title in the center 
  theme(plot.title = element_text(hjust = 0.5))

#distribution of maternal mortality 
ggplot(data = data_mat_mort, aes(x = Year, y = `Maternal Mortality Ratio per 100,000 live births`)) +
  geom_point() +
  labs(title = "Maternal Mortality Ratio over the years", x = "Year", y = "Maternal Mortality Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  #make title in the center 
  theme(plot.title = element_text(hjust = 0.5)) +
  #add horizontal line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey")

#distribution of abortion rate - start from 2015 to 2020
# Filter the dataset to include only data from 2015 onwards
data_abortion_filtered <- data_abortion[data_abortion$Year >= 2012, ]

# Plotting the data
ggplot(data = data_abortion_filtered, aes(x = Year, y = `No. of legal abortions reported in the U.S. from 1973 to 2021 (in 1,000s)`)) +
  geom_point() + # Adds points to represent the number of abortions
  labs(
    title = "Number of abortions over the years", # Sets the title of the plot
    x = "Year", # Labels the x-axis
    y = "Number of abortions (in 1,000s)" # Labels the y-axis
  ) +
  theme_classic() + # Applies a minimal theme to the plot for a clean look
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # Rotates x-axis labels for better readability
    plot.title = element_text(hjust = 0.5) # Centers the plot title
  ) +
  scale_y_continuous(breaks = seq(0, 3000, 100)) # Adjusts y-axis to show breaks every 100 units

  

'
Data Preprocessing
'

# Extract predictor variables
predictors <- final_data[, -c(3)]

# Standardize predictors in each group
merge_data_scaled <- as.data.frame(scale(predictors))

non_predictors_scaled <- as.data.frame(scale(non_predictors))

# Combine the standardized predictors and non-predictors
data_scaled <- cbind(merge_data_scaled, non_predictors_scaled)

target <- as.numeric(data_scaled$Annual_Healthcare_Expenditure)

summary(data_scaled)
summary(non_predictors)
summary(predictors)
'
Cross validation process
'

set.seed(123)
# Corrected predictor name
corrected_predictor_names <- c("Number_of_abortions", "Annual_GDP", "Maternal_mortality_ratio", "Annual_Gov_exp", "Inflation")

#MODEL WITH SELECTED PREDICTORS
print(corrected_predictor_names)
print(colnames(data_scaled))

# Verify which columns in corrected_predictor_names exist in data_scaled
valid_columns <- intersect(corrected_predictor_names, colnames(data_scaled))

# Print valid columns to ensure they match the expected columns
print(valid_columns)

# Print the column names in data_scaled
print("Column names in data_scaled:")
print(colnames(data_scaled))

# Subset the data frame using only valid columns
predictors1 <- as.matrix(data_scaled[, corrected_predictor_names, drop = FALSE])

# Check if the matrix is created successfully
print(dim(predictors1))

nfolds1 <- min(10, floor(nrow(data_scaled) / 3))
fit1 <- cv.glmnet(predictors1, target, alpha = 0.7, family = "gaussian", nfolds = nfolds1)
print(fit1)



coefficients1 <- coef(fit1, s = "lambda.min")
coefficients_df1 <- as.data.frame(as.matrix(coefficients1))
coefficientsrow_df1 <- rownames_to_column(coefficients_df1, var = "Predictor")
names(coefficientsrow_df1)[2] <- "Coefficient"
print(coefficientsrow_df1)
kable(coefficientsrow_df1, format = "markdown", caption = "Coefficients from CV.glmnet Model")

#MODEL WITH ALL PREDICTORS 
colnames(data_scaled) <- make.names(colnames(data_scaled))
predictors <- as.matrix(data_scaled[, !colnames(data_scaled) %in% c("Annual_Healthcare_Expenditure")])
nfolds <- min(10, floor(nrow(data_scaled) / 3))
fit <- cv.glmnet(predictors, target, alpha = 0.7, family = "gaussian", nfolds = nfolds)
print(fit)
coefficients <- coef(fit, s = "lambda.min")
coefficients_df <- as.data.frame(as.matrix(coefficients))
coefficientsrow_df <- rownames_to_column(coefficients_df, var = "Predictor")
names(coefficientsrow_df)[2] <- "Coefficient"
print(coefficientsrow_df)

kable(coefficientsrow_df, format = "markdown", caption = "Coefficients from CV.glmnet Model")
write.csv(coefficientsrow_df, "glmnet_coefficient2.csv", row.names = FALSE)
cat("The coefficients have been saved to glmnet_coefficients.csv\n")

plot(fit, xvar = "lambda", label = TRUE)

#plot graph
par(mar = c(5, 5, 5, 2) + 0.2)  # Bottom, left, top, right

# Plot the glmnet object
plot(fit)
title(main = "Model with all predictors", line = 3)

#compare fit and fit1
plot(fit1)
title(main = "Model with selected predictors", line = 3)


# Define the range of alpha values to test
alpha_values <- c(0.5, 0.7)

# Initialize a list to store the fit objects
fit_list <- list()

# Loop over each alpha value
for (alpha in alpha_values) {
  fit <- cv.glmnet(predictors, target, alpha = 0.7, family = "gaussian", nfolds = nfolds)
  fit_list[[paste0("alpha_", alpha)]] <- fit
  print(paste("Alpha:", alpha))
  print(summary(fit))
}

# Compare models based on cross-validation performance
# Example: Plotting the cross-validation curve for each alpha
par(mfrow = c(1, length(alpha_values)))
for (alpha in alpha_values) {
    plot(
      fit_list[[paste0("alpha_", alpha)]], 
      main = paste("Alpha =", alpha), 
      xlab = "Log(lambda)", 
      ylab = "Mean Squared Error", 
      ylim = c(0, 1), 
      col = "blue",
      cex.lab = 1.5,  # Increase label size
      cex.main = 1.8,  # Increase title size
      cex.axis = 1.2,  # Increase axis text size
      lwd = 2  # Increase line width
    )
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")  # Add grid lines
}



#MODEL WITH ONLY ABORTION AND MATERNAL MORTALITY
predictor_names <- c("No..of.legal.abortions.reported.in.the.U.S..from.1973.to.2021..in.1.000s.", "Maternal.Mortality.Ratio.per.100.000.live.births")
print(predictor_names)
print(colnames(data_scaled))
valid_columns2 <- intersect(predictor_names, colnames(data_scaled))
# Print valid columns to ensure they match the expected columns
print(valid_columns2)
predictors2 <- as.matrix(data_scaled[, !colnames(predictor_names) %in% c("Annual_Healthcare_Expenditure")])
nfolds2 <- min(10, floor(nrow(data_scaled) / 3))
fit2 <- cv.glmnet(predictors2, target, alpha = 0.7, family = "gaussian", nfolds = nfolds2)
print(fit2)
coefficients3 <- coef(fit, s = "lambda.min")
coefficients_df3 <- as.data.frame(as.matrix(coefficients3))
coefficientsrow_df3 <- rownames_to_column(coefficients_df3, var = "Predictor")
names(coefficientsrow_df3)[2] <- "Coefficient"
print(coefficientsrow_df3)
kable(coefficientsrow_df3, format = "markdown", caption = "Coefficients from CV.glmnet Model")
write.csv(coefficientsrow_df3, "glmnet_coefficient3.csv", row.names = FALSE)
cat("The coefficients have been saved to glmnet_coefficients.csv\n")


#use the results to predict

# Predict using the model with all predictors
predictions_all <- predict(fit, newx = predictors)
print(head(predictions_all))
#summarise the results
summary(predictions_all)
#predict using the model with selected predictors
predictions_selected <- predict(fit1, newx = predictors1)
print(predictions_selected)

#unscale the predictions
mean_predictions <- mean(final_data$Annual_Healthcare_Expenditure)
sd_predictions <- sd(final_data$Annual_Healthcare_Expenditure)
unscale_prediction_selected <- (predictions_selected) * sd_predictions + mean_predictions
print(unscale_prediction)

unscale_prediction_all <- (predictions_all) * sd_predictions + mean_predictions
print(unscale_prediction_all)

results <- data.frame(
  Original = final_data$Annual_Healthcare_Expenditure,
  Scaled_Predictions_All = predictions_all,
  Unscaled_Predictions_All = unscale_prediction_all,
  Scaled_Predictions_Selected = predictions_selected,
  Unscaled_Predictions_Selected = unscale_prediction_selected
)
print(results)

# Evaluate the model using RMSE
rmse_all <- sqrt(mean((unscale_prediction_all - final_data$Annual_Healthcare_Expenditure)^2))
print(rmse_all)
rmse_selected <- sqrt(mean((unscale_prediction_selected - final_data$Annual_Healthcare_Expenditure)^2))
print(rmse_selected)

# Evaluate the model using R-squared
r_squared_all <- 1 - sum((final_data$Annual_Healthcare_Expenditure - unscale_prediction_all)^2) / sum((final_data$Annual_Healthcare_Expenditure - mean(final_data$Annual_Healthcare_Expenditure))^2)
print(r_squared_all)
r_squared_selected <- 1 - sum((final_data$Annual_Healthcare_Expenditure - unscale_prediction_selected)^2) / sum((final_data$Annual_Healthcare_Expenditure - mean(final_data$Annual_Healthcare_Expenditure))^2)
print(r_squared_selected)

# make it into a table
results <- data.frame(
  Model = c("All Predictors", "Selected Predictors"),
  RMSE = c(rmse_all, rmse_selected),
  R_squared = c(r_squared_all, r_squared_selected)
)
results



'
Lasso Model Evaluation for comparison
'

# Assuming your data frame is named 'brexit_data_scaled'
predictors_subset <- subset(data_scaled, select=c(1:5))

# Fit best subset selection model
best_subset_model <- regsubsets(predictors_subset$Annual_Healthcare_Expenditure~ ., data = predictors_subset, method = "exhaustive")
summary(best_subset_model)

# Fit Lasso using glmnet
lasso_fit <- glmnet(as.matrix(predictors_subset), as.numeric(data_scaled$Annual_Healthcare_Expenditure), alpha = 0.5)
lasso_fit
# Plot coefficients
plot(lasso_fit, xvar = "lambda", label = TRUE)

# Display cross-validated error
lambda <- as.matrix(lasso_fit$lambda)
lambda
plot(lambda)



