#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
df <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv", header = TRUE)

library(ggplot2)

# Running regression analysis
model <- lm(voteshare ~ difflog, data = df)
summary(model)  # Output regression analysis results

# Scatterplot with regression line
ggplot(df, aes(x = difflog, y = voteshare)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Regression Analysis of Vote Share on Difflog",
       x = "Difference in Log of Spending (difflog)", 
       y = "Vote Share (voteshare)")  # Draw regression lines and scatter plots

# Save and inspect residuals
residuals <- residuals(model)
head(residuals)

# Plot residuals
plot(residuals)

# Extract coefficients
intercept <- coef(model)[1]  # Extracted intercept
slope <- coef(model)[2]  # Extracted slope

# Prediction equation
prediction_equation <- paste("voteshare =", as.character(intercept), " + ", as.character(slope), "* difflog")
print(prediction_equation)

#question 2
# 1. Run the regression analysis
model_presvote <- lm(presvote ~ difflog, data = df)
summary(model_presvote)  # Output regression analysis results

# 2. Create a scatter plot and add the regression line
library(ggplot2)
ggplot(df, aes(x = difflog, y = presvote)) +
  geom_point() +  # Create scatter plot
  geom_smooth(method = "lm", color = "blue") +  # Add regression line
  labs(title = "Regression Analysis of Presvote on Difflog",
       x = "Difference in Log of Spending (difflog)", 
       y = "Presidential Vote Share (presvote)")  # Set title and axis labels

# 3. Save the residuals and create a residual plot
residuals_presvote <- residuals(model_presvote)
head(residuals_presvote)  # View the first few residuals

# Create residual plot
plot(residuals_presvote, main = "Residuals of Regression Model", 
     xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal reference line

# 4. Write the regression equation
intercept_presvote <- coef(model_presvote)[1]  # Extract intercept
slope_presvote <- coef(model_presvote)[2]  # Extract slope
prediction_equation_presvote <- paste("presvote =", round(intercept_presvote, 3), 
                                      " + ", round(slope_presvote, 3), "* difflog")
print(prediction_equation_presvote)

# question 3
# 1. Run the regression analysis
model_voteshare <- lm(voteshare ~ presvote, data = df)
summary(model_voteshare)  # Output regression analysis results

# 2. Create a scatter plot and add the regression line
library(ggplot2)
ggplot(df, aes(x = presvote, y = voteshare)) +
  geom_point() +  # Create scatter plot
  geom_smooth(method = "lm", color = "blue") +  # Add regression line
  labs(title = "Regression Analysis of Voteshare on Presvote",
       x = "Presidential Vote Share (presvote)", 
       y = "Incumbent's Vote Share (voteshare)")  # Set title and axis labels

# 3. Save residuals and create a residual plot (optional step for diagnostics)
residuals_voteshare <- residuals(model_voteshare)
head(residuals_voteshare)  # View the first few residuals
plot(residuals_voteshare, main = "Residuals of Regression Model", 
     xlab = "Index", ylab = "Residuals")
abline(h = 0, col = "red")  # Add a horizontal reference line

# 4. Write the regression equation
intercept_voteshare <- coef(model_voteshare)[1]  # Extract intercept
slope_voteshare <- coef(model_voteshare)[2]  # Extract slope
prediction_equation_voteshare <- paste("voteshare =", round(intercept_voteshare, 3), 
                                       " + ", round(slope_voteshare, 3), "* presvote")
print(prediction_equation_voteshare)

#question 4
# Assuming the residuals from Question 1 are saved in residuals_voteshare
# and the residuals from Question 2 are saved in residuals_presvote

# 1. Run a regression where the outcome variable is the residuals from Question 1 
# and the explanatory variable is the residuals from Question 2
model_residuals <- lm(residuals_voteshare ~ residuals_presvote)
summary(model_residuals)  # Output regression analysis results

# 2. Create a scatter plot and add the regression line
library(ggplot2)
ggplot(data.frame(residuals_voteshare, residuals_presvote), aes(x = residuals_presvote, y = residuals_voteshare)) +
  geom_point() +  # Create scatter plot
  geom_smooth(method = "lm", color = "blue") +  # Add regression line
  labs(title = "Regression Analysis of Residuals from Voteshare on Residuals from Presvote",
       x = "Residuals from Presvote (residuals_presvote)", 
       y = "Residuals from Voteshare (residuals_voteshare)")  # Set title and axis labels

# 3. Write the regression equation
intercept_residuals <- coef(model_residuals)[1]  # Extract intercept
slope_residuals <- coef(model_residuals)[2]  # Extract slope
prediction_equation_residuals <- paste("residuals_voteshare =", round(intercept_residuals, 3), 
                                       " + ", round(slope_residuals, 3), "* residuals_presvote")
print(prediction_equation_residuals)

#question 5

# 1. Run the regression where the outcome variable is voteshare and the explanatory variables are difflog and presvote
model_voteshare <- lm(voteshare ~ difflog + presvote, data = df)
summary(model_voteshare)  # Output regression analysis results

# 2. Write the prediction equation
intercept_voteshare <- coef(model_voteshare)[1]  # Extract intercept
slope_difflog <- coef(model_voteshare)[2]  # Extract slope for difflog
slope_presvote <- coef(model_voteshare)[3]  # Extract slope for presvote
prediction_equation_voteshare <- paste("voteshare =", round(intercept_voteshare, 3), 
                                       " + ", round(slope_difflog, 3), "* difflog", 
                                       " + ", round(slope_presvote, 3), "* presvote")
print(prediction_equation_voteshare)

# 3. Analyze the similarities with the output in Question 4



