# Load the necessary library and dataset
install.packages("car") 
library(car)            
data(Prestige)           
# Check the structure of the dataset
str(Prestige)

# question1
#a
# Create a new variable
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0
                                
# Check the first few rows to confirm
head(Prestige)
                                
#b
# Fit the linear model
prestige_model <- lm(prestige ~ income * professional, data = Prestige)
                                
# View the summary of the model
summary(prestige_model)

