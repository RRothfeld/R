#####################################################################################
# MODE CHOICE IN R - MNL ESTIMATION
#-----------------------------------------------------------------------------------#
# 07. January 2019
#	
# Raoul Rothfeld
# Based on Material from Hema Sharanya Rayaprolu
#####################################################################################

#-----------------------------------------------------------------------------------#
# Install and load packages
#-----------------------------------------------------------------------------------#

install.packages("mlogit")
library(mlogit)
install.packages("questionr")
library(questionr)
install.packages("corrplot")
library(corrplot)



#-----------------------------------------------------------------------------------#
# Read data
#-----------------------------------------------------------------------------------#

fit_data <- read.csv("modeChoiceFitData.csv")
summary(fit_data)



#-----------------------------------------------------------------------------------#
# Check for variable correlations and manioulate data format
#-----------------------------------------------------------------------------------#

my_cormtx <- cor(fit_data[,2:7]) # Computes correlation matrix
corrplot(my_cormtx, method = "number") # Plots correlation matrix

# Arrange data in long shape (convert from wide to long)
logit_data <- mlogit.data(fit_data, choice = "mode", shape = "wide", varying = 8:11, sep = ".")
summary(logit_data)

# Add binary variables for later usage
logit_data$walk <- ifelse(logit_data$alt=="walk",1,0)
logit_data$bike <- ifelse(logit_data$alt=="bike",1,0)
logit_data$auto <- ifelse(logit_data$alt=="auto",1,0)
logit_data$transit <- ifelse(logit_data$alt=="transit",1,0)
head(logit_data)


#-----------------------------------------------------------------------------------#
# Estimate multinomial logit models
#-----------------------------------------------------------------------------------#

# Model with one travel time coefficient across all modes
mnl_fit0 <- mlogit(mode ~ ttime | age+gender+income+hh_size+auto_avail+bike_avail | 0,
                   data = logit_data,
                   weights = weight)
summary(mnl_fit0)

# Model with different travel time coefficient for different modes
mnl_fit1 <- mlogit(mode ~ 0 
                   | age
                   +gender
                   +income
                   +hh_size
                   +auto_avail
                   +bike_avail 
                   | ttime,
                   data = logit_data,
                   weights = weight)
summary(mnl_fit1)

# Removing income because estimates insignificant
mnl_fit2 <- mlogit(mode ~ 0 
                   | age
                   +gender
                   +hh_size
                   +auto_avail
                   +bike_avail 
                   | ttime,
                   data = logit_data,
                   weights = weight)
summary(mnl_fit2)

# Removing walk:gender and transit:bike_avail because estimates insignificant
mnl_fit3 <- mlogit(mode ~ I(bike*gender)
                   +I(transit*gender)
                   +I(walk*bike_avail)
                   +I(bike*bike_avail) 
                   | age+hh_size+auto_avail | ttime,
                   data = logit_data,
                   weights = weight)
summary(mnl_fit3)

# Natural logarithm of transit ttime
mnl_fit4 <- mlogit(mode ~ I(bike*gender)
                   +I(transit*gender)
                   +I(walk*bike_avail)
                   +I(bike*bike_avail)
                   +I(walk*ttime)
                   +I(bike*ttime)
                   +I(auto*ttime)
                   +I(transit*log(ttime)) 
                   | age+hh_size+auto_avail | 1,
                   data = logit_data,
                   weights = weight)
summary(mnl_fit4)

# Positive estimate for auto and transit travel time on taking logarithm of transit time
# Quadratic form of transit ttime
mnl_fit5 <- mlogit(mode ~ I(bike*gender)
                   +I(transit*gender)
                   +I(walk*bike_avail)
                   +I(bike*bike_avail)
                   +I(walk*ttime)
                   +I(bike*ttime)
                   +I(auto*ttime)
                   +I(transit*(ttime)^2) 
                   | age+hh_size+auto_avail | 1,
                   data = logit_data,
                   weights = weight)
summary(mnl_fit5)

# Show coeeficients in table form
mnl_coeff <- data.frame(coefficients(mnl_fit5))
View(mnl_coeff)

# Simple accuracy check: Predicted vs. observed mode shares#
model <- apply(predict(mnl_fit5, newdata = logit_data), 2, weighted.mean, w=fit_data$weight) # Predicted shares
actual <- prop.table(wtd.table(fit_data$mode, weights = fit_data$weight)) # Observed shares

# Store and show results
compare_fits <- cbind(model, actual)
compare_fits


#-----------------------------------------------------------------------------------#
# Predit using the created MNL model
#-----------------------------------------------------------------------------------#

# Read in test data
test_data <- read.csv("modeChoiceTestData.csv")

# Transform test data into exactly the same format as original data
logit_test_data <- mlogit.data(test_data, choice = "mode", shape = "wide", varying = 8:11, sep = ".")
logit_test_data$walk <- ifelse(logit_test_data$alt=="walk",1,0)
logit_test_data$bike <- ifelse(logit_test_data$alt=="bike",1,0)
logit_test_data$auto <- ifelse(logit_test_data$alt=="auto",1,0)
logit_test_data$transit <- ifelse(logit_test_data$alt=="transit",1,0)

# Predict
predict(mnl_fit5, newdata = logit_test_data, weights = weight)

# Compare aggregated predictions
test_predicted <- apply(predict(mnl_fit5, newdata = logit_test_data), 2, weighted.mean, w=test_data$weight) # Predicted shares
compare_fits <- cbind(compare_fits, test_predicted)
test_actual <- prop.table(wtd.table(test_data$mode, weights = test_data$weight)) # Observed shares
compare_fits <- cbind(compare_fits, test_actual)

compare_fits

# Plot comparison
library(ggplot2)
library(reshape2)
melted_data <- melt(compare_fits)
names(melted_data) <- c("mode", "dataset", "value")

ggplot(melted_data, aes(mode, value)) +   
  geom_bar(aes(fill = dataset), position = "dodge", stat="identity")
