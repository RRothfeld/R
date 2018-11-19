
#####################################################################################
# INTRODUCTION INTO R - DATA ANALYSIS																								#
#-----------------------------------------------------------------------------------#
# 19. November 2018																																	#
#																																										#
# Raoul Rothfeld																																		#
# Based on Material from Hema Sharanya Rayaprolu																		#
#####################################################################################

#-----------------------------------------------------------------------------------#
# Basic Operations																																	#
#-----------------------------------------------------------------------------------#

# Setting up workspace
getwd()

# Help - ?function (lookup in documentation) or ??function (online)
?setwd

# Operations, variables, and Vectors (atomic vectors, lists)
4+6
a <- 5+7											# Store results in variables
b <- 1:20
c <- seq(0, 20,  by = 2) 			# Returns sequence of numbers from 0 to 20 incremented by 2
d <- seq(0, 10, length = 5) 	# Returns 5 equidistant numbers between 0 and 10
e <- rep(1, times = 5)				# Five times repetition of number 1

f <- c(1,5,6,8.5)  						# Returns an atomic (homogenous) vector, despite input of
															# integer and double numbers; c is short for concatenate
															# or combine

c(1,5,"6",8.5)								# What kind of atomic vector will be returned?

g <- rep(f, times = 2)				# Access values via variables
h <- rep(f, each = 2) 				# Repeats each value twice

i <- c("one","two","three")
str(i)												# Returns the structure of a variable/object
j <- list(TRUE, FALSE, TRUE)
str(j)

# Sampling and list-apply
?sample
k <- sample(1:20, size = 4, replace = FALSE)
l <- sample(c(0,1), size = 10, replace = TRUE, prob = c(0.1,0.9))

m <- c(4,16,64)
n <- sapply(m, FUN = sqrt)		# Executes a function (FUN) for each element of a vector/list
lapply(m, FUN = sqrt)
sapply(c(4,-16,64), FUN = abs)

# Matrices
?matrix
m1 <- matrix(data = 1:20, nrow = 4, ncol = 5)
m1

m2 <- rep(1, times=4)
m2

m3 <- cbind(m1, m2) 					# Column-bind
m3

colnames(m3) <- c("one","two","three","four","five","six")
m3

m4 <- matrix(data = 21:40, nrow = 4, ncol = 5)
m4

m5 <- rbind(m1,m4) 						# Row-bind
m5

# Environment commands
ls() 													# A list of all the objects in the workspace
rm(m5)
ls()
rm(list = ls()) 							# removes all workspace objects - clears memory

# Data frame and data import
sampleData <- read.csv("sampleData.csv") # read.table if reading from a .txt file
class(sampleData)

# Hint: Use RStudio'S "Import Dataset" function if you are unsure how to import a file

# Data export
write.csv(sampleData, file = "output.csv")


#-----------------------------------------------------------------------------------#
# Exploring Data																																		#
#-----------------------------------------------------------------------------------#

dim(sampleData) # nrow ncol
summary(sampleData)
names(sampleData) # a list of colnames
# TODO rename
head(sampleData) # first 6 rows of the dataset
str(sampleData) # data types of each column along with the first 10 entries

sampleData$hh_size

table(sampleData$hh_size) # frequency
summary(sampleData$hh_size) # statistical distribution
prop.table(table(sampleData$hh_size)) # share of the total

unique(sampleData$hh_size) # return unique values
unique(sampleData$region_type)
class(sampleData$region_type)
sampleData$region_type <- factor(sampleData$region_type, levels = c("metropolitan","urban","rural"))  # set the order of variables
str(sampleData$region_type)
table(sampleData$region_type)

tapply(sampleData$hh_size, sampleData$region_typ, mean) # mean houehold size by region type

# Example: Compute average trip length by income

summary(sampleData$hh_income)
# Let's add a new column with income categories
sampleData$income_cat <- cut(sampleData$hh_income, breaks = c(seq(0,7000,by=1000),10000), labels = 1:8) # create income categories
str(sampleData$income_cat)
summary(sampleData)
sum(is.na(sampleData$income_cat)==TRUE) # check if there are any N/A variables in your dataset

aggregate(sampleData$trip_length_km, by = list(sampleData$income_cat), mean)
?aggregate
# For weighted analysis
distByIncome <- by(sampleData, sampleData$income_cat, function(x) weighted.mean(x$trip_length_km, x$weight))
distByIncome
distByIncome <- do.call(cbind, list(distByIncome))
#distByIncome <- cbind(distByIncome)  # make it more readable
distByIncome
# Analyze by two variables
distByIncomeHHsize <- by(sampleData, list(sampleData$income_cat, sampleData$hh_size), function(x) weighted.mean(x$trip_length_km, x$weight))
distByIncomeHHsize <- do.call(cbind, list(distByIncomeHHsize))
distByIncomeHHsize  # shows hh size in columns and income categories in row

# TODO DATA:TABLE + ADD REFERENCE

####### --------------- VISUALIZING DATA --------------------- #######

# install.packages("questionr")
# library(questionr)

attach(sampleData)
#search()
hist(hh_income, main = "Frequency distribution: Household income", xlab = "Montly household income (Euro)")
hist(trip_length_km, main = "Frequency distribution: Distance to work", xlab = "Distance from home to work location (km)")
hist(dist_to_transit, main = "Frequency distribution: Distance to transit", xlab = "Distance from home location to nearest transit stop (km)")
par(mfrow = c(2,2))
boxplot(dist_to_transit, horizontal = TRUE, main = "Boxplot: Distance to transit", xlab = "Distance from home location to nearest transit stop (km)")
summary(dist_to_transit)
plot(region_type)
plot(hh_size)
# TODO
plot(trip_length_km,
		 type = "l",
		 xlab = "Income category",
		 lab = "Average trip length (km)",
		 ain = "Trip length distribution by income")



install.packages("ggplot2")
library(ggplot2)

# Trip distribution by income across region types
incomeByRegion <- data.frame(wtd.table(sampleData$income_cat, sampleData$region_type, weights = sampleData$weight))
names(incomeByRegion) <- c("incomeCat", "regionType", "Trips")
str(incomeByRegion)
levels(incomeByRegion$regionType)
incomeByRegion$regionType <- factor(incomeByRegion$regionType, levels = c("metropolitan", "urban", "rural"))
ggplot() + geom_area(aes(y = Trips, x = incomeCat, group = regionType, fill = regionType), data = incomeByRegion) + ggtile("Trip Distribution By Income Category")


####### --------------- MODELING DATA --------------------- #######

# Regression 
# HH autos = fn(HH size, HH income, Number of workers, Number of children, Distance to transit, Region type)

# Check correlation between independent variables

cor1 <- cor(subset(sampleData, select = c("hh_size", "hh_income", "n_emp", "n_children", "dist_to_transit", "region_type")))
levels(sampleData$region_type)
sampleData$region_type <- factor(sampleData$region_type, levels = c("metropolitan", "urban", "rural"))
sampleData$region_type <- as.numeric(sampleData$region_type)
cor1 <- cor(subset(sampleData, select = c("hh_size", "hh_income", "n_emp", "n_children", "dist_to_transit", "region_type")))

# Visualizing correlation

#install.packages("corrplot")
install.packages("corrplot")
library(corrplot)
corrplot(cor1, method = "number") # number of children and household size are fairly correlated

# Multiple Linear Regression
regressionFit1 <- lm(hh_autos~hh_size+hh_income+n_emp+dist_to_transit+region_type, data = sampleData, weights = sampleData$weight)
summary(regressionFit1)
# n_children instead of hh_size
regressionFit2 <- lm(hh_autos~n_children+hh_income+n_emp+dist_to_transit+region_type, data = sampleData, weights = sampleData$weight)
summary(regressionFit2)
# Transform variables
regressionFit3 <- lm(hh_autos~hh_size+log(hh_income)+n_emp+dist_to_transit+region_type, data = sampleData, weights = sampleData$weight)
summary(regressionFit3)
regressionFit4 <- lm(hh_autos~hh_size+log(hh_income)+n_emp+I(dist_to_transit^2)+region_type, data = sampleData, weights = sampleData$weight)
summary(regressionFit4)
# Interaction term
regressionFit5 <- lm(hh_autos~hh_size+hh_income+n_emp+dist_to_transit*region_type, data = sampleData, weights = sampleData$weight)
summary(regressionFit5)
regressionFit6 <- lm(hh_autos~hh_size+hh_income+n_emp+dist_to_transit:region_type, data = sampleData, weights = sampleData$weight)
summary(regressionFit6)

# TODO
# save predictions of the model in the new data frame together with variable you want to plot against
predicted_df <- data.frame(hh_autos_pred = predict(regressionFit6, sampleData), hh_size=sampleData$hh_size)

