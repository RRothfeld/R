
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

# Setting up working directory/workspace
getwd()

# Help - ?function (lookup in documentation) or ??function (online)
?setwd 	# Workspace can also be set via Files viewer --> "More"

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

sample(c(0,1), size = 10, replace = TRUE, prob = c(0.5,0.5))
sample(c(0,1), size = 10, replace = TRUE, prob = c(0.1,0.9))

m <- c(4,16,64)
n <- sapply(m, FUN = sqrt)		# Executes a function (FUN) for each element of a vector/list
n

lapply(m, FUN = sqrt)
sapply(c(4,-16,64), FUN = abs)

# Matrices
?matrix
m1 <- matrix(data = 1:20, nrow = 4, ncol = 5)
m1 														# Simply calling a variable returns its value in the console

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
rm(a)
ls()
rm(list = ls()) 							# Removes all workspace objects, i.e. clears memory

# Data frame and data import
sampleData <- read.csv("sampleData.csv") # read.table if reading from a .txt file
class(sampleData)

# Hint: Use RStudio'S "Import Dataset" function if you are unsure how to import a file

# Data export
write.csv(sampleData, file = "output.csv")


#-----------------------------------------------------------------------------------#
# Exploring Data																																		#
#-----------------------------------------------------------------------------------#

View(sampleData)		# Open data viewer with sampleData

nrow(sampleData)		# Number of rows
ncol(sampleData)		# Number of columns
dim(sampleData) 		# Size of data.frame
summary(sampleData)	# Quick summary (mean, median, range, ...) of data.frame

str(sampleData) 		# data types of each column along with the first 10 entries
names(sampleData) 	# Lists column-names
head(sampleData) 		# Returns first 6 rows of the dataset (always use with large datasets)

sampleData$hh_size 	# Returns a column of a dataset (dataset$column)

table(sampleData$hh_size) 						# Frequency table
summary(sampleData$hh_size) 					# Statistical distribution
prop.table(table(sampleData$hh_size)) # Share of the total

unique(sampleData$hh_size) 						# Returns only unique values (removes dublicates)
unique(sampleData$region_type)
class(sampleData$region_type)					# Factor means categorical data, i.e. limited number of different values
str(sampleData$region_type)
prop.table(table(sampleData$region_type))

sampleData$region_type <- factor(sampleData$region_type,
																 levels = c("metropolitan","urban","rural"))	# Set the order of variables
prop.table(table(sampleData$region_type))

tapply(sampleData$hh_size, sampleData$region_typ, mean)					# Mean houehold size by region type
tapply(sampleData$trip_length_km, sampleData$region_typ, sum)		# Sum of trip length by region type

# Example: Compute average trip length by income category
summary(sampleData$hh_income)

ncol(sampleData)
sampleData$income_cat <- cut(sampleData$hh_income,                  # Add column with dataset$new-column
														 breaks = c(seq(0,5000,by=2500),10000), # What will the breaks be?
														 labels = c("low", "medium", "high")) 	# What ranges does each level cover?
ncol(sampleData)

str(sampleData$income_cat)
summary(sampleData$income_cat)
sum(is.na(sampleData$income_cat)==TRUE)															# Check if there are any N/A values

aggregate(sampleData$trip_length_km,                                # Input data/column
					by = list(sampleData$income_cat),                         # Aggregate by list of columns (here only one)
					mean)                                                     # Aggregative function
?aggregate

# For weighted analysis
distByIncome <- by(sampleData,
									 sampleData$income_cat,
									 function(x) weighted.mean(x$trip_length_km, x$weight))
distByIncome

distByIncome <- do.call(cbind, list(distByIncome))
distByIncome

colnames(distByIncome) <- c("Average Distance per Income Group [km]")
distByIncome

# Analyze by two variables
distByIncomeHHsize <- by(sampleData,
												 list(sampleData$income_cat, sampleData$hh_size),
												 function(x) weighted.mean(x$trip_length_km, x$weight))
distByIncomeHHsize <- do.call(cbind, list(distByIncomeHHsize))
distByIncomeHHsize  # Shows hh_size in columns and income categories in rows

# Extract rows/columns from data.frame via subset
?subset

# Extract rows/columns from data.frame with dataset[rows,columns]
extract <- sampleData[5,6] 					# Return 5th row, 6th column
head(extract)

extract <- sampleData[5:10,1:3] 		# Return rows 5 to 10, columns 1 to 3
head(extract)

extract <- sampleData[1:5,] 				# Return rows 1 to 5, all columns
head(extract)												# How could all rows be returned for specific columns?

extract <- sampleData[sampleData$hh_size == 2,] 		# Return all rows where condition is TRUE
head(extract)

extract <- sampleData[,c("hh_size", "n_children")]
head(extract)

extract <- sampleData[sampleData$hh_size == 2, c("hh_size", "n_children")]
head(extract)

#-----------------------------------------------------------------------------------#
# Visualizing Data																																	#
#-----------------------------------------------------------------------------------#

attach(sampleData)	# Makes sampleData available in global environment, i.e. its
 										# columns/rows can be accessed directly; use sparingly
search()

hist(hh_income,
		 main = "Frequency distribution: Household income",
		 xlab = "Montly household income (Euro)")

hist(trip_length_km,
		 main = "Frequency distribution: Distance to work",
		 xlab = "Distance from home to work location (km)")

hist(dist_to_transit,
		 main = "Frequency distribution: Distance to transit",
		 xlab = "Distance from home location to nearest transit stop (km)")

par(mfrow = c(2,2))
boxplot(dist_to_transit, horizontal = TRUE)
plot(region_type)
plot(hh_size)
plot(income_cat)

par(mfrow = c(1,1)) # Reset graph grid

# Libraries are add-on packages of code that others have pre-written for a specific purpose
# If necessary install via code:		install.packages("ggplot2")			
# or via RStudio "Tools" --> "Install Packages"
# or via Package viewer --> "Install"
library(ggplot2)	# Load ggplot2 library (widely used and powerfull visualization package)

ggplot(sampleData, aes(hh_income, trip_length_km, colour = region_type)) + 
	geom_point()

ggplot(sampleData, aes(hh_income, trip_length_km, colour = income_cat)) + 
	geom_point()

ggplot(sampleData, aes(income_cat, trip_length_km, fill = region_type)) + 
	geom_col()

ggplot(sampleData, aes(income_cat, trip_length_km, fill = region_type)) + 
	geom_col() +
	scale_fill_brewer(palette = "Reds") +
	labs(title = "BIG TITLE", x = "x axis", y = "y axis")

ggplot(sampleData, aes(income_cat, trip_length_km)) + 
	geom_col() +
	facet_grid(. ~ region_type)

ggplot(sampleData, aes(income_cat, trip_length_km, fill = region_type)) + 
	geom_boxplot()


#-----------------------------------------------------------------------------------#
# Regressions																																				#
#-----------------------------------------------------------------------------------#

# Regression 
# HH autos = fn(HH size, HH income, Number of workers, Number of children, Distance to transit, Region type)

# Check correlation between independent variables
cor1 <- cor(subset(sampleData,
									 select = c("hh_size", "hh_income", "n_emp", "n_children", "dist_to_transit", "region_type")))

head(subset(sampleData,
						select = c("hh_size", "hh_income", "n_emp", "n_children", "dist_to_transit", "region_type")))

levels(sampleData$region_type)

sampleData$region_type <- as.numeric(sampleData$region_type)
unique(sampleData$region_type)

cor1 <- cor(subset(sampleData,
									 select = c("hh_size", "hh_income", "n_emp", "n_children", "dist_to_transit", "region_type")))

# If necessary: install.packages("corrplot")
library(corrplot)
corrplot(cor1, method = "number")	# Number of children and household size are fairly correlated

# Multiple Linear Regression
regressionFit1 <- lm(hh_autos~hh_size+hh_income+n_emp+dist_to_transit+region_type,
										 data = sampleData,
										 weights = sampleData$weight)
summary(regressionFit1)

# n_children instead of hh_size
regressionFit2 <- lm(hh_autos~n_children+hh_income+n_emp+dist_to_transit+region_type,
										 data = sampleData,
										 weights = sampleData$weight)
summary(regressionFit2)

# Transform variables
regressionFit3 <- lm(hh_autos~hh_size+log(hh_income)+n_emp+dist_to_transit+region_type,
										 data = sampleData,
										 weights = sampleData$weight)
summary(regressionFit3)

regressionFit4 <- lm(hh_autos~hh_size+log(hh_income)+n_emp+I(dist_to_transit^2)+region_type,
										 data = sampleData,
										 weights = sampleData$weight)
summary(regressionFit4)
?I

# Interaction term
regressionFit5 <- lm(hh_autos~hh_size+hh_income+n_emp+dist_to_transit*region_type, # a*b --> a+b+a:b
										 data = sampleData,
										 weights = sampleData$weight)
summary(regressionFit5)

regressionFit6 <- lm(hh_autos~hh_size+hh_income+n_emp+dist_to_transit:region_type, # a:b --> (only) a:b
										 data = sampleData,
										 weights = sampleData$weight)
summary(regressionFit6)

# Save predictions of a model in a new data frame together with variables you want to plot against
predicted_df <- data.frame(hh_autos_pred = predict(regressionFit6, sampleData),
													 hh_size=sampleData$hh_size,
													 hh_income=sampleData$hh_income,
													 n_emp=sampleData$n_emp)
head(predicted_df)
