
######################################################################################################
############# ---------------- INTRODUCTION TO DATA ANALYSIS WITH R ----------------- ################
######################################################################################################


####### ----------------- IMPORTING DATA INTO R ---------------------- ########


# Setting up workspace
getwd()
setwd("/Users/rm/Documents/teaching/2017WS/travelBehavior/exercise/hema_dataAnalysis")

# Help - ?function
?setwd

# Vectors (atomic vectors, lists)
4+6
a <- 5+7
b <- 1:20
c <- seq(0 ,20, by = 2) # Gives a sequence of numbers from 0 to 20 incremented by 2
d <- seq(0, 10, length = 30) # Gives 30 equidistant numbers between 0 and 10
e <- rep(1,times = 5)
f <- c(1,5,6,8.5,1,4)  # c is short for concatenate; integers are converted to real numbers if one of the values is real (makes it an "atomic vector")
g <- rep(f, times=2)
h <- rep(c(0,1,2), each=2)  # repeats each value twice
i <- c("one","two","three")
j <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
str(j)  # get the data structure of j
k <- sample(1:20, size = 4, replace = FALSE)
?sample
l <- sample(c(0,1), size = 10, replace = TRUE, prob = c(0.3,0.7))
m <- sapply(c(4,16,64), FUN = sqrt, simplify = TRUE) # try with lapply (list apply, remove simplify=TRUE) and vapply (data type is specified by user)
sapply(c(4,16,64), FUN = sqrt)
class(sapply(c(4,16,64), FUN = sqrt))
lapply(c(4,16,64), FUN = sqrt)
class(lapply(c(4,16,64), FUN = sqrt))

# Matrices
?matrix
m1 <- matrix(data = 1:20, nrow = 4, ncol = 5)
m2 <- rep(1, times=4)
m3 <- cbind(m1, m2) # column-bind
colnames(m3) <- c("one","two","three","four","five","six")
m3

m4 <- matrix(data = 1:20, nrow = 4, ncol = 5)
m5 <- rbind(m1,m4) # row-bind


ls() # A list of all the objects in the workspace
rm(m5)
ls()
rm(list = ls()) # removes all workspace objects - clears memory

# Data frame
# TODO IMPORT
sampleData <- read.csv("DataSample_Introduction.csv") # read.table if reading from a .txt file
class(sampleData)


####### ----------------- EXPLORING DATA ---------------------- ########
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
plot(trip_length_km)


install.packages("ggplot2")
library(ggplot2)
# REPLACE WITH GGPLOT2
# Trip length distribution by income
# par(mfrow = c(1,1))
# plot(distByIncome[,1], type = "l",  lwd = 2, xlab = "Income category", ylab = "Average trip length (km)", main = "Trip length distribution by income")
# plot(distByIncome[,1], type = "l",  lwd = 2, xlab = "Income category", ylab = "Average trip length (km)", main = "Trip length distribution by income", ylim = c(0,16))
# ?plot
# ?par  # shows parameters that can be used with plot() function
# tripsByIncome <- aggregate(sampleData$weight, by = list(sampleData$income_cat), sum)
# par(new = TRUE) # to draw a second plot over the previous plot
# plot(tripsByIncome$x, type = "l", lwd = 2, axes = F, xlab = NA, ylab = NA, col = "orange")
# axis(side = 4)  # side 1: bottom, 2: left, 3: top and 4: right
# mtext(side = 4, line = 2.5, text = "Total number of trips")
# par(xpd = TRUE)
# legend(x=6, y=-250, legend = c("Average trip length (km)", "Total number of trips"), lty = c(1,1) , lwd = 2, col = c("black", "orange"), bty = "n")

# TODO GGPLOT2 + ADD REFERENCE

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

# WITHOUT WEIGHT?!
regressionFit1 <- lm(hh_autos~hh_size+hh_income+n_emp+dist_to_transit+region_type, data = sampleData)
summary(regressionFit1)

# TODO
# save predictions of the model in the new data frame 
# together with variable you want to plot against
predicted_df <- data.frame(hh_autos_pred = predict(regressionFit1, sampleData), hh_size=sampleData$hh_size)

# TODO
# this is the predicted line of multiple linear regression
p <- ggplot(data = sampleData, aes(x = hh_size, y = hh_autos)) + 
	geom_point(color='blue')
p
p + geom_line(color='red',data = predicted_df, aes(x=hh_autos_pred, y=hh_size))

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

plot(regressionFit1)

