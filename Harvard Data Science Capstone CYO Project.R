
################################################################################
##### Part 1 - Loading R Packages & UCI Concrete Compressive Strength Data #####
################################################################################

# Install the required packages from CRAN
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(gdata)) install.packages("gdata", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(DMwr)) install.packages("DMwR", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")
if(!require(NeuralNetTools)) install.packages("NeuralNetTools", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(lmtest)) install.packages("lmtest", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(trafo)) install.packages("trafo", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(fpc)) install.packages("fpc", repos = "http://cran.us.r-project.org")
if(!require(ggstatsplot)) install.packages("ggstatsplot", repos = "http://cran.us.r-project.org")
if(!require(rcompanion)) install.packages("rcompanion", repos = "http://cran.us.r-project.org")
if(!require(nortest)) install.packages("nortest", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(BBmisc)) install.packages("BBmisc", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(gmodels)) install.packages("gmodels", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if(!require(stats)) install.packages("stats", repos = "http://cran.us.r-project.org")

# Load required libraries
library('RCurl')
library('gdata')
library('readxl')
library('httr')
library('GGally')
library('tidyverse')
library('ggplot2')
library('psych')
library('DMwR')
library('lmtest')
library('caret')
library('data.table')
library('neuralnet')
library('NeuralNetTools')
library('rpart')
library('dplyr')
library('randomForest')
library('e1071')
library('rattle')
library('trafo')
library('MASS')
library('cluster')
library('fpc')
library('ggstatsplot')
library('rcompanion')
library('nortest')
library('naivebayes')
library('BBmisc')
library('class')
library('rpart')
library('rattle')
library('factoextra')
library('gmodels')
library('RColorBrewer')
library('ROCR')
library('Metrics')
library('stats')

# Load the Concrete compressive Stregth Dataset from the UCI Machine Learning Repository
ConDat <- read_excel("Data/Concrete_Data.xls")

#####################################################
##### Part 2 - Data Inspection & Pre-processing #####
#####################################################

# Inspect the data.
str(ConDat)
head(ConDat)
summary(ConDat)
# The dataset consists of 1030 obs. of  9 variables
# mean compressive strength is 35.8MPa(Range 2.3-82.6MPa)
# This seems reasonable as typical concrete compressive strength values
# for most normal applications are in the range 10-60MPa.

# count NAs
sapply(ConDat, function(x){sum(is.na(x))})
# There are no na values

# count blank entries
sapply(ConDat, function(x){sum(x=='', na.rm=T)})
# There are no blank entries
# No requirement to deal with NA's or missing values.

# Rename the attributes to simplify & Recheck structure of the data
names(ConDat) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Compressive_Strength")
summary(ConDat)
str(ConDat)
head(ConDat)

##############################################
##### Part 3 - Exploratory Data Analysis #####
##############################################

# Use pairs function to provide visualization to check for correlations
pairs(ConDat)

# Use pairs.panels function to provide visualization to check for correlations
pairs.panels(ConDat)

# Use GGally ggcorr function to visualize correlations
ggcorr(ConDat, label = TRUE, hjust = 0.70, size = 3, label_size = 5)

# plot data to view relationships and calculate correlation
# Plot relationship between compressive strength and qty cement
plot(Compressive_Strength ~ Cement, data = ConDat, col = "blue")
abline(lm(Compressive_Strength ~ Cement, data = ConDat), col="red")
# moderate positive correlation observed

# Calculate correlation between compressive strength and qty cement
cor(ConDat$Compressive_Strength, ConDat$Cement)
# moderate positive correlation of 0.5

# Plot relationship between compressive strength and qty blast furnace slag
plot(Compressive_Strength ~ Blast_Furnace_Slag, data = ConDat, col = "blue")
abline(lm(Compressive_Strength ~ Blast_Furnace_Slag, data = ConDat), col="red")
# None to very weak positive relationship observed

#Calculate correlation between compressive strength and qty blast furnace slag
cor(ConDat$Compressive_Strength, ConDat$Blast_Furnace_Slag)
# Very weak positive correlation of 0.13

# Plot relationship between compressive strength and qty fly ash
plot(Compressive_Strength ~ Fly_Ash, data = ConDat, col = "blue")
abline(lm(Compressive_Strength ~ Fly_Ash, data = ConDat), col="red")
# None to very weak negative relationship observed

# Calculate correlation between compressive strength and qty fly ash
cor(ConDat$Compressive_Strength, ConDat$Fly_Ash)
# Very weak negative correlation of -0.11

# Plot relationship between compressive strength and qty water
plot(Compressive_Strength ~ Water, data = ConDat, col = "blue")
abline(lm(Compressive_Strength ~ Water, data = ConDat), col="red")
# Weak negative relationship observed

# Calculate correlation between compressive strength and qty water
cor(ConDat$Compressive_Strength, ConDat$Water)
# Weak negative correlation of -0.29 

# Plot relationship between compressive strength and qty superplasticizer
plot(Compressive_Strength ~ Superplasticizer, data = ConDat, col = "blue")
abline(lm(Compressive_Strength ~ Superplasticizer, data = ConDat), col="red")
# Weak positive relationship observed

# Calculate correlation between compressive strength and qty superplasticizer
cor(ConDat$Compressive_Strength, ConDat$Superplasticizer)
# Weak positive correlation of 0.37

# Plot relationship between compressive strength and qty coarse aggregate
plot(Compressive_Strength ~ Coarse_Aggregate, data = ConDat, col = "blue")
abline(lm(Compressive_Strength ~ Coarse_Aggregate, data = ConDat), col="red")
#None to very weak negative relationship observed

# Calculate correlation between compressive strength and qty coarse aggregate
cor(ConDat$Compressive_Strength, ConDat$Coarse_Aggregate)
#Very weak negative correlation of -0.16

# Plot relationship between compressive strength and qty fine aggregate
plot(Compressive_Strength ~ Fine_Aggregate, data = ConDat, col = "blue")
abline(lm(Compressive_Strength ~ Fine_Aggregate, data = ConDat), col="red")
#None to very weak negative relationship observed

# Calculate correlation between compressive strength and qty fine aggegrate
cor(ConDat$Compressive_Strength, ConDat$Fine_Aggregate)
# Very weak negative correlation of -0.17

# Plot relationship between compressive strength and age
plot(Compressive_Strength ~ Age, data = ConDat, col = "blue")
abline(lm(Compressive_Strength ~ Age, data = ConDat), col = "red")
# Weak positive relationship oberved

# Calculate correlation between compressive strength and age
cor(ConDat$Compressive_Strength, ConDat$Age)
# Weak positive correlation of 0.33


# Based on this initial evaluation, it appears that that the most significant positive
# correlations were achieved for Cement (0.4978327), Superplasticizer (0.3661023) and
# Age (0.32887).  The most significant negative correlation is for Water (-0.2896135).
# None of the correlations are very strong which (in conjunction with the scatterplots)
# may indicate that the relationships between Compressive_Strength and the other attributes
# in the dataset may be non-linear and may require further transformation to develop a more
# accurate linear regression model.


####################################################################
########## Part 3 - Multiple Linear Regression Model ###############
####################################################################

# Load the Concrete compressive Stregth Dataset from the UCI Machine Learning Repository
ConDat <- read_excel("Data/Concrete_Data.xls")

# Rename the attributes to simplify & Recheck structure of the data
names(ConDat) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Compressive_Strength")

# Split ConDat data into training (80%) and validation (20%) sets
set.seed(123)
TrainConDat <- sample(nrow(ConDat),0.8*nrow(ConDat), replace=FALSE)
TrainSet <- ConDat[TrainConDat,]
ValidSet <- ConDat[-TrainConDat,]

#Build model with all parameters included
csmodel1 <- lm(Compressive_Strength~., data=TrainSet)

# evaluate csmodel1
summary(csmodel1)

# Adj  R-squared of 0.6205 so model explains about 62% of the variability

# Plot & check correlation between response in ValidSet and response of csmodel1 built
# using TrainSet
plot(ValidSet$Compressive_Strength, predict(csmodel1, ValidSet),
     xlim =c(0,85), ylim=c(0,85), xlab= "actual", ylab = "predicted",
     main = "Compressive Strength - csmodel1")
abline(lm(ValidSet$Compressive_Strength ~ predict(csmodel1, ValidSet)), col = "red")
cor(ValidSet$Compressive_Strength, predict(csmodel1, ValidSet))
# Correlation is about 76% so model needs more work to improve accuracy

# Check for "LINE" Requirements for a linear regression model
# L - Linearity (Some concern over this based on scatter plots above)
# I - Independence
# N - Normality
# E - Equal Variances


# plot residuals
plot(csmodel1)
layout(matrix(1:2,2,2)) 
# residual plot looks reasonably randomly distributed around the zero line
# however some concerns that there may be "funnelling" or "bowing" indicating
# that data is heteroscedastic and that data transformation may be required for modelling


# Perfrom Durbin-Watson test to check for Independence
dwtest(csmodel1)
# If there is no autocorrelation the Durbin-Watson statistic should be between 1.5 and 2.5
# and the p-value will be above 0.05.
# With a DW value of 2.0669 and p value of 0.8322 there is no evidence of autocorrelation 

# Plot histogram and normal Q-Q plot to confirm residuals are normally distributed
# Plot histogram
hist(residuals(csmodel1), col="gray")

# Plot Q-Q plot
qqnorm(residuals(csmodel1))
# Data looks to be reasonably normally distributed


# Complete Shapiro-wilk test to confirm residuals are normally distributed
shapiro.test(csmodel1$residuals)
# Data looks not to be normally distributed (P value <0.05)

# Complete Anderson_Darling test to confirm residuala are normal distributed
ad.test(csmodel1$residuals)
# Data looks not to be normally distributed (P value <0.05)

# Calculate confidence and prediction intervals for csmodel1
# (show first 20 rows only)
# Confidence Interval
confcs1 <- predict(csmodel1,ValidSet,interval="confidence")
head(confcs1, 20)
# Prediction Interval
predcs1 <- predict(csmodel1,ValidSet,interval="predict")
head(predcs1, 20)

# Build Stepwise Compressive Strength Forwards, Backwards and Stepwise
# Backward Elimination
csmodel2 <- step(csmodel1, direction="backward")
csmodel2
summary(csmodel2)

# Forward Selection
csmodel3 <- step(csmodel1, direction="forward")
csmodel3
summary(csmodel3)

# Stepwise Selection
csmodel4 <- step(csmodel1, direction="both")
csmodel4
summary(csmodel4)

# Stepwise Compressive Strength Models Forwards, Backwards and Stepwise
# do not result in improved Adjusted R-squared value

# Build model eliminating least significant parameter (coarse aggregate)
csmodel5 <- lm(Compressive_Strength~ Cement + Blast_Furnace_Slag + Fly_Ash + Water + Superplasticizer + Fine_Aggregate + Age, data=TrainSet)
csmodel5
summary(csmodel5)

# This did not improve the Adjusted R-squared value but did simplify the model

# Build model eliminating two least significant parameter (coarse aggregate & fine aggregate)
csmodel6 <- lm(Compressive_Strength~ Cement + Blast_Furnace_Slag + Fly_Ash + Water + Superplasticizer + Age, data=TrainSet)
csmodel6
summary(csmodel6)

# Again this did not improve the Adjusted R-squared value but did further simplify the model

# Check the simplified for "LINE" requirements for linear regression
# L - Linearity
# I - Independence 
# N - Normality
# E - Equal Variances

# plot residuals
plot(csmodel6)
# residual plot looks reasonably randomly distributed around the zero line
# however some concerns that there may be "funnelling" or "bowing" indicating
# that data is heteroscedastic and that transformation may be required


# Perfrom Durbin-Watson test to check for Independence
dwtest(csmodel6)
# If there is no autocorrelation the Durbin-Watson statistic should be between 1.5 and 2.5
# and the p-value will be above 0.05.
# With a DW value of 2.0612 and p value of 0.8106 there is no evidence of autocorrelation 


# Plot histogram and normal Q-Q plot for csmodel6 to confirm residuals are normally distributed
# Plot histogram
hist(residuals(csmodel6), col="gray")

# Plot Q-Q Plot
qqnorm(residuals(csmodel6))

# Complete Shapiro-wilk test to confirm residuals are normally distributed
shapiro.test(csmodel6$residuals)
# Data looks not to be normally distributed (P value <0.05)

# Complete Anderson_Darling test to confirm residuala are normal distributed
ad.test(csmodel6$residuals)
# Data looks not to be normally distributed (P value <0.05)


# Try to see if correlation can be improved by using Transformations on data to improve linearity
# Use square root, squared, log and exp functions to see if there is any increase in correlation

# Compressive strength and qty cement transformations
cor(ConDat$Compressive_Strength, ConDat$Cement)
cor(ConDat$Compressive_Strength, sqrt(ConDat$Cement))
cor(ConDat$Compressive_Strength, (ConDat$Cement^2))
cor(ConDat$Compressive_Strength, log(ConDat$Cement))
cor(ConDat$Compressive_Strength, exp(ConDat$Cement))
# No correlation improvement from these transformations

# Compressive strength and qty blast furnace slag
cor(ConDat$Compressive_Strength, ConDat$Blast_Furnace_Slag)
cor(ConDat$Compressive_Strength, sqrt(ConDat$Blast_Furnace_Slag))
cor(ConDat$Compressive_Strength, (ConDat$Blast_Furnace_Slag^2))
cor(ConDat$Compressive_Strength, log(ConDat$Blast_Furnace_Slag))
cor(ConDat$Compressive_Strength, exp(ConDat$Blast_Furnace_Slag))
# No correlation improvement from these transformations

# Compressive strength and qty fly ash
cor(ConDat$Compressive_Strength, ConDat$Fly_Ash)
cor(ConDat$Compressive_Strength, sqrt(ConDat$Fly_Ash))
cor(ConDat$Compressive_Strength, (ConDat$Fly_Ash^2))
cor(ConDat$Compressive_Strength, log(ConDat$Fly_Ash))
cor(ConDat$Compressive_Strength, exp(ConDat$Fly_Ash))
# No correlation improvement from these transformations

# Compressive strength and qty water
cor(ConDat$Compressive_Strength, ConDat$Water)
cor(ConDat$Compressive_Strength, sqrt(ConDat$Water))
cor(ConDat$Compressive_Strength, (ConDat$Water^2))
cor(ConDat$Compressive_Strength, log(ConDat$Water))
cor(ConDat$Compressive_Strength, exp(ConDat$Water))
# No correlation improvement from these transformations

# Compressive strength and qty superplasticizer
cor(ConDat$Compressive_Strength, ConDat$Superplasticizer)
cor(ConDat$Compressive_Strength, sqrt(ConDat$Superplasticizer))
cor(ConDat$Compressive_Strength, (ConDat$Superplasticizer^2))
cor(ConDat$Compressive_Strength, log(ConDat$Superplasticizer))
cor(ConDat$Compressive_Strength, exp(ConDat$Superplasticizer))
# No correlation improvement from these transformations

# Compressive strength and qty coarse aggregate
cor(ConDat$Compressive_Strength, ConDat$Coarse_Aggregate)
cor(ConDat$Compressive_Strength, sqrt(ConDat$Coarse_Aggregate))
cor(ConDat$Compressive_Strength, (ConDat$Coarse_Aggregate^2))
cor(ConDat$Compressive_Strength, log(ConDat$Coarse_Aggregate))
cor(ConDat$Compressive_Strength, exp(ConDat$Coarse_Aggregate))
# No correlation improvement from these transformations

# Compressive strength and qty fine aggregate
cor(ConDat$Compressive_Strength, ConDat$Fine_Aggregate)
cor(ConDat$Compressive_Strength, sqrt(ConDat$Fine_Aggregate))
cor(ConDat$Compressive_Strength, (ConDat$Fine_Aggregate^2))
cor(ConDat$Compressive_Strength, log(ConDat$Fine_Aggregate))
cor(ConDat$Compressive_Strength, exp(ConDat$Fine_Aggregate))
# No correlation improvement from these transformations

# Compressive strength and age
cor(ConDat$Compressive_Strength, ConDat$Age)
cor(ConDat$Compressive_Strength, sqrt(ConDat$Age))
cor(ConDat$Compressive_Strength, (ConDat$Age^2))
cor(ConDat$Compressive_Strength, log(ConDat$Age))
cor(ConDat$Compressive_Strength, exp(ConDat$Age))
# No correlation improvement from these transformations

# At this point, ome background research (https://courses.washington.edu/cm425/strength.pdf)
# seemed to support the observation that some of the relationships associated with
# concrete compressive strength may not be linear and would require a transformation.
# In addition the he background research also identified an important relationship between
# concrete compressive strength and Water/Cement Ratio
# (a relationship known as Abram's law).
# Based on this finding transfromations of this ratio were also completed

# Compressive strength and water/cement based on Abrams law
cor(ConDat$Compressive_Strength, (ConDat$Water/ConDat$Cement))
cor(ConDat$Compressive_Strength, sqrt(ConDat$Water/ConDat$Cement))
cor(ConDat$Compressive_Strength, ((ConDat$Water/ConDat$Cement)^2))
cor(ConDat$Compressive_Strength, log(ConDat$Water/ConDat$Cement))
cor(ConDat$Compressive_Strength, exp(ConDat$Water/ConDat$Cement))

# log(Age) shows significant improvement in correlation to compressive strength
# Plot relationship between compressive strength and log(Age)
plot(Compressive_Strength ~ log(Age), data = ConDat, col="green")
abline(lm(Compressive_Strength ~ log(Age), data = ConDat), col="red")

# log(Water/Cement) also shows a strong correlation
plot(Compressive_Strength ~ log(Water/Cement), data = ConDat, col="green")
abline(lm(Compressive_Strength ~ log(Water/Cement), data = ConDat), col="red")

# Rebuild csmodel6 as csmodel7 replacing Age with log(Age) 
# and water and Cement variables with log(water/cement) 
# to see if model accuracy is improved
csmodel7 <- lm(Compressive_Strength~ log(Water/Cement) + Blast_Furnace_Slag + Fly_Ash + Superplasticizer + log(Age), data=TrainSet)
csmodel7
summary(csmodel7)

# This model is much better with an Adjusted R-squared value of 0.8093

# plot residuals
plot(csmodel7)
layout(matrix(1:2,2,2)) 
# residual plot looks reasonably randomly distributed around the zero line.
# Still some slight concerns with "funneling" but seems to be better than previous models

# Perfrom Durbin-Watson test to check for Independence
dwtest(csmodel7)
# If there is no autocorrelation the Durbin-Watson statistic should be between 1.5 and 2.5
# and the p-value will be above 0.05.
# With a DW value of 2.2018 and p value of 0.9981 there is no evidence of autocorrelation 

# Plot histogram and normal Q-Q plot for csmodel7 to confirm residuals are normally distributed
# Plot histogram
hist(residuals(csmodel7), col="gray")

# Plot Q-Q plot
qqnorm(residuals(csmodel7))

# Complete Shapiro-wilk test to confirm residuals are normally distributed
shapiro.test(csmodel7$residuals)
# Data looks not to be normally distributed (P value <0.05)

# Complete Anderson_Darling test to confirm residuala are normal distributed
ad.test(csmodel7$residuals)
# Data looks not to be normally distributed (P value <0.05)

# Data looks to be normally distributed

# calculate confidence and prediction intervals for csmodel7 (show first 20 rows only)
# Confidence Interval
confcs7 <- predict(csmodel7,newdata = ValidSet, interval="confidence")
head(confcs7, 20)
# Prediction Interval
predcs7 <- predict(csmodel7,newdata = data.frame(ValidSet), interval="prediction")
head(predcs7, 20)


# Plot & check correlation between response in ValidSet and response of csmodel7 built
# using TrainSet
plot(ValidSet$Compressive_Strength, predict(csmodel7, ValidSet),
     xlim =c(0,85), ylim=c(0,85), xlab= "actual", ylab = "predicted",
     main = "Compressive Strength - csmodel7")
abline(lm(ValidSet$Compressive_Strength ~ predict(csmodel7, ValidSet)), col = "red")
cor(ValidSet$Compressive_Strength, predict(csmodel7, ValidSet))
# Correlation has improved to about 90%

# Calculate RMSE and MAPE
CSPred <- predict(csmodel7, ValidSet)
actuals_preds <- data.frame(cbind(actuals=ValidSet$Compressive_Strength, predicteds=CSPred)) 
DMwR::regr.eval(actuals_preds$actuals, actuals_preds$predicteds)

########################################################
##### Part 4 - Artificial Neural Network Modelling #####
#########################################################
# Clear data from global environment before re-loading data

# Load the Concrete compressive Stregth Dataset from the UCI Machine Learning Repository
ConDat <- read_excel("Data/Concrete_Data.xls")

# Rename the attributes to simplify & Recheck structure of the data
names(ConDat) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Compressive_Strength")

# Normalize the ConDat dataset as Neural Networks work best when input data
# are scaled to a narrow range arond zero
# create function to normalize data
norm_function <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Nomalize ConDat dataset without Concrete Strength Category
ConDatNorm <- as.data.frame(lapply(ConDat, norm_function))
summary(ConDatNorm)
str(ConDatNorm)
head(ConDatNorm)

# Split Data into Training Set and Validation Set
set.seed(123)
annTrainSet <- ConDatNorm[1:721, ]  
annValidSet <- ConDatNorm[722:1030, ]

# Build first ANN model using all inputs
set.seed(123)
ann_conmodel1 <- neuralnet(Compressive_Strength ~.,data = annTrainSet)

# Plot first ANN mode
plot(ann_conmodel1)


# Plot first ANN model with plotnet
par(mar = numeric(4), family = 'serif')
plotnet(ann_conmodel1, alpha = 0.6)

# Check prediction accuracy of first ANN model
ann_results <- compute(ann_conmodel1, annValidSet[1:8])
ann_predict <- ann_results$net.result
cor(ann_predict, annValidSet$Compressive_Strength)


# Build second ANN model using 5 hidden nodes
set.seed(123)
ann_conmodel2 <- neuralnet(Compressive_Strength ~., data = annTrainSet, hidden =5)

# Plot second ANN model
plot(ann_conmodel2)

# Plot second ANN model with plotnet
par(mar = numeric(4), family = 'serif')
plotnet(ann_conmodel2, alpha = 0.6)

# Check prediction accuracy of second ANN model
ann_results2 <- compute(ann_conmodel2, annValidSet[1:8])
ann_predict2 <- ann_results2$net.result
cor(ann_predict2, annValidSet$Compressive_Strength)

# Build third ANN model using 5 hidden neurons
# and logistic activation function
set.seed(123)
ann_conmodel3 <- neuralnet(Compressive_Strength ~., data = annTrainSet, hidden = 3,
                           act.fct = "logistic")

# Plot third ANN model
plot(ann_conmodel3)

# Plot third ANN model with plotnet
par(mar = numeric(4), family = 'serif')
plotnet(ann_conmodel3, alpha = 0.6)

# Check prediction accuracy of third ANN model
ann_results3 <- compute(ann_conmodel3, annValidSet[1:8])
ann_predict3 <- ann_results3$net.result
cor(ann_predict3, annValidSet$Compressive_Strength)

###################################################################
##### Part 5 Data Visualization for Classification Algorithms #####
###################################################################

# It was decided to use notched box plots to see which features
# showed the best separation for Concrete Strength

# Load the Concrete compressive Stregth Dataset from the UCI Machine Learning Repository
ConDat <- read_excel("Data/Concrete_Data.xls")

# Rename the attributes to simplify & Recheck structure of the data
names(ConDat) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Compressive_Strength")

# Create 4 categories for classification purposes
# Fail - <5Mpa
# Low Strength 5-19MPa
# Standard Strength 20-49MPa
# High Strength >= 50MPa
# Concrete classification based on compressive strength in MPa was obtained from the
# website linked below:
# https://www.baseconcrete.co.uk/different-types-of-concrete-grades-and-their-uses/


ConDat$Concrete_Category[ConDat$Compressive_Strength <5] <- 1
ConDat$Concrete_Category[ConDat$Compressive_Strength >=5 & ConDat$Compressive_Strength <20] <- 2
ConDat$Concrete_Category[ConDat$Compressive_Strength >=20 & ConDat$Compressive_Strength<50] <- 3
ConDat$Concrete_Category[ConDat$Compressive_Strength >=50] <- 4

# Inspect Data
str(ConDat)
summary(ConDat)
head(ConDat)


# Remove Compressive_Strength column from datframe
ConDat = subset(ConDat, select = -c(9))

# Convert Concrete_Category to factors
ConDat$Concrete_Category <- factor(x=ConDat$Concrete_Category,
                                   levels=sort(unique(ConDat$Concrete_Category)),
                                   labels = c( "Fail", "Low Strength", "Standard Strength", "High Strength"))

# Inspect Data
str(ConDat)
summary(ConDat)
head(ConDat)

# Visualize features using notched box plots

# Cement
par(mfrow=c(1,1))
boxplot(ConDat$Cement ~ ConDat$Concrete_Category,
        notch=TRUE, staplewex = 1, main = "Cement",
        ylab="Kg/m^3", col="blue")

# Blast Furnace Slag
par(mfrow=c(1,1))
boxplot(ConDat$Blast_Furnace_Slag ~ ConDat$Concrete_Category,
        notch=TRUE, staplewex = 1, main = "Blast Furnace Slag",
        ylab="Kg/m^3", col="blue")

# Fly Ash
par(mfrow=c(1,1))
boxplot(ConDat$Fly_Ash ~ ConDat$Concrete_Category,
        notch=TRUE, staplewex = 1, main = "Fly Ash",
        ylab="Kg/m^3", col="blue")

# Water
par(mfrow=c(1,1))
boxplot(ConDat$Water ~ ConDat$Concrete_Category,
        notch=TRUE, staplewex = 1, main = "Water",
        ylab="Kg/m^3", col="blue")

# Superplasticizer
par(mfrow=c(1,1))
boxplot(ConDat$Superplasticizer ~ ConDat$Concrete_Category,
        notch=TRUE, staplewex = 1, main = "Superplasticizer",
        ylab="Kg/m^3", col="blue")

# Coarse Aggregate
par(mfrow=c(1,1))
boxplot(ConDat$Coarse_Aggregate ~ ConDat$Concrete_Category,
        notch=TRUE, staplewex = 1, main = "Coarse Aggregate",
        ylab="Kg/m^3", col="blue")

# Fine Aggregate
par(mfrow=c(1,1))
boxplot(ConDat$Fine_Aggregate ~ ConDat$Concrete_Category,
        notch=TRUE, staplewex = 1, main = "Fine Aggregate",
        ylab="Kg/m^3", col="blue")

# Age
par(mfrow=c(1,1))
boxplot(ConDat$Age ~ ConDat$Concrete_Category,
        notch=TRUE, staplewex = 1, main = "Age",
        ylab="days", col="blue")


############################################
##### Part 6 - Decision Tree Modelling #####
############################################

# Load the Concrete compressive Stregth Dataset from the UCI Machine Learning Repository
ConDat <- read_excel("Data/Concrete_Data.xls")

# Rename the attributes to simplify & Recheck structure of the data
names(ConDat) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Compressive_Strength")

# Check max and min values for 
max(ConDat$Compressive_Strength)
min(ConDat$Compressive_Strength)
# Classification needs to include compressive strength range of 2.3MPa to 82.6MPa

# It was decided to Create 4 categories for decision tree classification purposes
# Fail - <5Mpa
# Low Strength 5-19MPa
# Standard Strength 20-49MPa
# High Strength >= 50MPa


ConDat$Concrete_Category[ConDat$Compressive_Strength <5] <- 1
ConDat$Concrete_Category[ConDat$Compressive_Strength >=5 & ConDat$Compressive_Strength <20] <- 2
ConDat$Concrete_Category[ConDat$Compressive_Strength >=20 & ConDat$Compressive_Strength<50] <- 3
ConDat$Concrete_Category[ConDat$Compressive_Strength >=50] <- 4

# Inspect data to confirm values have been changed
summary(ConDat)
str(ConDat)
head(ConDat)

# Remove Compressive_Strength column from datframe
ConDat = subset(ConDat, select = -c(9))

# Inspect data to confirm removal of column
summary(ConDat)
str(ConDat)
head(ConDat)

# Convert Concrete_Category to factors
ConDat$Concrete_Category <- factor(x=ConDat$Concrete_Category,
                                    levels=sort(unique(ConDat$Concrete_Category)),
                                    labels = c( "Fail", "Low Strength", "Standard Strength", "High Strength"))

# Inspect data to confirm numbers changed to factors with names
summary(ConDat)
str(ConDat)
head(ConDat)


# shuffle data to eliminate time sequence
shuffle_index <- sample(1:nrow(ConDat))
ConDat <- ConDat[shuffle_index, ]


# Table of Concrete Category
table(ConDat$Concrete_Category)
# 6 Fails, 191 Low Strength, 623 standard strength and 210 High Strength

# Proportion Table of Concrete Categories
prop.table(table(ConDat$Concrete_Category))

# Split data into training (80%) and test (20%) datset
set.seed(123)
DT_Train <- sample(nrow(ConDat),0.8*nrow(ConDat), replace=FALSE)
DTTrainSet <- ConDat[DT_Train,]
DTValidSet <- ConDat[-DT_Train,]

#Inspect Training & Validation Sets
# Training Set
str(DTTrainSet)
summary(DTTrainSet)
head(DTTrainSet)
table(DTTrainSet$Concrete_Category)

# Validation Set
str(ValidSet)
summary(DTValidSet)
head(DTValidSet)
table(DTValidSet$Concrete_Category)

# Build Model using rpart
dt_model1 <- rpart(Concrete_Category ~., data = DTTrainSet, method = "class")

# Plot the trees
par(xpd = NA) # avoid clipping the text in some devices
plot(dt_model1)
text(dt_model1, digits = 3)

# nicer plot
fancyRpartPlot(dt_model1, cex=0.6)

# make predictions on the test data
predicted.classes <- dt_model1 %>%
  predict(DTValidSet, type = "class")
head(predicted.classes)

# Compute model accuracy rate on test data
mean(predicted.classes==DTValidSet$Concrete_Category)

# Confusion Matrix
confusionMatrix(predicted.classes,DTValidSet$Concrete_Category)

# Print & Plot cp values
printcp(dt_model1) 
plotcp(dt_model1)

# Find cp value with lowest cross validation error
cp <- dt_model1$cptable[which.min(dt_model1$cptable[,"xerror"])]
cp

# Prune the tree
###pruning
dt_model2 <- prune(dt_model1, cp = 0.01)

# plot the trees
par(xpd=NA) # Avoid clipping the text in some devices
plot(dt_model2)
text(dt_model2, digits=3)

# nicer plot
fancyRpartPlot(dt_model2, cex=0.6)

# make predictions on the test data
predicted.classes <- dt_model2 %>%
  predict(DTValidSet, type = "class")
head(predicted.classes)

# Compute model accuracy rate on test data
mean(predicted.classes==DTValidSet$Concrete_Category)

# Confusion Matrix
confusionMatrix(predicted.classes,DTValidSet$Concrete_Category)

############################################
##### Part 7 - Random Forest Modelling #####
############################################

# Clear data from global environment before re-loading data


# Load the Concrete compressive Stregth Dataset from the UCI Machine Learning Repository
ConDat <- read_excel("Data/Concrete_Data.xls")

# Rename the attributes to simplify & Recheck structure of the data
names(ConDat) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Compressive_Strength")

# Create 4 categories for Random Forest classification purposes
# Fail - <5Mpa
# Low Strength 5-19MPa
# Standard Strength 20-49MPa
# High Strength >= 50MPa


ConDat$Concrete_Category[ConDat$Compressive_Strength <5] <- 1
ConDat$Concrete_Category[ConDat$Compressive_Strength >=5 & ConDat$Compressive_Strength <20] <- 2
ConDat$Concrete_Category[ConDat$Compressive_Strength >=20 & ConDat$Compressive_Strength<50] <- 3
ConDat$Concrete_Category[ConDat$Compressive_Strength >=50] <- 4

# Inspect Data
str(ConDat)
summary(ConDat)
head(ConDat)


# Remove Compressive_Strength column from datframe
ConDat = subset(ConDat, select = -c(9))

# Convert Concrete_Category to factors
ConDat$Concrete_Category <- factor(x=ConDat$Concrete_Category,
                                   levels=sort(unique(ConDat$Concrete_Category)),
                                   labels = c( "Fail", "Low Strength", "Standard Strength", "High Strength"))

# Inspect Data
str(ConDat)
summary(ConDat)
head(ConDat)

# Create Training & Validation sets for Random Forest modelling
set.seed(123)
train <- sample(nrow(ConDat), 0.7*nrow(ConDat), replace = FALSE)

RFTrainSet <- ConDat[train,]
RFValidSet <- ConDat[-train,]
summary(RFTrainSet)
summary(RFValidSet)

# Create first Random Forest Model using all data apart from Concrete Compressive Strength

rf_model1 <- randomForest(as.factor(Concrete_Category) ~.,
                               data=RFTrainSet,
                               importance=TRUE)

#Plot Random Forest model
par(xpd=NA) # Avoid clipping the text in some devices
plot(rf_model1)


# Check which parameters are most influential
rffit <- randomForest(Concrete_Category ~
                        Cement + 
                        Blast_Furnace_Slag + 
                        Fly_Ash +
                        Water + 
                        Superplasticizer + 
                        Coarse_Aggregate + 
                        Fine_Aggregate + 
                        Age,
                      data=RFTrainSet, ntree=2000, keep.forest=FALSE, importance=TRUE)

varImpPlot(rffit) # plot results

# Using For Loop to identify the right mtry for model
a=c()
i=5
for (i in 3:6) {rf_model1 <- randomForest(Concrete_Category ~., data =
                                            RFTrainSet, ntree = 500, mtry = i, importance = TRUE)
predValid <- predict(rf_model1, RFValidSet, type = "class")
a[i-2] = mean(predValid ==
                RFValidSet$Concrete_Category)}

a
plot(3:6, a, pch = 19, cex=3,  main = "Best mtry plot", col = "red")

# Fine tuning parameters of Random Forest Model with mtry = 6
rf_model2 <- randomForest(Concrete_Category ~., data = RFTrainSet, ntree = 500, mtry = 4, importance = TRUE)
rf_model2


par(xpd=NA) # Avoid clipping the text in some devices
plot(rf_model2)

# Predicting on train set
predTrain <- predict(rf_model2, RFTrainSet, type = "class")


# Checking classification accuracy
table(predTrain, RFTrainSet$Concrete_Category)

# Predicting on Validation Set
predValid <- predict(rf_model2, RFValidSet, type = "class")


# Checking classification accuracy
mean(predValid == RFValidSet$Concrete_Category)
table(predValid, RFValidSet$Concrete_Category)

confusionMatrix(predValid, RFValidSet$Concrete_Category)

#####################################################
##### Part 8 - Support Vector Machine Modelling #####
#####################################################

# Clear data from global environment before re-loading data

# Load the Concrete compressive Stregth Dataset from the UCI Machine Learning Repository
ConDat <- read_excel("Data/Concrete_Data.xls")

# Rename the attributes to simplify & Recheck structure of the data
names(ConDat) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Compressive_Strength")

# Create 4 categories for SVM classification purposes
# Fail - <5Mpa
# Low Strength 5-19MPa
# Standard Strength 20-49MPa
# High Strength >= 50MPa


ConDat$Concrete_Category[ConDat$Compressive_Strength <5] <- 1
ConDat$Concrete_Category[ConDat$Compressive_Strength >=5 & ConDat$Compressive_Strength <20] <- 2
ConDat$Concrete_Category[ConDat$Compressive_Strength >=20 & ConDat$Compressive_Strength<50] <- 3
ConDat$Concrete_Category[ConDat$Compressive_Strength >=50] <- 4

# Remove Compressive_Strength column from datframe
ConDat = subset(ConDat, select = -c(9))

# Convert Concrete_Category to factors
ConDat$Concrete_Category <- factor(x=ConDat$Concrete_Category,
                                   levels=sort(unique(ConDat$Concrete_Category)),
                                   labels = c( "Fail", "Low Strength", "Standard Strength", "High Strength"))

# Table of Concrete Category
table(ConDat$Concrete_Category)

# Proportion Table of Concrete Category
round(prop.table(table(ConDat$Concrete_Category)), digits=2)

# Split data into training and test datset
set.seed(123)
train <- sample(nrow(ConDat),0.7*nrow(ConDat), replace=FALSE)
svmTrainSet <- ConDat[train,]
svmValidSet <- ConDat[-train,]

#Inspect Training & Validation Sets
# Training Set
str(svmTrainSet)
summary(svmTrainSet)
head(svmTrainSet)
table(svmTrainSet$Concrete_Category)

# Validation Set
str(svmValidSet)
summary(svmValidSet)
head(svmValidSet)
table(svmValidSet$Concrete_Category)

# Scale the feature set of the training and validatuion
# datasets apart from Concrete_Category prior to SVM modelling
svmTrainSet[-9] = scale(svmTrainSet[-9]) 
svmValidSet[-9] = scale(svmValidSet[-9]) 

#Re-inspect the scaled Training & Validation Sets
# Training Set
str(svmTrainSet)
summary(svmTrainSet)

# Validation Set
str(svmValidSet)
summary(svmValidSet)


# Build SVM classifier model with linear kernel
svm_model1 = svm(formula = Concrete_Category ~ ., 
                 data = svmTrainSet, 
                 type = 'C-classification', 
                 kernel = 'linear') 

summary(svm_model1)

# Test Model using Validation Set
svmpred = predict(svm_model1, newdata = svmValidSet[-9])

# Checking classification accuracy
mean(svmpred == svmValidSet$Concrete_Category)
table(svmpred, svmValidSet$Concrete_Category)

# Confusion Matrix
confusionMatrix(svmpred, svmValidSet$Concrete_Category)


# Build SVM classifier model with radial kernel
svm_model2 = svm(formula = Concrete_Category ~ ., 
                 data = svmTrainSet, 
                 type = 'C-classification', 
                 kernel = 'radial') 

summary(svm_model2)

# Test Model using Validation Set
svmpred = predict(svm_model2, newdata = svmValidSet[-9])

# Checking classification accuracy
mean(svmpred == svmValidSet$Concrete_Category)
table(svmpred, svmValidSet$Concrete_Category)

# Confusion Matrix
confusionMatrix(svmpred, svmValidSet$Concrete_Category)

##################################################
##### Part 9 - K-Nearest Neighbour Modelling #####
##################################################
# Clear data from global environment before re-loading data

# Load the Concrete compressive Stregth Dataset from the UCI Machine Learning Repository
ConDat <- read_excel("Data/Concrete_Data.xls")

# Rename the attributes to simplify & Recheck structure of the data
names(ConDat) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Compressive_Strength")

# Create 4 categories for knn classification purposes
# Fail - <5Mpa
# Low Strength 5-19MPa
# Standard Strength 20-49MPa
# High Strength >= 50MPa


ConDat$Concrete_Category[ConDat$Compressive_Strength <5] <- 1
ConDat$Concrete_Category[ConDat$Compressive_Strength >=5 & ConDat$Compressive_Strength <20] <- 2
ConDat$Concrete_Category[ConDat$Compressive_Strength >=20 & ConDat$Compressive_Strength<50] <- 3
ConDat$Concrete_Category[ConDat$Compressive_Strength >=50] <- 4

# Remove Compressive_Strength column from datframe
ConDat = subset(ConDat, select = -c(9))

# Convert Concrete_Category to factors
ConDat$Concrete_Category <- factor(x=ConDat$Concrete_Category,
                                   levels=sort(unique(ConDat$Concrete_Category)),
                                   labels = c( "Fail", "Low Strength", "Standard Strength", "High Strength"))

# Normalize data to make suitable for knn
ConDat_n <- as.data.frame(lapply(ConDat[1:8], normalize))
str(ConDat_n)

# split normalized data into training and test set
set.seed(123)
knnTrainSet <- ConDat_n[1:721, ]  
knnValidSet <- ConDat_n[722:1030, ]

# Create the vectors, TrainSet_labels and ValidSet_labels.
set.seed(123)
knnTrainSet_labels <- ConDat[1:721, 9]  
knnValidSet_labels <- ConDat[722:1030, 9]


knnValidSet_pred <- knn(train = knnTrainSet, test = knnValidSet,
                     cl = knnTrainSet_labels$Concrete_Category, k=4)
# need gmodels package
# Create a cross tabulation indicating the agreement between the two vectors.
# Specifying prop.chisq = FALSE will remove the chi-square values that are not needed

CrossTable(x = knnValidSet_labels$Concrete_Category, y = knnValidSet_pred, prop.chisq=FALSE)

# Calculate prediction accuracy
mean(knnValidSet_pred == knnValidSet_labels$Concrete_Category)

# Confusion Matrix
confusionMatrix(knnValidSet_pred, knnValidSet_labels$Concrete_Category)


#############################################
###### Part 10 - Naive-Bayes Modelling ######
#############################################
# Clear data from global environment before re-loading data

# Load the Concrete compressive Stregth Dataset from the UCI Machine Learning Repository
ConDat <- read_excel("Data/Concrete_Data.xls")

# Rename the attributes to simplify & Recheck structure of the data
names(ConDat) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", "Age", "Compressive_Strength")

# Create 4 categories for Naive-Bayes classification purposes
# Fail - <5Mpa
# Low Strength 5-19MPa
# Standard Strength 20-49MPa
# High Strength >= 50MPa


ConDat$Concrete_Category[ConDat$Compressive_Strength <5] <- 1
ConDat$Concrete_Category[ConDat$Compressive_Strength >=5 & ConDat$Compressive_Strength <20] <- 2
ConDat$Concrete_Category[ConDat$Compressive_Strength >=20 & ConDat$Compressive_Strength<50] <- 3
ConDat$Concrete_Category[ConDat$Compressive_Strength >=50] <- 4

# Remove Compressive_Strength column from datframe
ConDat = subset(ConDat, select = -c(9))

# Convert Concrete_Category to factors
ConDat$Concrete_Category <- factor(x=ConDat$Concrete_Category,
                                   levels=sort(unique(ConDat$Concrete_Category)),
                                   labels = c( "Fail", "Low Strength", "Standard Strength", "High Strength"))

# shuffle data to eliminate time sequence influence
shuffle_index <- sample(1:nrow(ConDat))
ConDat <- ConDat[shuffle_index, ]

# Inspect Shuffled data
summary(ConDat)
str(ConDat)
head(ConDat)

# Table of Concrete Strength Categories
table(ConDat$Concrete_Category)

# Proportion Table of Concrete Strength Categories
round(prop.table(table(ConDat$Concrete_Category)), digits=2)

# Split data into training and test datset
set.seed(123)
ConDat_train <- sample(nrow(ConDat),0.7*nrow(ConDat), replace=FALSE)
nbTrainSet <- ConDat[ConDat_train,]
nbValidSet <- ConDat[-ConDat_train,]

#Inspect Training & Validation Sets
# Training Set
str(nbTrainSet)
summary(nbTrainSet)
head(nbTrainSet)
table(nbTrainSet$Concrete_Category)

# Validation Set
str(nbValidSet)
summary(nbValidSet)
head(nbValidSet)
table(nbValidSet$Concrete_Category)

# Build Naive-Bayes Model
nb_model1 <- naive_bayes(Concrete_Category ~ ., nbTrainSet, laplace = 0)
nb_model1
summary(nb_model1)

# Plot first Naive-Bayes Model
par(mfrow=c(1,1))
plot(nb_model1, ask = TRUE)

# Predict using model and Validation Set
nbPred <- predict(nb_model1, nbValidSet, type = "class")


# Check model Accuracy using Validation Set
mean(nbPred == nbValidSet$Concrete_Category)
table(nbPred, nbValidSet$Concrete_Category)
confusionMatrix(nbPred, nbValidSet$Concrete_Category)

###############################################
###### Part 11 - Create Table of Results ######
###############################################
# Create vectors 
mlr <- c(0.897965, 0.8903,
         7.3154196, 0.2227156, NA, NA)
ann <- c( 0.7910229, NA, 
          NA, NA, NA, NA)
dt <- c(0.7508, NA,
        NA, NA, 0.5400, 0.9615)
rf <- c(0.8424, NA,
        NA, NA, 0.7049, 0.9798)
svm <- c(0.7730, NA,
         NA, NA, 0.6066, 0.9395)

knn <-  c(0.65572, NA,
          NA, NA,  0.41935, 0.89209)
nb <-  c(0.69357, NA,
         NA, NA, 0.46032,  0.92683)

# Create dataframe from vectors
df <- data.frame(mlr, ann, dt, rf, svm, knn, nb)

str(df)


# Add row names and column names
rownames(df) <- c("Accuracy", "Adjusted R-Squared", 
                  "RMSE", "MAPE", "Sensitivity", "Specificity")
colnames(df) <- c("Multiple Linear Regression", "Artificial Neural Network", "Decision Trees",
                  "Random Forest", "Support Vector Machine", "K Nearest Neighbour", "Naive-Bayes")df
# Inspect dataframe
str(df)

# Transpose dataframe
df_transpose = t(df)
df_transpose

