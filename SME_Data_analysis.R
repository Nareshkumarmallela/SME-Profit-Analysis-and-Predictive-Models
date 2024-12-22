library(tidyverse)
library(ggcorrplot)
library(leaps)

SME_data<-read.csv("SME_Profit.csv") #load data

str(SME_data) #Structure of the data

SME_data <- SME_data %>%
  mutate(State = as.factor(State))

SME_data<-SME_data[,1:5] # Removing the redundant variables

str(SME_data) #To View Structure of the data(Analyse the result)

numeric_SME_data<-Filter(is.numeric,SME_data) #Creating dataframe for boxplots 

#Normalization
numeric_SME_data[, c("R.D.Spend", "Administration", "Marketing.Spend","Profit")] <-
  scale(numeric_SME_data[, c("R.D.Spend", "Administration", "Marketing.Spend","Profit")])

#Boxplots
boxplot(numeric_SME_data,main = "Box Plots of Numeric Variables", col = c("lightblue", "lightgreen", "lightpink"))

#Scatter Diagram

# Define a color palette for the states
state_colors <- rainbow(length(levels(SME_data$State)))

#partioning the plotting space
par(mfrow = c(1,3))

plot(SME_data$Profit,SME_data$R.D.Spend,col = state_colors, 
     xlab = "Profit", ylab = "R.D.Spend")

plot(SME_data$Profit,SME_data$Administration,col = state_colors, main = "Relationship Scatter Diagram", 
     xlab = "Profit", ylab = "Administration")
# Add legend
legend("topright", legend = levels(SME_data$State), 
       col = state_colors, 
       pch = 1)

plot(SME_data$Profit,SME_data$Marketing.Spend,col = state_colors, 
     xlab = "Profit", ylab = "Marketing.Spend")
dev.off()
# Create a heatmap with percentage labels
ggcorrplot(cor(numeric_SME_data),title = "Correlation Heat Map", lab = TRUE, lab_size = 3)

#Creating dummy Variable for factorial Data
SME_data$State_Newyork<-ifelse(SME_data$State=="New York",1,0)
SME_data$State_California<-ifelse(SME_data$State=="California",1,0)
SME_data<-SME_data[,-4] 

# Fitting a Predictive Model
Profit_model<-lm(Profit~.,SME_data)
summary(Profit_model)

# Partitioning the Data
set.seed(1) # to guarantee that the same random values are produced each time you run the code.
Train.index<-sample(c(1:50),30) # used 60% of data for training(50*60%) 
train_SME<-SME_data[Train.index,]  #training dataset
valid_SME<-SME_data[-Train.index,] #validation Dataset

# Multiple Linear Regression

SME_data.lm<-lm(Profit~.,data=train_SME)
options(scipen = 999)
summary(SME_data.lm)

#Predicting the profit using validation data
SME_data.lm.pred <- predict(SME_data.lm,valid_SME)
SME_data.lm.pred

# Create a residual plot
plot(SME_data.lm$fitted.values, SME_data.lm$residuals,
     main = "Residual Plot",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, lty = 1, lwd = 2, col = "red")  # Add a horizontal line at 0

## Selecting subsets of Predictors
###Exhaustive R###
Exhaustive_search <- regsubsets(Profit~.,data = train_SME,
                                nbest = 1,nvmax = dim(train_SME)[2],method = "exhaustive")
sum<-summary(Exhaustive_search)
sum
sum$cp

###Forward R##

## create model with no predictors for bottom of search range
SME_data.lm.null <- lm(Profit~1, data = train_SME)

# use step() to run forward selection
SME_data.lm.step <- step(SME_data.lm.null,
                         scope=list(lower=SME_data.lm.null, upper=SME_data.lm), direction =
                           "forward")
summary(SME_data.lm.step)

###Backward R###

SME_data.lm.step1 <- step(SME_data.lm, direction = "backward")
summary(SME_data.lm.step1)

###Stepwise R###
SME_data.lm.step2 <- step(SME_data.lm, direction = "both")
summary(SME_data.lm.step2)

###PCA###
pca_SME<-SME_data[,1:3] ##removing categorical Variable
pca_SME
pca<-prcomp(pca_SME,scale. = TRUE)
summary(pca)
