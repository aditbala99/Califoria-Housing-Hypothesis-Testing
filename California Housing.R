datan  <- read.csv("C:/Users/aditya/OneDrive/Desktop/housing.csv", header=T)
#print(datan)
summary(datan$median_house_value)
dim(datan) #dimensions of the dataset
str(datan) #overview of the dataset
head(datan)
#boxplot(datan)
typeof(datan)

#To treat NULL data in data frame
for(i in 1:ncol(datan)){
  if(sum(is.na(datan[,i])) > 0){
    if(mode(datan[,i]) == 'numeric'){
      datan[is.na(datan[,i]), i] <- median(datan[,i], na.rm = TRUE)
    }
    else if(mode(datan[,i]) == 'character'){
      datan[is.na(datan[,i]), i] <- getmode(datan[,i])
    }}
}

#one hot encoding to treat categorical variables
one_hot_encoding = function(df, columns="xyz"){
  # create a copy of the original data.frame for not modifying the original
  df = cbind(df)
  # convert the columns to vector in case it is a string
  columns = c(columns)
  # for each variable perform the One hot encoding
  for (column in columns){
    unique_values = sort(unique(df[column])[,column])
    non_reference_values  = unique_values[c(-1)] # the first element is going 
    # to be the reference by default
    for (value in non_reference_values){
      # the new dummy column name
      new_col_name = paste0(column,'.',value)
      # create new dummy column for each value of the non_reference_values
      df[new_col_name] <- with(df, ifelse(df[,column] == value, 1, 0))
    }
    # delete the one hot encoded column
    df[column] = NULL
    
  }
  return(df)
}

datan <- one_hot_encoding(datan, columns = c("ocean_proximity"))
datan = subset(datan, select = -c(1,2))


#Plotting bivariate analysis
y <- datan$median_house_value
x1 <- datan$housing_median_age
x2 <- datan$total_rooms
x3 <- datan$total_bedrooms
x4 <- datan$population
x5 <- datan$households
x6 <- datan$median_income

plot(x1, y, main = "Price vs House Age",
     xlab = "House Age", ylab = "Price",
     pch = 19, frame = FALSE, col = "light blue")
abline(lm(y ~ x1, data = mtcars), col = "dark blue")

plot(x2, y, main = "Price vs Total Rooms in Block",
     xlab = "Total Rooms", ylab = "Price",
     pch = 19, frame = FALSE, col = "light blue")
abline(lm(y ~ x2, data = mtcars), col = "dark blue")

plot(x3, y, main = "Price vs Total Bedrooms in Block",
     xlab = "Total Bedrooms", ylab = "Price per unit area",
     pch = 19, frame = FALSE, col = "light blue")
abline(lm(y ~ x3, data = mtcars), col = "dark blue")
plot(x4, y, main = "Price vs Population in Block",
     xlab = "Population", ylab = "Price per unit area",
     pch = 19, frame = FALSE, col = "light blue")
abline(lm(y ~ x4, data = mtcars), col = "dark blue")
plot(x5, y, main = "Price vs Households in Block",
     xlab = "Households", ylab = "Price",
     pch = 19, frame = FALSE, col = "light blue")
abline(lm(y ~ x5, data = mtcars), col = "dark blue")
plot(x6, y, main = "Price vs Median Income",
     xlab = "Median Income", ylab = "Price per unit area",
     pch = 19, frame = FALSE, col = "light blue")
abline(lm(y ~ x6, data = mtcars), col = "dark blue")


attach(datan)

#date <- data$X1.transaction.date
#age<- data$X2.house.age
#dist<- data$X3.distance.to.the.nearest.MRT.station
#stores<- data$X4.number.of.convenience.stores
#lat<- data$X5.latitude
#longi<- data$X6.longitude
#price<- data$Y.house.price.of.unit.area

#Check for outliers
library(plotly)
plot_ly(datan, y = datan$median_house_value, type="box", name = 'House Value') %>%
  add_trace(y = datan$housing_median_age, name = 'House Age') %>%
  add_trace(y = datan$total_rooms, name = 'Total Rooms') %>%
  add_trace(y = datan$median_income, name = 'Median Income') %>%
  add_trace(y = datan$total_bedrooms, name = 'Total Bedrooms') %>%
  add_trace(y = datan$population, name = 'Population in Block') %>%
  add_trace(y = datan$households, name = 'Total Households in Block') %>%
  
  layout(yaxis = list(title = 'Measure')) -> p
p  


#treat Outliers
library('ExPanDaR')
datan$total_rooms <- treat_outliers(datan$total_rooms, 0.05)
datan$total_bedrooms <- treat_outliers(datan$total_bedrooms, 0.05)
datan$population <- treat_outliers(datan$population, 0.05)
datan$households <- treat_outliers(datan$households, 0.05)
datan$median_income <- treat_outliers(datan$median_income, 0.05)

#Boxplot after treating outiers
library(plotly)
plot_ly(datan, y = datan$median_house_value, type="box", name = 'House Value') %>%
  add_trace(y = datan$housing_median_age, name = 'House Age') %>%
  add_trace(y = datan$total_rooms, name = 'Total Rooms') %>%
  add_trace(y = datan$median_income, name = 'Median Income') %>%
  add_trace(y = datan$total_bedrooms, name = 'Total Bedrooms') %>%
  add_trace(y = datan$population, name = 'Population in Block') %>%
  add_trace(y = datan$households, name = 'Total Households in Block') %>%
  
  layout(yaxis = list(title = 'Measure')) -> p
p  


#Plotting individual data
library(ggplot2)
ggplot(datan, aes(x=datan$housing_median_age)) + ggtitle('House Age') +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
ggplot(datan, aes(x=datan$total_rooms)) + ggtitle('Total Rooms') +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 80, colour="black", fill="white") + ylab("Percentage")
ggplot(datan, aes(x=datan$total_bedrooms)) + ggtitle('Total Bedrooms') +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 50, colour="black", fill="white") + ylab("Percentage")
ggplot(datan, aes(x=datan$population)) + ggtitle('Population') +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 50, colour="black", fill="white") + ylab("Percentage")
ggplot(datan, aes(x=datan$households)) + ggtitle('Households') +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 50, colour="black", fill="white") + ylab("Percentage")
ggplot(datan, aes(x=datan$median_income)) + ggtitle('Median Income') +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
ggplot(datan, aes(x=datan$median_house_value)) + ggtitle('Median House Value') +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 7000, colour="black", fill="white") + ylab("Number of Houses") + xlab("Median House Price")

#Z Test Hyp Testing
set.seed(123)
split = datan[sample(nrow(datan), size=45), ]
alpha = 0.05
u = mean(datan$median_house_value) 
n = length(split)
xbar = mean(split$median_house_value)
s = sd(split$median_house_value)
z_val = abs((xbar - u)/(s/sqrt(n)))
?pnorm
p_val = 2 * pnorm(z_val, lower.tail = FALSE)

cat('The t-value for the distribution is ', z_val)
cat('The p-value for the distribution is ', p_val)
if(p_val <= alpha){
  print('Reject the NULL hypothesis')
} else{
  print('Fail to reject NULL hypothesis')
}
z <- qnorm(0.975)
lower <- xbar-(z*(s/sqrt(n)))
upper <- xbar+(z*(s/sqrt(n)))
cat('The lower bound of confidence interval is ', lower)
cat('The upper bound of confidence interval is ', upper)

#T Test Hyp Testing
# H0: Mean of the Median House Price is 206855.82
# H1: Mean of the Median House Price is not 206855.82 
# Two-tailed sample test 
#Since n <= 30, so we take the t-distribution

library(caTools)
set.seed(123)
split = datan[sample(nrow(datan), size=20), ]
alpha = 0.05
u = mean(datan$median_house_value)
n = length(split$median_house_value)
xbar = mean(split$median_house_value)
s = sd(split$median_house_value)
t_val = abs((xbar - u)/(s/sqrt(n)))
p_val = 2 * pt(t_val, df = (n - 1), lower.tail = FALSE)
cat('The t-value for the distribution is ', t_val)
cat('The p-value for the distribution is ', p_val)
if(p_val <= alpha){
  print('Reject the NULL hypothesis')
} else{
  print('Not able to reject NULL hypothesis')
}
ci = 0.9
alpha = 1 - ci
aplha = alpha/2
mu = mean(datan$median_house_value)
n = length(split$median_house_value)
xbar = mean(split$median_house_value)
s = sd(split$median_house_value)
#t_val = abs((xbar - mu)/(s/sqrt(n)))
t_val = qt(0.95,df=(n-1))

lower <- xbar-(t_val*(s/sqrt(n)))
upper <- xbar+(t_val*(s/sqrt(n)))
cat('The lower bound of confidence interval is ', lower)
cat('The upper bound of confidence interval is ', upper)

#Building Regression model
library(caTools)
set.seed(123)
split = sample.split(datan$median_house_value, SplitRatio = 0.9)
training_set = subset(datan, split == TRUE)
test_set = subset(datan, split == FALSE)
#price_t<- test_set$median_house_value

m <- lm(formula = median_house_value ~ ., data = training_set)
summary(m)

# Predicting the Test set results
y_pred = predict(m, newdata = test_set)
#y_pred<-unname(y_pred)

MSE <- mean((y_pred - test_set$median_house_value)^2)
MSE
totalss <- sum((test_set$median_house_value - mean(test_set$median_house_value))^2)
totalss
# Regression and Residual Sum of the Squered.
regss <- sum((y_pred - mean(test_set$median_house_value))^2)
regss
resiss <- sum((test_set$median_house_value - y_pred)^2)
resiss
# Calulate R squared.
R2 <- regss/totalss
R2

#Correlation metrix
library(corrplot)
numeric.var <- sapply(datan, is.numeric)
corr.matrix <- cor(datan[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", order = "hclust", tl.col = "black", tl.srt=25, tl.cex=0.5, cl.cex=0.5,method = "number")
summary(m)
library("olsrr")
ols_eigen_cindex(m)

















