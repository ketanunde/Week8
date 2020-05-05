# Simple Linera Regression
# Independent Variable (same  as) = Explanatory variable
# Dependent Variable = resopnse variable

# Linear principles

head(women)
# dependent variable = weight
# independent variable = height

simple_linear_model <- lm(women$weight ~ women$height)
# or simple_linear_model <- lm(weight ~ height, data = women)

simple_linear_model

plot(women$height, xlab = "Height(in)", 
     ylab = "Weight(lbs)",
     main = "Scatter plot showing regression line for the weigh predicted from height")

abline(simple_linear_model)

summary(simple_linear_model)

# Conf interval of the model
confint(simple_linear_model)

cor(women$height, women$weight)

# Example using cars dataset
# contains stopping distance foor the cars from 1920s

head(cars)

# check model assumptions first

# step 1 - check linearity

# present data in a scatter plot
# x-axis is independent variable and y- axis is dependent variable
scatter.smooth(x = cars$speed, 
               y = cars$dist,
               main = "Distance ~ Speed", 
               xlab = "Car speed(mph)",
               ylab = "Stopping distances(yards)")

# a low correlation = -0.2 < x < 0.2

cor(cars$speed, cars$dist)

# check for outliers
# generally outliers is the defiened as 1.5 X interquartile range
# IQR = distance between 25th and 75th percentile

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # divide graph area into 1 row x 2 cols

attach(cars)

boxplot(speed,
        main = "Speed", 
        sub = paste("Outliers rows: ",
                    boxplot.stats(speed)$out))

boxplot(dist,
        main = "Distance", 
        sub = paste("Outliers rows: ",
                    boxplot.stats(dist)$out))

detach(cars)
par <- opar

# Boxplot suggest that distance value of 120 is an outlier
nrow(cars)
cars_content <- cars

cars <- subset(cars, cars$dist !=120)
nrow(cars)

# check for normality 

# Skewness function to example to examine normality
install.packages("e1071")
library(e1071)

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # divide graph area into 1 row x 2 cols

# Density plot for speed 
# minimum skewed to left
# skewness of < -1 or >1 = heighly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

plot(density(cars$speed), 
     main = "Density plot : Speed",
     ylab = "Frequency",xlab = "SPeed",
     sub = paste("Skewness : ", round(e1071:: skewness(cars$speed), 2)))

# fill the area under the plot
polygon(density(cars$speed), col = "red")

plot(density(cars$dist), 
     main = "Density plot : Distance",
     ylab = "Frequency", xlab = "SPeed",
     sub = paste("Skewness : ", round(e1071:: skewness(cars$dist), 2)))

# fill the area under the plot
polygon(density(cars$dist), col = "red")
par <- opar


# we can examine noramality using the qqnorm() function
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2)) # Divided the graph area into 1 row x 2 col
hist(cars$dist,
     main = "Normality proportion of distance",
     xlab = "Distance")

qqnorm(cars$dist)
qqline(cars$dist)

par <- opar

# Training and testing datasets
set.seed(1)
no_rows_data <- nrow(cars)
sample_data <- sample(1:no_rows_data, 
                 size = round(0.7 * no_rows_data), 
                 replace = FALSE)

training_data <- cars[sample_data, ]
testing_data <- cars[-sample_data, ]

# now built linear model using this training data
linearModel <- lm(dist ~ speed, data = training_data)

linearModel

summary(linearModel)

# AIC and BIC
AIC(linearModel)

BIC(linearModel)

# Predict using the model
predicted_distance <- predict(linearModel, testing_data)

# make actual vs  predicted data frame
actual_predictions <- data.frame(cbind(actuals = testing_data$dist, 
                                       predicted = predicted_distance))
head(actual_predictions)

correlation_accuracy <- cor(actual_predictions)
correlation_accuracy

# min - Amx accuracy
min_max_accuracy <- mean(apply(actual_predictions, 1, min) / apply(actual_predictions, 1, max))
min_max_accuracy


# Mean absoulte percentage error (MAPE)
mape <- mean(abs((actual_predictions$predicted - actual_predictions$actuals)) / actual_predictions$actuals)
mape

# demo of inputting a value into the model
df <- data.frame(speed = c(35))
predicted_distance <- predict(linearModel)
predicted_distance


