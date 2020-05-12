states <- as.data.frame(state.x77)

# Add the states name as variable
states$Name <- state.name
head(states)


colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

# Remove area variable
states <- subset(states, select = -c(Area))

# Training and testing
set.seed(1)
no_rows_data <- nrow(states)
sample <- sample(1:no_rows_data, 
                 size = round(0.7 * no_rows_data),
                 replace = FALSE)

training_data <- states[sample,]
testing_data <- states[-sample,]

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost +Life_Exp + HS_Grad, data = training_data)

# Multiple Linear Regession Evaluation
summary(fit)

confint(fit)

# Regression Diagnostics
# Lots of function in the car packge


library(car)
qqPlot(fit, 
       labels = row.names(states), 
       id.method = "identify",
       simulate = TRUE, 
       Main = "Q-Q plot")

# We need to analyze Alabama And Colorado
training_data["Alabama",]
training_data["Colorado",]

fitted(fit)["Alabama"]
fitted(fit)["Colorado"]

student_fit <- rstudent(fit)
hist(student_fit, 
     breaks = 10, 
     freq = FALSE, 
     xlab = "Studenttized rsidual",
     main = "Distribution of errors")

curve(dnorm(x, mean = mean(student_fit),
            sd = sd(student_fit)),
            add = TRUE,
            col = "blue", lwd = 2)

lines(density(student_fit)$x, density(student_fit)$y, col="red",lwd=2 ,lty = 2)
#legend("topright",legend = c("Normal curve", "Kennel densitycurve"),lty = 1:2,col = c("blue", "red"),cex = 0.7)

outlierTest(fit)

# Now deleting the Colorado records
# Delete Colorado
# Split Data into train and test
# Rebuild model

states <- subset(states, states$Name != "Colorado")

# Train test Split
set.seed(1)
no_rows_data <- nrow(states)
sample <- sample(1:no_rows_data, 
                 size = round(0.7 * no_rows_data),
                 replace = FALSE)

training_data <- states[sample,]
testing_data <- states[-sample,]

# Rebuild Model

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost +Life_Exp + HS_Grad, data = training_data)

outlierTest(fit)

# the Histogram
student_fit <- rstudent(fit)
hist(student_fit, 
     breaks = 10, 
     freq = FALSE, 
     xlab = "Studenttized rsidual",
     main = "Distribution of errors")

curve(dnorm(x, mean = mean(student_fit),sd = sd(student_fit)),add = TRUE,col = "blue", lwd = 2)
lines(density(student_fit)$x, density(student_fit)$y, col="red",lwd=2 ,lty = 2)

crPlots(fit)


# Cqoks D value =4/(Sample size - no of predicted values -1)
cutoff <- 4/(nrow(training_data) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h= cutoff, lty = 2, col = "red")

avPlots(fit, ask = FALSE)

# Influence plot
influencePlot(fit,
              main = "Influence plot", sub = "Circle size is proportional to Cooks distance")

ncvTest(fit)
spreadLevelPlot(fit)

library(gvlma)
gvlmodel <- gvlma(fit)
gvlmodel

library(MASS)
fit_test <- lm(Murder ~ Population + Illiteracy + Income + Frost +Life_Exp + HS_Grad, data = training_data)
stepAIC(fit_test, direction = "backward")

library(leaps)
leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost +Life_Exp + HS_Grad, data = training_data)
plot(leaps, scale = "adjr2")

# Predicting the Accuracy
fit_model <- lm(Murder ~ Population + Illiteracy + Income + Frost +Life_Exp + HS_Grad, data = training_data)
predicted_murder <- predict(fit_model, testing_data)

actual_predictions <- data.frame(cbind(actuals = testing_data$Murder, predicted = predicted_murder)) 
head(actual_predictions)

correlation_accuracy <- cor(actual_predictions)
correlation_accuracy

# Min- Max Accuracy 
min_max_accuracy <- mean(apply(actual_predictions, 1, min) / apply(actual_predictions, 1, max))
min_max_accuracy