# Working with the state.x77 dataset
?state.x77
head(state.x77)
states_data <- state.x77
nrow(states_data)
str(states_data)
class(states_data)

# Converting the dataset to data frame
new_state <- as.data.frame(states_data)
new_state
str(new_state)
class(new_state)

# Changing the variable names
colnames(new_state)
names(new_state)[4] <- "Life_exp"
names(new_state)[6] <- "HS_grade"
str(new_state)


# The Murder variable is we have to check the coorelation 
# with the other variables.
# If it's not correlated then it not of any use for building model

pairs(new_state)
# Chart provides general level of details on linearity.
# We need to check in more details with independent variable on X-axis.

scatter.smooth(x = new_state$Murder,
               y = new_state$Population,
               main = "Murders ~ Population",
               xlab = "Murders",
               ylab = "Polulatino")
cor(new_state$Murder, new_state$Population)

scatter.smooth(x = new_state$Murder,
               y = new_state$Income,
               main = "Murders ~ Income",
               xlab = "Murders",
               ylab = "Income")
cor(new_state$Murder, new_state$Income)

scatter.smooth(x = new_state$Murder,
               y = new_state$Illiteracy,
               main = "Murders ~ Illiteracy",
               xlab = "Murders",
               ylab = "Illiteracy")
cor(new_state$Murder, new_state$Illiteracy)

scatter.smooth(x = new_state$Murder,
               y = new_state$Life_exp,
               main = "Murders ~ Life_exp",
               xlab = "Murders",
               ylab = "Life_exp")
cor(new_state$Murder, new_state$Life_exp)

scatter.smooth(x = new_state$Murder,
               y = new_state$HS_grade,
               main = "Murders ~ HS_Grade",
               xlab = "Murders",
               ylab = "HS_Grade")
cor(new_state$Murder, new_state$HS_grade)

scatter.smooth(x = new_state$Murder,
               y = new_state$Frost,
               main = "Murders ~ Frost",
               xlab = "Murders",
               ylab = "Frost")
cor(new_state$Murder, new_state$Frost)

scatter.smooth(x = new_state$Murder,
               y = new_state$Area,
               main = "Murders ~ Area",
               xlab = "Murders",
               ylab = "Area")
cor(new_state$Murder, new_state$Area)


opar <- par(no.readonly = TRUE)


par(mfrow = c(1, 2)) # divide graph area in 2 columns
attach(new_state)
boxplot(Murder, 
        main = "Murder", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Murder)$out)) # box plot for 'speed'

boxplot(Illiteracy, 
        main = "Illiteracy", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Illiteracy)$out)) # box plot for 'distance'
detach(new_state)
par <- opar

# Outliers
par(mfrow = c(1, 2)) # divide graph area in 2 columns
attach(new_state)

boxplot(Murder, 
        main = "Murder", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Murder)$out)) # box plot for 'speed'

boxplot(Life_exp, 
        main = "Life_exp", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Life_exp)$out)) # box plot for 'distance'

detach(new_state)
par <- opar


# There is outliers
# 
par(mfrow = c(1, 2)) # divide graph area in 2 columns
attach(new_state)
boxplot(Murder, 
        main = "Murder", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Murder)$out)) # box plot for 'speed'

boxplot(Population, 
        main = "Population", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Population)$out)) # box plot for 'distance'

detach(new_state)
par <- opar







