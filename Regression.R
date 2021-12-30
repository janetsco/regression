library(ggplot2)

#Read in the csv
data <- read.csv('/Users/janetscott/Documents/Articles/Store.csv')

#View the first few lines of data
head(data)

#Create a new data frame with only the fields we are interested in.
data2 <- data[, c('revenue', 'online', 'instore')]

#View the first few lines of the new data frame
head(data2)

#Create a scatter plot to visualize the relationship between x and y
scatter.smooth(x=data2$online, y=data2$revenue, main="Revenue ~ Online",
xlab = "Online", ylab = "Revenue")
               
scatter.smooth(x=data2$instore, y=data2$revenue, main="Revenue ~ In Store",
xlab = "In Store", ylab = "Revenue")

#Fit a regression model
model <- lm(data2$revenue~data2$online)

#Return a list of the residuals 
res <- resid(model)

#Create residual vs. fitted plot
plot(fitted(model), res)

#Add a horizontal line at 0 
abline(0,0)

#Create a normal probability plot (Q-Q plot) of the residuals
qqnorm(residuals(model), main="Q-Q plot")
qqline(residuals(model))

#Display the results of the model
summary(model)

#Display the r-squared value
summary(model)$r.squared

