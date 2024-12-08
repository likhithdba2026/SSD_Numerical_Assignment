dataset <- mtcars
str(dataset)
colnames(dataset)
dim(dataset)
summary(dataset)

colSums(is.na(dataset))


observations <- nrow(dataset)
print(observations)

variables <- ncol(dataset)
print(variables)
dataset
#len(dataset)
#length(dataset)

df_mpg=dataset$mpg
mean(df_mpg)
median(df_mpg)
sd(df_mpg)
min(df_mpg)
max(df_mpg)


hist(df_mpg, breaks = "Sturges", col = "skyblue", main = "Histogram of mpg", xlab = "mpg")

value_counts <- table(dataset$vs)
print(value_counts)

df_vs = dataset$vs
#barplot(counts,
        #names.arg = categories,
        #col = "skyblue",
        #main = "Basic Bar Plot",
        #xlab = "Categories",
        #ylab = "Frequency")

# Box plot for mpg
boxplot(df_mpg,
        main = "Box Plot of mpg (Miles Per Gallon)",
        xlab = "mpg",
        col = "skyblue")


df_hp =dataset$hp
cor(df_mpg,df_hp)

# the value is -0.776, this indicates miles per gallon is inversely related to gross horsepower
# Strongly negatively correlated
# as one feature increases, the other feature decreases



?mtcars


# Fit a linear model
model <- lm(mpg ~ hp, data = mtcars)

# Scatter plot with trend line
plot(df_hp, df_mpg,
     main = "Scatter Plot with Trend Line",
     xlab = "Horsepower",
     ylab = "Miles Per Gallon",
     col = "blue",
     pch = 19)

# Add regression line
abline(model, col = "red", lwd = 2)


# As we can see from the plot, As the two columns are strong negatively correlated 
# So as one value increases, the other value decreases

# Fit the linear regression model
model <- lm(mpg ~ hp + wt, data = mtcars)

# View the summary of the model
summary(model)


# Diagnostic plots
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)

#additional tests
shapiro.test(residuals(model))

# Install and load lmtest
install.packages("lmtest")
library(lmtest)

# Perform Breusch-Pagan test
bptest(model)
