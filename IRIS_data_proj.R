dataset <- read.csv('/Users/likhithdabbikar/Desktop/out/Python_and_DATA_Science/datasets/IRIS.csv')
head(dataset,5)
str(dataset)
colnames(dataset)
dim(dataset)
summary(dataset)

colSums(is.na(dataset))
observations <- nrow(dataset)
print(observations)

variables <- ncol(dataset)
print(variables)

#2 question (Summary Statistics)
df_sl=dataset$sepal_length
mean(df_sl)
median(df_sl)
sd(df_sl)
min(df_sl)
max(df_sl)

#3 question
hist(df_sl, breaks = "Sturges", col = "skyblue", main = "Histogram of Sepal_length", xlab = "Sepal_length")


boxplot(df_sl,
        main = "Box Plot of Sepal length",
        ylab = "Sepal length Values",
        col = "lightgreen",
        border = "darkgreen",
        notch = TRUE)
# Adding a grid for clarity
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

#4 question
# Load the Iris dataset

df_species = dataset$species
# Summarize the counts of each species
species_counts <- table(df_species)

# Create a bar plot
barplot(
  species_counts,
  main = "Count of Each Species in the Iris Dataset",
  xlab = "Species",
  ylab = "Count",
  col = "skyblue",
  ylim = c(0, max(species_counts) + 5)
)

#5 question
df_sw =dataset$sepal_width
cor(df_sl,df_sw)

# highly uncorrelated

#6 question


# Plot the scatter plot
plot(df_sw, df_sl,  # Note the order: x (independent) first, then y (dependent)
     main = "Scatter Plot with Trend Line",
     xlab = "Sepal Width",
     ylab = "Sepal Length",
     col = "blue",
     pch = 19)

# Fit a linear model
model <- lm(df_sl ~ df_sw)  # Correct formula: y ~ x

# Add the trend line
abline(model, col = "red", lwd = 2) 

# Taking the labeling data petal length
df_pl = dataset$petal_length

model <- lm(dataset$petal_length ~ dataset$sepal_length + dataset$sepal_width)

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


