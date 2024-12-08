dataset <- read.csv('/Users/likhithdabbikar/Desktop/out/Python_and_DATA_Science/datasets/pokemon_data.csv')
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

head(dataset)
#2 question (Summary Statistics)
df_attack=dataset$Attack
mean(df_attack)
median(df_attack)
sd(df_attack)
min(df_attack)
max(df_attack)

#3 question
hist(df_attack, breaks = "Sturges", col = "skyblue", main = "Histogram of attack power of pokemons", xlab = "Pokemon's attack power")



boxplot(df_attack,
        main = "Box Plot of attack power of pokemons",
        ylab = "Attack power",
        col = "lightgreen",
        border = "darkgreen",
        notch = FALSE)
# Adding a grid for clarity
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

#4 question


# Create a sample dataset
df_type <- dataset$Type.1

# Summarize the counts of each category
type_counts <- table(df_type)

# Create a bar plot and save bar midpoints
bar_midpoints <- barplot(
  type_counts,
  main = "Count of Each pokemon type in the Dataset",
  xlab = "Pokemon Type",
  ylab = "Count",
  col = "skyblue",
  ylim = c(0, max(type_counts) + 5)  # Adjust ylim for label space
)





#5 question
df_hp =dataset$HP
cor(df_attack,df_hp)

# highly uncorrelated

#6 question
df_age = dataset$age

# Plot the scatter plot
plot(df_attack ,df_hp,   # Note the order: x (independent) first, then y (dependent)
     main = "Scatter Plot with Trend Line",
     xlab = "attack",
     ylab = "HP",
     col = "blue",
     pch = 19)

# Fit a linear model
model <- lm(df_attack ~ df_hp)  # Correct formula: y ~ x

# Add the trend line
abline(model, col = "red", lwd = 2) 

# Taking the labeling data petal length
df_speed = dataset$speed

model <- lm(dataset$Speed ~ df_attack + df_hp )

summary(model)
head(dataset)
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


numerical_data <- dataset[, c(5:10)]
head(numerical_data)
# Perform PCA
pca_model <- prcomp(numerical_data, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca_model)

# Plot explained variance (Scree plot)
explained_variance <- (pca_model$sdev^2) / sum(pca_model$sdev^2)
plot(explained_variance, type = "b",
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Scree Plot")
cumulative_variance <- cumsum(explained_variance)
lines(cumulative_variance, type = "b", col = "red")
legend("topright", legend = c("Proportion", "Cumulative"), col = c("black", "red"), lty = 1)

