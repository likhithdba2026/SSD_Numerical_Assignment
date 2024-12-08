dataset <- read.csv('/Users/likhithdabbikar/Desktop/out/Python_and_DATA_Science/datasets/insurance.csv')
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
df_charges=dataset$charges
mean(df_charges)
median(df_charges)
sd(df_charges)
min(df_charges)
max(df_charges)

#3 question
hist(df_charges, breaks = "Sturges", col = "skyblue", main = "Histogram of Insurance charges", xlab = "Insurance")


boxplot(df_charges,
        main = "Box Plot of Insurance charges",
        ylab = "charges",
        col = "lightgreen",
        border = "darkgreen",
        notch = TRUE)
# Adding a grid for clarity
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

#4 question


# Create a sample dataset
df_sex <- dataset$sex

# Summarize the counts of each category
species_counts <- table(df_sex)

# Create a bar plot and save bar midpoints
bar_midpoints <- barplot(
  species_counts,
  main = "Count of Each Species in the Dataset",
  xlab = "Sex",
  ylab = "Count",
  col = "skyblue",
  ylim = c(0, max(species_counts) + 5)  # Adjust ylim for label space
)

# Add the frequency counts as labels inside the bars
text(
  x = bar_midpoints,                   # X-coordinates (bar midpoints)
  y = species_counts / 2,              # Y-coordinates (middle of the bars)
  labels = species_counts,             # Text labels (frequency counts)
  cex = 0.8,                           # Text size
  col = "white"                        # Text color (contrasts with bar color)
)



#5 question
df_bmi =dataset$bmi
cor(df_charges,df_bmi)

# highly uncorrelated

#6 question
df_age = dataset$age

# Plot the scatter plot
plot(df_bmi ,df_charges,   # Note the order: x (independent) first, then y (dependent)
     main = "Scatter Plot with Trend Line",
     xlab = "bmi",
     ylab = "charges",
     col = "blue",
     pch = 19)

# Fit a linear model
model <- lm(df_charges ~ df_bmi)  # Correct formula: y ~ x

# Add the trend line
abline(model, col = "red", lwd = 2) 

# Taking the labeling data petal length
df_age = dataset$age

model <- lm(dataset$charges ~ dataset$age + dataset$bmi + dataset$children)

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


numerical_data <- dataset[, c(1,3,4,7)]
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



