# Load necessary library
library(dplyr)

# Load the data
data <- read.csv("mental_health.csv")

# 1. Ensure valid and unique column names
colnames(data) <- make.names(colnames(data), unique = TRUE)

# Display column names to verify exact names
print(colnames(data))

# 2. Change column names to shorter versions
colnames(data) <- c("timestamp", "age", "gender", "kulliyyah", "year_sem", 
                    "credit_hours", "courses", "study_hours", "stress_lvl", 
                    "sleep_hours", "rested_freq", "socialize_freq", "lonely", "acad_perf", 
                    "miss_deadlines", "mh_awareness", "mh_usage", "phone_usage", 
                    "exercise_freq", "social_activities", "extracurricular")

# 3. Handle missing values
# Remove rows with too many missing values (set threshold as needed)
threshold <- 5
data <- data[rowSums(is.na(data)) <= threshold, ]

# Impute remaining missing values with median for numeric columns
data <- data %>%
  mutate(
    age = ifelse(is.na(age), median(age, na.rm = TRUE), age),
    study_hours = ifelse(is.na(study_hours), median(study_hours, na.rm = TRUE), study_hours),
    sleep_hours = ifelse(is.na(sleep_hours), median(sleep_hours, na.rm = TRUE), sleep_hours),
    phone_usage = ifelse(is.na(phone_usage), median(phone_usage, na.rm = TRUE), phone_usage),
    exercise_freq = ifelse(is.na(exercise_freq), median(exercise_freq, na.rm = TRUE), exercise_freq)
  )

# 4. Convert range columns to numeric (age, study_hours, sleep_hours, socialize_freq, phone_usage, exercise_freq)
data <- data %>%
  mutate(
    # Handle age with ranges and exact numbers
    age = case_when(
      grepl("18 - 20", age) ~ 19,          # Midpoint of 18-20 range
      grepl("21 - 23", age) ~ 22,          # Midpoint of 21-23 range
      grepl("24 - 26", age) ~ 25,          # Midpoint of 24-26 range
      grepl("27 - 29", age) ~ 28,          # Midpoint of 27-29 range
      grepl("^\\d+$", age) ~ as.numeric(age),  # Exact age as numeric
      TRUE ~ NA_real_
    ),
    study_hours = case_when(
      study_hours == "Less than 2 hours" ~ 1,
      study_hours == "2-4 hours" ~ 3,
      study_hours == "4-6 hours" ~ 5,
      study_hours == "more than 6 hours" ~ 7,
      TRUE ~ NA_real_
    ),
    sleep_hours = case_when(
      sleep_hours == "Less than 4 hours" ~ 3,
      sleep_hours == "4-6 hours" ~ 5,
      sleep_hours == "6-8 hours" ~ 7,
      sleep_hours == "More than 8 hours" ~ 9,
      TRUE ~ NA_real_
    ),
    socialize_freq = case_when(
      socialize_freq == "1-2 times" ~ 1,
      socialize_freq == "3-5 times" ~ 3,
      socialize_freq == "More than 5 times" ~ 5,
      TRUE ~ NA_real_
    ),
    phone_usage = case_when(
      phone_usage == "Less than 4 hours" ~ 2,
      phone_usage == "2-4 hours" ~ 3,
      phone_usage == "4-6 hours" ~ 5,
      phone_usage == "6-8 hours" ~ 7,
      phone_usage == "8-10 hours" ~ 9,
      phone_usage == "More than 10 hours" ~ 11,
      TRUE ~ NA_real_
    ),
    exercise_freq = case_when(
      exercise_freq == "Never" ~ 1,
      exercise_freq == "1-2 times" ~ 2,
      exercise_freq == "3-5 times" ~ 4,
      exercise_freq == "More than 5 times" ~ 5,
      TRUE ~ NA_real_
    )
  )

# 5. Convert linear scale columns to 1-5 (rested frequency, missed deadlines)
data <- data %>%
  mutate(
    rested_freq = case_when(
      rested_freq == "Never" ~ 1,
      rested_freq == "Rarely" ~ 2,
      rested_freq == "Sometimes" ~ 3,
      rested_freq == "Often" ~ 4,
      rested_freq == "Always" ~ 5,
      TRUE ~ NA_real_
    ),
    miss_deadlines = case_when(
      miss_deadlines == "Never" ~ 1,
      miss_deadlines == "Rarely" ~ 2,
      miss_deadlines == "Sometimes" ~ 3,
      miss_deadlines == "Often" ~ 4,
      miss_deadlines == "Always" ~ 5,
      TRUE ~ NA_real_
    )
  )

# 6. Encode categorical variables as factors
data <- data %>%
  mutate(
    gender = factor(gender),
    kulliyyah = factor(kulliyyah),
    year_sem = factor(year_sem)
  )

# 7. Feature engineering: Create stress index
data <- data %>%
  mutate(
    stress_index = 0.4 * stress_lvl + 0.3 * miss_deadlines + 0.3 * (5 - rested_freq)
  )

# 8. Remove duplicate rows
data <- data[!duplicated(data), ]

# 9. Ensure consistency in categorical variables (trim spaces, convert to lowercase)
data <- data %>%
  mutate(
    gender = tolower(trimws(gender)),
    kulliyyah = tolower(trimws(kulliyyah))
  )

# View cleaned and preprocessed data
head(data)

# 10. Descriptive Statistical Analysis
# Summary statistics for numeric columns
summary_stats <- data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    
    mean_study_hours = mean(study_hours, na.rm = TRUE),
    median_study_hours = median(study_hours, na.rm = TRUE),
    sd_study_hours = sd(study_hours, na.rm = TRUE),
    
    mean_sleep_hours = mean(sleep_hours, na.rm = TRUE),
    median_sleep_hours = median(sleep_hours, na.rm = TRUE),
    sd_sleep_hours = sd(sleep_hours, na.rm = TRUE),
    
    mean_phone_usage = mean(phone_usage, na.rm = TRUE),
    median_phone_usage = median(phone_usage, na.rm = TRUE),
    sd_phone_usage = sd(phone_usage, na.rm = TRUE),
    
    mean_exercise_freq = mean(exercise_freq, na.rm = TRUE),
    median_exercise_freq = median(exercise_freq, na.rm = TRUE),
    sd_exercise_freq = sd(exercise_freq, na.rm = TRUE)
  )

print("Summary statistics:")
print(summary_stats)

# Identify outliers using the IQR method
outliers <- data %>%
  summarise(
    lower_bound_age = quantile(age, 0.25) - 1.5 * IQR(age),
    upper_bound_age = quantile(age, 0.75) + 1.5 * IQR(age),
    
    lower_bound_study_hours = quantile(study_hours, 0.25) - 1.5 * IQR(study_hours),
    upper_bound_study_hours = quantile(study_hours, 0.75) + 1.5 * IQR(study_hours),
    
    lower_bound_sleep_hours = quantile(sleep_hours, 0.25) - 1.5 * IQR(sleep_hours),
    upper_bound_sleep_hours = quantile(sleep_hours, 0.75) + 1.5 * IQR(sleep_hours)
  )

print("Outliers thresholds:")
print(outliers)

# Handle outliers by capping them at lower and upper bounds
data <- data %>%
  mutate(
    age = ifelse(age < outliers$lower_bound_age, outliers$lower_bound_age,
                 ifelse(age > outliers$upper_bound_age, outliers$upper_bound_age, age)),
    study_hours = ifelse(study_hours < outliers$lower_bound_study_hours, outliers$lower_bound_study_hours,
                         ifelse(study_hours > outliers$upper_bound_study_hours, outliers$upper_bound_study_hours, study_hours)),
    sleep_hours = ifelse(sleep_hours < outliers$lower_bound_sleep_hours, outliers$lower_bound_sleep_hours,
                         ifelse(sleep_hours > outliers$upper_bound_sleep_hours, outliers$upper_bound_sleep_hours, sleep_hours))
  )

# Verify the cleaned data
print("Cleaned data preview:")
head(data)

# 12. Create the stress index and risk status column before splitting
data <- data %>%
  mutate(
    stress_index = 0.4 * stress_lvl + 0.3 * miss_deadlines + 0.3 * (5 - rested_freq),
    risk_status = ifelse(stress_index >= median(stress_index, na.rm = TRUE), "At Risk", "Not At Risk")
  )

# Ensure target variable is a factor
data$risk_status <- factor(data$risk_status)

# 11. Data Splitting and Model Validation
# Split data into training (60%) and testing (40%) sets
set.seed(123)  # Ensure reproducibility
sample_index <- sample(1:nrow(data), size = 0.6 * nrow(data))

train_set <- data[sample_index, ]
test_set <- data[-sample_index, ]

# Check the size of training and testing sets
cat("Training set size:", nrow(train_set), "\n")
cat("Testing set size:", nrow(test_set), "\n")


# 12. Model Development
# Convert stress_index into a binary target variable: "At Risk" or "Not At Risk"
data <- data %>%
  mutate(
    risk_status = ifelse(stress_index >= median(stress_index, na.rm = TRUE), "At Risk", "Not At Risk")
  )

# Ensure target variable is a factor
data$risk_status <- factor(data$risk_status)

# Check class distribution
table(data$risk_status)

# 13. Decision Tree Model
library(rpart)
library(rpart.plot)

# Train a decision tree model
dt_model <- rpart(risk_status ~ study_hours + sleep_hours + 
                    exercise_freq + miss_deadlines + rested_freq, 
                  data = train_set, method = "class")

# Plot the decision tree
rpart.plot(dt_model, main = "Decision Tree for Mental Health Prediction")

# Predict on the test set
dt_predictions <- predict(dt_model, test_set, type = "class")

# Evaluate the model
dt_confusion_matrix <- table(test_set$risk_status, dt_predictions)
dt_accuracy <- sum(diag(dt_confusion_matrix)) / sum(dt_confusion_matrix)
cat("Decision Tree Accuracy:", dt_accuracy, "\n")

# 14. Naïve Bayes Model
library(e1071)

# Train a Naïve Bayes model
nb_model <- naiveBayes(risk_status ~ study_hours + sleep_hours  + 
                         exercise_freq + miss_deadlines + rested_freq, 
                       data = train_set)

# Predict on the test set
nb_predictions <- predict(nb_model, test_set)

# Evaluate the model
nb_confusion_matrix <- table(test_set$risk_status, nb_predictions)
nb_accuracy <- sum(diag(nb_confusion_matrix)) / sum(nb_confusion_matrix)
cat("Naïve Bayes Accuracy:", nb_accuracy, "\n")

# 15. k-Nearest Neighbors (k-NN) Model
library(class)

# Normalize numeric columns for k-NN
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

numeric_columns <- c("study_hours", "sleep_hours", "exercise_freq")
train_set_normalized <- as.data.frame(lapply(train_set[, numeric_columns], normalize))
test_set_normalized <- as.data.frame(lapply(test_set[, numeric_columns], normalize))

# Add target variable
train_set_normalized$risk_status <- train_set$risk_status
test_set_normalized$risk_status <- test_set$risk_status

# Apply k-NN (k = 5)
k <- 5
knn_predictions <- knn(train = train_set_normalized[, -ncol(train_set_normalized)], 
                       test = test_set_normalized[, -ncol(test_set_normalized)], 
                       cl = train_set_normalized$risk_status, k = k)

# Evaluate the model
knn_confusion_matrix <- table(test_set$risk_status, knn_predictions)
knn_accuracy <- sum(diag(knn_confusion_matrix)) / sum(knn_confusion_matrix)
cat("k-NN Accuracy (k =", k, "):", knn_accuracy, "\n")

# 16. Comparative Evaluation of Techniques
# Accuracy for Decision Tree
dt_accuracy <- sum(dt_predictions == test_set$risk_status) / nrow(test_set)

# Accuracy for Naïve Bayes
nb_accuracy <- sum(nb_predictions == test_set$risk_status) / nrow(test_set)

# Accuracy for k-NN
knn_accuracy <- sum(knn_predictions == test_set$risk_status) / nrow(test_set)

# Create results data frame
results <- data.frame(
  Model = c("Decision Tree", "Naïve Bayes", "k-Nearest Neighbors"),
  Accuracy = c(dt_accuracy, nb_accuracy, knn_accuracy)
)

# Print results
print("Comparative Evaluation of Models:")
print(results)
