# Load libraries
library(tidyverse)
library(ggplot2)
library(effsize)

# Load and label data
data <- read.csv("data_sts2.csv", header = FALSE)
colnames(data) <- c("timestamp", "consent", "career_stage", "age", "gender", 
                    "marital_status", "governorate", "workplace", "specialty", 
                    "psychiatric_meds", "med_names", "chronic_disease", 
                    "disease_name", "disease_onset", "hours_worked", 
                    "depression_score", "depression_severity",
                    paste0("scale_", 1:30))

# Convert and clean data
df <- data %>%
  mutate(
    age = as.numeric(age),
    hours_worked = as.numeric(hours_worked),
    depression_score = as.numeric(depression_score),
    gender = factor(gender, levels = c("Male", "Female", "Other")),
    depression_severity = factor(depression_severity,
                                 levels = c("None", "Mild", "Moderate", "Severe"),
                                 ordered = TRUE)
  )

# Show parsing summaries
cat("Summary of key variables:\n")
summary(select(df, age, hours_worked, depression_score))

# Filter out incomplete data
initial_n <- nrow(df)
df <- df %>% filter(!is.na(age), !is.na(depression_score))
cat("Rows removed due to missing age or depression score:", initial_n - nrow(df), "\n")

# Descriptive statistics
cat("Sample size:", nrow(df), "participants\n")
cat("Mean age:", round(mean(df$age), 1), "years\n\n")

cat("Gender distribution:\n")
print(df %>% count(gender) %>% mutate(pct = round(n / sum(n) * 100, 1)))

cat("\nDepression severity distribution:\n")
print(df %>% count(depression_severity) %>% mutate(pct = round(n / sum(n) * 100, 1)))

# Statistical tests
cat("\nT-test: Depression score by gender\n")
print(t.test(depression_score ~ gender, data = df))

cat("\nEffect size (Cohen's d):\n")
print(cohen.d(depression_score ~ gender, data = df))

cat("\nCorrelation: Age vs Depression Score\n")
print(cor.test(df$age, df$depression_score))

cat("\nCorrelation: Hours Worked vs Depression Score\n")
print(cor.test(df$hours_worked, df$depression_score, use = "complete.obs"))

# Visualizations
# 1. Depression Severity by Gender
ggplot(df, aes(depression_severity, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Depression Severity by Gender",
       x = "Depression Severity", y = "Count", fill = "Gender",
       caption = "Data: STS2 Survey") +
  theme_minimal()

# 2. Age vs Depression Score
ggplot(df, aes(age, depression_score, color = gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Age vs Depression Score",
       x = "Age", y = "Depression Score", color = "Gender") +
  theme_minimal()

# Summary table by gender
summary_table <- df %>%
  group_by(gender) %>%
  summarise(
    n = n(),
    mean_age = round(mean(age, na.rm = TRUE), 1),
    mean_depression = round(mean(depression_score, na.rm = TRUE), 1),
    .groups = 'drop'
  )

cat("\nSummary table by gender:\n")
print(summary_table)

# Export summary
write.csv(summary_table, "summary_by_gender.csv", row.names = FALSE)