library(dplyr)
library(caret)
library(tidyr)
library(markdown)
library(pROC)
library(ggplot2)
library(stats)
library(car)
library(lmtest)
result_matrix$AI_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "AI" in the row
  ai_count <- sum(grepl("AI", coded_renamed_columns_data[i, ]))
  
  # Store the count in result_matrix$AI_Count
  result_matrix$AI_Count[i] <- ai_count
}

result_matrix$Human_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "Human" in the row
  human_count <- sum(grepl("Human", coded_renamed_columns_data[i, ]))
  
  # Store the count in result_matrix$Human_Count
  result_matrix$Human_Count[i] <- human_count
}


na_renamed_columns_data <- renamed_columns_data

na_renamed_columns_data[, 1] <- as.numeric(na_renamed_columns_data[, 1])
na_renamed_columns_data <- na_renamed_columns_data[complete.cases(na_renamed_columns_data), ]

result_matrix <- matrix(ncol = 3, nrow = nrow(na_renamed_columns_data))
colnames(result_matrix) <- c("Age", "AI Count", "Human Count")

# Initialize a count variable
ai_correct_count <- 0
human_correct_count <- 0

# Loop through each row of the dataset
for (row_num in 1:nrow(na_renamed_columns_data)) {
  ai_count <- 0
  human_count <- 0
  
  # Loop through columns that start with "AI"/"Human"
  for (i in seq_along(na_renamed_columns_data)) {
    if (startsWith(names(na_renamed_columns_data)[i], "AI")) {
      ai_count <- ai_count + (na_renamed_columns_data[row_num, i] == "AI")
    }
  }
  for (i in seq_along(na_renamed_columns_data)) {
    if (startsWith(names(na_renamed_columns_data)[i], "Human")) {
      human_count <- human_count + (na_renamed_columns_data[row_num, i] == "Human")
    }
  }
  
  
  # Update the result matrix
  result_matrix[row_num, ] <- c(row_num, ai_count, human_count)
  
  # Update the total count
  ai_correct_count <- ai_correct_count + ai_count
  human_correct_count <- human_correct_count + human_count
}
result_matrix <- as.data.frame(result_matrix)
result_matrix$Age <- na_renamed_columns_data[, 1]

# ANCOVA 

ancova_model <- lm(result_matrix[, 2] ~ result_matrix[, 1])  # Fit ANCOVA model
ai_anova_result <- Anova(ancova_model, type = "III")  # Conduct ANCOVA with Type III sum of squares
summary(ai_anova_result)  # Display ANCOVA summary
sum_sq <- c(6.053, 66.035, 126.018, 361.545, 539.292, 952.565)
df <- c(1.0, 1.0, 1.0, 112.7, 168.5, 336.0)
f_value <- c(2.135, 12.714, 23.293, 23.293, 33.872, 44.450)
pr_f <- c(0.00000, 0.03623, 0.07245, 0.07245, 0.10868, 0.14490)

ai_ancova_summary <- data.frame(Sum.Sq = sum_sq, Df = df, F.value = f_value, Pr_gt_F = pr_f)
ai_ancova_summary <- cbind(Desc_Stats = c(min(result_matrix$Age), quantile(result_matrix$Age, c(0.25, 0.5, 0.75)), mean(result_matrix$Age), max(result_matrix$Age)), ai_ancova_summary)
ai_ancova_summary <- cbind(c("Min", "1st Q", "Median", "Mean", "3rd Q", "Max"), ai_ancova_summary)
colnames(ai_ancova_summary)[1] <- "Stat"

ancova_model <- lm(result_matrix[, 3] ~ result_matrix[, 1])  # Fit ANCOVA model
human_anova_result <- Anova(ancova_model, type = "III")  # Conduct ANCOVA with Type III sum of squares
summary(human_anova_result)  # Display ANCOVA summary
sum_sq <- c(2.268, 59.736, 117.204, 327.928, 490.758, 864.312)
df <- c(1.0, 1.0, 1.0, 112.7, 168.5, 336.0)
f_value <- c(0.8817, 12.0520, 23.2222, 23.2222, 34.3925, 45.5628)
pr_f <- c(0.0000, 0.0871, 0.1742, 0.1742, 0.2613, 0.3484)

human_ancova_summary <- data.frame(Sum.Sq = sum_sq, Df = df, F.value = f_value, Pr_gt_F = pr_f)
human_ancova_summary <- cbind(Desc_Stats = c(min(result_matrix$Age), quantile(result_matrix$Age, c(0.25, 0.5, 0.75)), mean(result_matrix$Age), max(result_matrix$Age)), human_ancova_summary)
human_ancova_summary <- cbind(c("Min", "1st Q", "Median", "Mean", "3rd Q", "Max"), human_ancova_summary)
colnames(human_ancova_summary)[1] <- "Stat"

