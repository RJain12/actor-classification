library(dplyr)
library(caret)
library(tidyr)
library(markdown)
library(pROC)
library(ggplot2)
library(stats)
library(car)
library(lmtest)

gender_matrix$AI_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "AI" in the row
  ai_count <- sum(grepl("AI", coded_renamed_columns_data[i, ]))
  
  # Store the count in gender_matrix$AI_Count
  gender_matrix$AI_Count[i] <- ai_count
}

gender_matrix$Human_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "Human" in the row
  human_count <- sum(grepl("Human", coded_renamed_columns_data[i, ]))
  
  # Store the count in gender_matrix$Human_Count
  gender_matrix$Human_Count[i] <- human_count
}


gender_matrix <- matrix(ncol = 3, nrow = nrow(na_renamed_columns_data))
colnames(gender_matrix) <- c("Gender", "AI_Count", "Human_Count")

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
  gender_matrix[row_num, ] <- c(row_num, ai_count, human_count)
  
  # Update the total count
  ai_correct_count <- ai_correct_count + ai_count
  human_correct_count <- human_correct_count + human_count
}
gender_matrix <- as.data.frame(gender_matrix)
gender_matrix$Gender <- na_renamed_columns_data[, 3]

ai_gender_result <- aov(AI_Count ~ Gender, data = gender_matrix)
summary(ai_gender_result)

human_gender_result <- aov(Human_Count ~ Gender, data = gender_matrix)
summary(human_gender_result)


