library(dplyr)
library(caret)
library(tidyr)
library(markdown)
library(pROC)
library(ggplot2)
library(stats)
library(car)
library(lmtest)

education_matrix$AI_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "AI" in the row
  ai_count <- sum(grepl("AI", coded_renamed_columns_data[i, ]))
  
  # Store the count in education_matrix$AI_Count
  education_matrix$AI_Count[i] <- ai_count
}

education_matrix$Human_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "Human" in the row
  human_count <- sum(grepl("Human", coded_renamed_columns_data[i, ]))
  
  # Store the count in education_matrix$Human_Count
  education_matrix$Human_Count[i] <- human_count
}

education_matrix <- matrix(ncol = 3, nrow = nrow(na_renamed_columns_data))
colnames(education_matrix) <- c("Education", "AI_Count", "Human_Count")

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
  education_matrix[row_num, ] <- c(row_num, ai_count, human_count)
  
  # Update the total count
  ai_correct_count <- ai_correct_count + ai_count
  human_correct_count <- human_correct_count + human_count
}
education_matrix <- as.data.frame(education_matrix)
education_matrix$Education <- na_renamed_columns_data[, 4]

ai_education_result <- aov(AI_Count ~ Education, data = education_matrix)
summary(ai_education_result)

human_education_result <- aov(Human_Count ~ Education, data = education_matrix)
summary(human_education_result)


