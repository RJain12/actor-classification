library(dplyr)
library(caret)
library(tidyr)
library(markdown)
library(pROC)
library(ggplot2)
library(stats)
library(car)
library(lmtest)

continent_matrix <- matrix(ncol = 3, nrow = nrow(na_renamed_columns_data))
colnames(continent_matrix) <- c("Continent", "AI_Count", "Human_Count")

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
  continent_matrix[row_num, ] <- c(row_num, ai_count, human_count)
  
  # Update the total count
  ai_correct_count <- ai_correct_count + ai_count
  human_correct_count <- human_correct_count + human_count
}
continent_matrix <- as.data.frame(continent_matrix)
continent_matrix$Continent <- na_renamed_columns_data[, 5]

ai_continent_result <- aov(AI_Count ~ Continent, data = continent_matrix)
summary(ai_continent_result)

human_gender_result <- aov(Human_Count ~ Continent, data = continent_matrix)
summary(human_gender_result)


