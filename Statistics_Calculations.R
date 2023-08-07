library(dplyr)
library(caret)
library(tidyr)
library(markdown)
# Count the number of responses with AI -> AI
ai_correct_count <- 0

for (i in seq_along(renamed_columns_data)) {
  if (startsWith(names(renamed_columns_data)[i], "AI")) {
    ai_correct_count <- ai_correct_count + sum(renamed_columns_data[[i]] == "AI")
  }
}
print(ai_correct_count)

# Count the number of incorrect responses with AI -> Human
ai_incorrect_count <- 0

for (i in seq_along(renamed_columns_data)) {
  if (startsWith(names(renamed_columns_data)[i], "AI")) {
    ai_incorrect_count <- ai_incorrect_count + sum(renamed_columns_data[[i]] == "Human")
  }
}
print(ai_incorrect_count)

total_aivalues_count <- 0

for (i in seq_along(renamed_columns_data)) {
  if (startsWith(names(renamed_columns_data)[i], "AI")) {
    total_aivalues_count <- total_aivalues_count + sum(renamed_columns_data[[i]] == "AI" | renamed_columns_data[[i]] == "Human")
  }
}
print(total_aivalues_count)

human_correct_count <- 0

for (i in seq_along(renamed_columns_data)) {
  if (startsWith(names(renamed_columns_data)[i], "Human")) {
    human_correct_count <- human_correct_count + sum(renamed_columns_data[[i]] == "Human")
  }
}
print(human_correct_count)

# Count the number of incorrect responses with AI -> Human
human_incorrect_count <- 0

for (i in seq_along(renamed_columns_data)) {
  if (startsWith(names(renamed_columns_data)[i], "AI")) {
    human_incorrect_count <- human_incorrect_count + sum(renamed_columns_data[[i]] == "Human")
  }
}
print(human_incorrect_count)

total_humanvalues_count <- 0

for (i in seq_along(renamed_columns_data)) {
  if (startsWith(names(renamed_columns_data)[i], "AI")) {
    total_humanvalues_count <- total_humanvalues_count + sum(renamed_columns_data[[i]] == "AI" | renamed_columns_data[[i]] == "Human")
  }
}
print(total_humanvalues_count)

AI_Human_Counts <- data.frame(
  Response = c("AI response", "Human response"),
  "AI actual" = c(NA, NA),
  "Human actual" = c(NA, NA)
)


AI_Human_Counts[1, 2] <- ai_correct_count
AI_Human_Counts[1, 3] <- human_incorrect_count
AI_Human_Counts[2, 2] <- ai_incorrect_count
AI_Human_Counts[2, 3] <- human_correct_count

AI_Human_ConfusionMatrix <- data.frame(
  row.names = c("Predicted: AI", "Predicted: Human"),
  `Actual: AI` = c(TP = 943, FN = 819),
  `Actual: Human` = c(FP = 819, TN = 660)
)
