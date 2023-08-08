library(dplyr)
library(caret)
library(tidyr)
library(markdown)
library(pROC)
library(ggplot2)

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

AI_Human_ConfusionMatrix_Numeric <- data.frame(
  row.names = c("Predicted: AI", "Predicted: Human"),
  `Actual: AI` = c(TP = 943, FN = 819),
  `Actual: Human` = c(FP = 819, TN = 660)
)

write.csv(AI_Human_ConfusionMatrix_Numeric, file = "tables/AI_Human_ConfusionMatrix_Numeric.csv", row.names = TRUE)


AI_Human_ConfusionMatrix_Percentage <- data.frame(
  row.names = c("Predicted: AI", "Predicted: Human"),
  `Actual: AI` = c(TP = "53.52%", FN = "55.38%"),
  `Actual: Human` = c(FP = "46.48%", TN = "44.62%")
)

write.csv(AI_Human_ConfusionMatrix_Percentage, file = "tables/AI_Human_ConfusionMatrix_Percentage.csv", row.names = TRUE)


ai_selected_columns <- renamed_columns_data[, grepl("^AI", names(renamed_columns_data))]
ai_selected_columns_csv <- paste(names(ai_selected_columns), collapse = ",")
count_ai_values_correct <- sum(sapply(ai_selected_columns, function(col) sum(grepl("AI", col))))
count_ai_values_incorrect <- sum(sapply(ai_selected_columns, function(col) sum(grepl("Human", col))))
human_selected_columns <- renamed_columns_data[, grepl("^Human", names(renamed_columns_data))]
human_selected_columns_csv <- paste(names(human_selected_columns), collapse = ",")
count_human_values_correct <- sum(sapply(human_selected_columns, function(col) sum(grepl("Human", col))))
count_human_values_incorrect <- sum(sapply(human_selected_columns, function(col) sum(grepl("AI", col))))

thorough_rows <- which(apply(renamed_columns_data, 1, function(row) any(grepl("I have used this, and thoroughly understand how it works", row))))
rough_rows <- which(apply(renamed_columns_data, 1, function(row) any(grepl("I have used this, and roughly understand how it works", row))))
lack_rows <- which(apply(renamed_columns_data, 1, function(row) any(grepl("I have used this before, but lack an understanding of how it works", row))))
heard_rows <- which(apply(renamed_columns_data, 1, function(row) any(grepl("I have heard of this before, but have not used it", row))))
notheard_rows <- which(apply(renamed_columns_data, 1, function(row) any(grepl("I have not heard of this before", row))))

thorough_ai_all_count <- sum(rowSums(renamed_columns_data[thorough_rows, ] == "AI", na.rm = TRUE))
rough_ai_all_count <- sum(rowSums(renamed_columns_data[rough_rows, ] == "AI", na.rm = TRUE))
lack_ai_all_count <- sum(rowSums(renamed_columns_data[lack_rows, ] == "AI", na.rm = TRUE))
heard_ai_all_count <- sum(rowSums(renamed_columns_data[heard_rows, ] == "AI", na.rm = TRUE))
notheard_ai_all_count <- sum(rowSums(renamed_columns_data[notheard_rows, ] == "AI", na.rm = TRUE))

thorough_human_all_count <- sum(rowSums(renamed_columns_data[thorough_rows, ] == "Human", na.rm = TRUE))
rough_human_all_count <- sum(rowSums(renamed_columns_data[rough_rows, ] == "Human", na.rm = TRUE))
lack_human_all_count <- sum(rowSums(renamed_columns_data[lack_rows, ] == "Human", na.rm = TRUE))
heard_human_all_count <- sum(rowSums(renamed_columns_data[heard_rows, ] == "Human", na.rm = TRUE))
notheard_human_all_count <- sum(rowSums(renamed_columns_data[notheard_rows, ] == "Human", na.rm = TRUE))


thorough_ai_correct_count <- sum(rowSums(renamed_columns_data[thorough_rows, !(names(renamed_columns_data) %in% unlist(strsplit(ai_selected_columns_csv, ","))) ] == "AI", na.rm = TRUE))
rough_ai_correct_count <- sum(rowSums(renamed_columns_data[rough_rows, !(names(renamed_columns_data) %in% unlist(strsplit(ai_selected_columns_csv, ","))) ] == "AI", na.rm = TRUE))
lack_ai_correct_count <- sum(rowSums(renamed_columns_data[lack_rows, !(names(renamed_columns_data) %in% unlist(strsplit(ai_selected_columns_csv, ","))) ] == "AI", na.rm = TRUE))
heard_ai_correct_count <- sum(rowSums(renamed_columns_data[heard_rows, !(names(renamed_columns_data) %in% unlist(strsplit(ai_selected_columns_csv, ","))) ] == "AI", na.rm = TRUE))
notheard_ai_correct_count <- sum(rowSums(renamed_columns_data[notheard_rows, !(names(renamed_columns_data) %in% unlist(strsplit(ai_selected_columns_csv, ","))) ] == "AI", na.rm = TRUE))



# Contingency Table
ai_columns <- grep("^AI", names(renamed_columns_data))
ai_counts <- sapply(renamed_columns_data[, ai_columns], function(col) sum(col == "AI"))
ai_row_numbers <- which(apply(renamed_columns_data[, ai_columns], 1, function(row) any(row == "AI")))
ai_correct_thorough <- sum(apply(renamed_columns_data[ai_row_numbers, ], 1, function(row) any(row == "I have used this, and thoroughly understand how it works")))
human_columns <- grep("^Human", names(renamed_columns_data))
human_counts <- sapply(renamed_columns_data[, human_columns], function(col) sum(col == "Human"))
human_row_numbers <- which(apply(renamed_columns_data[, human_columns], 1, function(row) any(row == "Human")))
human_correct_thorough <- sum(apply(renamed_columns_data[human_row_numbers, ], 1, function(row) any(row == "I have used this, and thoroughly understand how it works")))
ai_correct_rough <- sum(apply(renamed_columns_data[ai_row_numbers, ], 1, function(row) any(row == "I have used this, and roughly understand how it works")))
human_correct_rough <- sum(apply(renamed_columns_data[human_row_numbers, ], 1, function(row) any(row == "I have used this, and roughly understand how it works")))
ai_correct_lack <- sum(apply(renamed_columns_data[ai_row_numbers, ], 1, function(row) any(row == "I have used this before, but lack an understanding of how it works")))
human_correct_lack <- sum(apply(renamed_columns_data[human_row_numbers, ], 1, function(row) any(row == "I have used this before, but lack an understanding of how it works")))
ai_correct_heard <- sum(apply(renamed_columns_data[ai_row_numbers, ], 1, function(row) any(row == "I have heard of this before, but have not used it")))
human_correct_heard <- sum(apply(renamed_columns_data[human_row_numbers, ], 1, function(row) any(row == "I have heard of this before, but have not used it")))
ai_correct_notheard <- sum(apply(renamed_columns_data[ai_row_numbers, ], 1, function(row) any(row == "I have not heard of this before")))
human_correct_notheard <- sum(apply(renamed_columns_data[human_row_numbers, ], 1, function(row) any(row == "I have not heard of this before")))

ai_human_rows <- which(apply(renamed_columns_data, 1, function(row) any(row == "I have used this, and thoroughly understand how it works")))
ai_human_counts <- sum(apply(renamed_columns_data[ai_human_rows, ], 1, function(row) any(row %in% c("AI", "Human"))))
ai_correct_thorough_percent <- paste0(round((ai_correct_thorough / ai_human_counts) * 100), "%")
human_correct_thorough_percent <- paste0(round((human_correct_thorough / ai_human_counts) * 100), "%")

ai_human_rows <- which(apply(renamed_columns_data, 1, function(row) any(row == "I have used this, and roughly understand how it works")))
ai_human_counts <- sum(apply(renamed_columns_data[ai_human_rows, ], 1, function(row) any(row %in% c("AI", "Human"))))
ai_correct_rough_percent <- paste0(round((ai_correct_rough / ai_human_counts) * 100), "%")
human_correct_rough_percent <- paste0(round((human_correct_rough / ai_human_counts) * 100), "%")

ai_human_rows <- which(apply(renamed_columns_data, 1, function(row) any(row == "I have used this before, but lack an understanding of how it works")))
ai_human_counts <- sum(apply(renamed_columns_data[ai_human_rows, ], 1, function(row) any(row %in% c("AI", "Human"))))
ai_correct_lack_percent <- paste0(round((ai_correct_lack / ai_human_counts) * 100), "%")
human_correct_lack_percent <- paste0(round((human_correct_lack / ai_human_counts) * 100), "%")

ai_human_rows <- which(apply(renamed_columns_data, 1, function(row) any(row == "I have heard of this before, but have not used it")))
ai_human_counts <- sum(apply(renamed_columns_data[ai_human_rows, ], 1, function(row) any(row %in% c("AI", "Human"))))
ai_correct_heard_percent <- paste0(round((ai_correct_heard / ai_human_counts) * 100), "%")
human_correct_heard_percent <- paste0(round((human_correct_heard / ai_human_counts) * 100), "%")

ai_human_rows <- which(apply(renamed_columns_data, 1, function(row) any(row == "I have not heard of this before")))
ai_human_counts <- sum(apply(renamed_columns_data[ai_human_rows, ], 1, function(row) any(row %in% c("AI", "Human"))))
ai_correct_notheard_percent <- paste0(round((ai_correct_notheard / ai_human_counts) * 100), "%")
human_correct_notheard_percent <- paste0(round((human_correct_notheard / ai_human_counts) * 100), "%")


contingency_table_percent <- data.frame(
  Experience = c("Used & Thoroughly Understand", "Used & Roughly Understand", "Used & Lack Understanding", "Heard of & Not Used", "Not Heard of"),
  AI_Correct = c(ai_correct_thorough_percent, ai_correct_rough_percent, ai_correct_lack_percent, ai_correct_heard_percent, ai_correct_notheard_percent),
  Human_Correct = c(human_correct_thorough_percent, human_correct_rough_percent, human_correct_lack_percent, human_correct_heard_percent, human_correct_notheard_percent)
)

ggplot(contingency_table_percent, aes(x = Experience, y = AI_Correct, fill = Experience)) +
  geom_bar(stat = "identity") +
  labs(title = "Correct AI Classifications by Experience Level",
       y = "Percentage of Correct AI Classifications",
       x = "Experience with AI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))