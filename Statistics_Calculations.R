library(dplyr)
library(caret)
library(tidyr)
library(markdown)
library(pROC)
library(ggplot2)
library(stats)

# Count the number of responses with AI -> AI
ai_correct_count <- 0

for (i in seq_along(renamed_columns_data)) {
  if (startsWith(names(renamed_columns_data)[i], "AI")) {
    ai_correct_count <- ai_correct_count + sum(renamed_columns_data[[i]][as.integer(rownames(renamed_columns_data)) >= 0 & as.integer(rownames(renamed_columns_data)) <= 99] == "AI")
  }
}


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

# Isolating for AI Experience & Contingency Table 

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


ai_selected_columns <- strsplit(ai_selected_columns_csv, ",")[[1]]

thorough_ai_correct_count <- sum(rowSums(renamed_columns_data[thorough_rows, ai_selected_columns] == "AI", na.rm = TRUE))
rough_ai_correct_count <- sum(rowSums(renamed_columns_data[rough_rows, ai_selected_columns] == "AI", na.rm = TRUE))
lack_ai_correct_count <- sum(rowSums(renamed_columns_data[lack_rows, ai_selected_columns] == "AI", na.rm = TRUE))
heard_ai_correct_count <- sum(rowSums(renamed_columns_data[heard_rows, ai_selected_columns] == "AI", na.rm = TRUE))
notheard_ai_correct_count <- sum(rowSums(renamed_columns_data[notheard_rows, ai_selected_columns] == "AI", na.rm = TRUE))


human_selected_columns <- strsplit(human_selected_columns_csv, ",")[[1]]

thorough_human_correct_count <- sum(rowSums(renamed_columns_data[thorough_rows, human_selected_columns] == "Human", na.rm = TRUE))
rough_human_correct_count <- sum(rowSums(renamed_columns_data[rough_rows, human_selected_columns] == "Human", na.rm = TRUE))
lack_human_correct_count <- sum(rowSums(renamed_columns_data[lack_rows, human_selected_columns] == "Human", na.rm = TRUE))
heard_human_correct_count <- sum(rowSums(renamed_columns_data[heard_rows, human_selected_columns] == "Human", na.rm = TRUE))
notheard_human_correct_count <- sum(rowSums(renamed_columns_data[notheard_rows, human_selected_columns] == "Human", na.rm = TRUE))


# Contingency Table
ai_correct_thorough_percent <- paste0(round((thorough_ai_correct_count / thorough_ai_all_count) * 100), "%")
human_correct_thorough_percent <- paste0(round((thorough_human_correct_count / thorough_human_all_count) * 100), "%")

ai_correct_rough_percent <- paste0(round((rough_ai_correct_count / rough_ai_all_count) * 100), "%")
human_correct_rough_percent <- paste0(round((rough_human_correct_count / rough_human_all_count) * 100), "%")

ai_correct_lack_percent <- paste0(round((lack_ai_correct_count / lack_ai_all_count) * 100), "%")
human_correct_lack_percent <- paste0(round((lack_human_correct_count / lack_human_all_count) * 100), "%")

ai_correct_heard_percent <- paste0(round((heard_ai_correct_count / heard_ai_all_count) * 100), "%")
human_correct_heard_percent <- paste0(round((heard_human_correct_count / heard_human_all_count) * 100), "%")

# ai_correct_notheard_percent <- paste0(round((notheard_ai_correct_count / notheard_ai_all_count) * 100), "%")
# human_correct_notheard_percent <- paste0(round((notheard_human_correct_count / notheard_human_all_count) * 100), "%")


contingency_table_percent <- data.frame(
  Experience = c("Used & Thoroughly Understand", "Used & Roughly Understand", "Used & Lack Understanding", "Heard of & Not Used", "Not Heard of"),
  AICorrect = c(ai_correct_thorough_percent, ai_correct_rough_percent, ai_correct_lack_percent, ai_correct_heard_percent, ai_correct_notheard_percent),
  HumanCorrect = c(human_correct_thorough_percent, human_correct_rough_percent, human_correct_lack_percent, human_correct_heard_percent, human_correct_notheard_percent)
)

contingency_table_percent_ready <- contingency_table_percent

contingency_table_percent_ready[] <- lapply(contingency_table_percent_ready, function(x) gsub("%", "\\%", x, fixed = TRUE))
contingency_table_percent_ready$Experience <- gsub("&", "\\\\&", contingency_table_percent_ready$Experience)
contingency_table_numerical <- data.frame(
  Experience = c("Used & Thoroughly Understand", "Used & Roughly Understand", "Used & Lack Understanding", "Heard of & Not Used"),
  AI_Correct = c(thorough_ai_correct_count, rough_ai_correct_count, lack_ai_correct_count, heard_ai_correct_count),
  Human_Correct = c(thorough_human_correct_count, rough_human_correct_count, lack_human_correct_count, heard_human_correct_count)
)


write.csv(contingency_table_percent, file = "tables/AI_Classification_Experience_Contingency_Table_Percentage.csv", row.names = TRUE)
write.csv(contingency_table_numerical, file = "tables/AI_Classification_Experience_Contingency_Table_Numeric.csv", row.names = TRUE)

tikz("tikz/AIvsExperience", width = 6, height = 4, standAlone = TRUE)
ggplot(contingency_table_percent_ready, aes(x = Experience, y = AICorrect, fill = Experience)) +
  geom_bar(stat = "identity") +
  labs(title = "Correct AI Classifications by Experience Level (AI)",
       y = "Percentage of Correct Classifications",
       x = "Experience with AI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# Chi-Squared Test
total_ai_correct <- sum(contingency_table_numerical$AI_Correct)
total_human_correct <- sum(contingency_table_numerical$Human_Correct)
total_experiences <- sum(total_ai_correct, total_human_correct)

# Calculate expected frequencies
expected_ai_correct <- (contingency_table_numerical$AI_Correct + contingency_table_numerical$Human_Correct) * total_ai_correct / total_experiences
expected_human_correct <- (contingency_table_numerical$AI_Correct + contingency_table_numerical$Human_Correct) * total_human_correct / total_experiences

# Calculate the chi-squared statistic
chi_squared <- sum((contingency_table_numerical$AI_Correct - expected_ai_correct)^2 / expected_ai_correct) +
  sum((contingency_table_numerical$Human_Correct - expected_human_correct)^2 / expected_human_correct)

df <- (nrow(contingency_table_numerical) - 1) * (ncol(contingency_table_numerical) - 1)

# Find the p-value
p_value <- 1 - pchisq(chi_squared, df)

alpha <- 0.05

# Compare p-value to significance level
if (p_value < alpha) {
  result <- "Reject null hypothesis: There is a significant association between experience and correct classification."
} else {
  result <- "Fail to reject null hypothesis: No significant association between experience and correct classification."
}

# Print results
print(paste("Chi-Squared Statistic:", chi_squared))
print(paste("Degrees of Freedom:", df))
print(paste("P-value:", p_value))
print(result)

# Chi Square for ONLY  heard of & thoroughly
contingency_table_numerical_thorough_notheard <- data.frame(
  Experience = c("Used & Thoroughly Understand", "Heard of & Not Used"),
  AI_Correct = c(thorough_ai_correct_count, heard_ai_correct_count),
  Human_Correct = c(thorough_human_correct_count, heard_human_correct_count)
)
total_ai_correct <- sum(contingency_table_numerical_thorough_notheard$AI_Correct)
total_human_correct <- sum(contingency_table_numerical_thorough_notheard$Human_Correct)
total_experiences <- sum(total_ai_correct, total_human_correct)
expected_ai_correct <- (contingency_table_numerical$AI_Correct + contingency_table_numerical$Human_Correct) * total_ai_correct / total_experiences
expected_human_correct <- (contingency_table_numerical$AI_Correct + contingency_table_numerical$Human_Correct) * total_human_correct / total_experiences

chi_squared <- sum((contingency_table_numerical$AI_Correct - expected_ai_correct)^2 / expected_ai_correct) +
  sum((contingency_table_numerical$Human_Correct - expected_human_correct)^2 / expected_human_correct)

df <- (nrow(contingency_table_numerical) - 1) * (ncol(contingency_table_numerical) - 1)

# Find the p-value
p_value <- 1 - pchisq(chi_squared, df)

alpha <- 0.05

# Compare p-value to significance level
if (p_value < alpha) {
  result <- "Reject null hypothesis: There is a significant association between experience and correct classification."
} else {
  result <- "Fail to reject null hypothesis: No significant association between experience and correct classification."
}

# Print results
print(paste("Chi-Squared Statistic:", chi_squared))
print(paste("Degrees of Freedom:", df))
print(paste("P-value:", p_value))
print(result)


# McNemar  
contingency_input_correct <- table(cleaned_full_counts$AI_correct, cleaned_full_counts$Human_correct)
a <- contingency_input_correct[2, 2]
b <- contingency_input_correct[1, 2]
c <- contingency_input_correct[2, 1]
d <- contingency_input_correct[1, 1]
mcnemar_statistic <- ((b - c)^2) / (b + c)

# Degrees of freedom is 1 for McNemar's test
df <- 1

# Calculate p-value using chi-square distribution
p_value <- 1 - pchisq(mcnemar_statistic, df)

# Print McNemar's test statistic and p-value
print(paste("McNemar's Test Statistic:", mcnemar_statistic))
print(paste("Degrees of Freedom:", df))
print(paste("P-value:", p_value))

# ROC CURVE
copy_cleaned_full_counts <- cleaned_full_counts
copy_cleaned_full_counts$AI_percentage_correct <- (copy_cleaned_full_counts$AI_correct / 10) * 100
copy_cleaned_full_counts$Human_percentage_correct <- (copy_cleaned_full_counts$Human_correct / 10) * 100
roc_ai <- roc(copy_cleaned_full_counts$AI_percentage_correct, copy_cleaned_full_counts$AI_input)

# Create an ROC curve for Human
roc_human <- roc(copy_cleaned_full_counts$Human_percentage_correct, copy_cleaned_full_counts$Human_input)

# Plot the ROC curves
plot(roc_ai, col = "blue", main = "ROC Curve (Overall)", sub = "AI vs. Human", col.main = "darkblue", col.sub = "darkblue")
lines(roc_human, col = "red")
legend("bottomright", legend = c("AI", "Human"), col = c("blue", "red"), lty = 1)

# ROC: Heard of but not used

heardofnotused_subset_full <- copy_cleaned_full_counts[copy_cleaned_full_counts$aiexperience == "I have heard of this before, but have not used it", ]
heardofnotused_subset_full$AI_percentage_correct <- (heardofnotused_subset_full$AI_correct / 10) * 100
heardofnotused_subset_full$Human_percentage_correct <- (heardofnotused_subset_full$Human_correct / 10) * 100
roc_ai <- roc(heardofnotused_subset_full$AI_percentage_correct, heardofnotused_subset_full$AI_input)

# Create an ROC curve for Human
roc_human <- roc(heardofnotused_subset_full$Human_percentage_correct, heardofnotused_subset_full$Human_input)

# Plot the ROC curves
plot(roc_ai, col = "blue", main = "ROC: Heard of but not used", sub = "AI vs. Human", col.main = "darkblue", col.sub = "darkblue")
lines(roc_human, col = "red")
legend("bottomright", legend = c("AI", "Human"), col = c("blue", "red"), lty = 1)
# ROC: I have used this, and thoroughly understand how it works

thoroughlyunderstand_subset_full <- copy_cleaned_full_counts[copy_cleaned_full_counts$aiexperience == "I have used this, and thoroughly understand how it works", ]
thoroughlyunderstand_subset_full$AI_percentage_correct <- (thoroughlyunderstand_subset_full$AI_correct / 10) * 100
thoroughlyunderstand_subset_full$Human_percentage_correct <- (thoroughlyunderstand_subset_full$Human_correct / 10) * 100
roc_ai <- roc(thoroughlyunderstand_subset_full$AI_percentage_correct, thoroughlyunderstand_subset_full$AI_input)
roc_human <- roc(thoroughlyunderstand_subset_full$Human_percentage_correct, thoroughlyunderstand_subset_full$Human_input)
plot(roc_ai, col = "blue", main = "ROC: Used and Thoroughly Understand", sub = "AI vs. Human", col.main = "darkblue", col.sub = "darkblue")
lines(roc_human, col = "red")
legend("bottomright", legend = c("AI", "Human"), col = c("blue", "red"), lty = 1)

# ROC: I have used this, and roughly understand how it works

roughly_subset_full <- copy_cleaned_full_counts[copy_cleaned_full_counts$aiexperience == "I have used this, and roughly understand how it works", ]
roughly_subset_full$AI_percentage_correct <- (roughly_subset_full$AI_correct / 10) * 100
roughly_subset_full$Human_percentage_correct <- (roughly_subset_full$Human_correct / 10) * 100
roc_ai <- roc(roughly_subset_full$AI_percentage_correct, roughly_subset_full$AI_input)
roc_human <- roc(roughly_subset_full$Human_percentage_correct, roughly_subset_full$Human_input)
plot(roc_ai, col = "blue", main = "ROC: Used & roughly understand how it works", sub = "AI vs. Human", col.main = "darkblue", col.sub = "darkblue")
lines(roc_human, col = "red")
legend("bottomright", legend = c("AI", "Human"), col = c("blue", "red"), lty = 1)

# ROC: I have not heard of this before

notheardof_subset_full <- copy_cleaned_full_counts[copy_cleaned_full_counts$aiexperience == "I have not heard of this before", ]
notheardof_subset_full$AI_percentage_correct <- (notheardof_subset_full$AI_correct / 10) * 100
notheardof_subset_full$Human_percentage_correct <- (notheardof_subset_full$Human_correct / 10) * 100
roc_ai <- roc(notheardof_subset_full$AI_percentage_correct, notheardof_subset_full$AI_input)
roc_human <- roc(notheardof_subset_full$Human_percentage_correct, notheardof_subset_full$Human_input)
plot(roc_ai, col = "blue", main = "ROC: I have not heard of this before", sub = "AI vs. Human", col.main = "darkblue", col.sub = "darkblue")
lines(roc_human, col = "red")
legend("bottomright", legend = c("AI", "Human"), col = c("blue", "red"), lty = 1)
# ROC: I have used this before, but lack an understanding of how it works

lack_subset_full <- copy_cleaned_full_counts[copy_cleaned_full_counts$aiexperience == "I have used this before, but lack an understanding of how it works", ]
lack_subset_full$AI_percentage_correct <- (lack_subset_full$AI_correct / 10) * 100
lack_subset_full$Human_percentage_correct <- (lack_subset_full$Human_correct / 10) * 100
roc_ai <- roc(lack_subset_full$AI_percentage_correct, lack_subset_full$AI_input)
roc_human <- roc(lack_subset_full$Human_percentage_correct, lack_subset_full$Human_input)
plot(roc_ai, col = "blue", main = "ROC: I have not heard of this before", sub = "AI vs. Human", col.main = "darkblue", col.sub = "darkblue")
lines(roc_human, col = "red")
legend("bottomright", legend = c("AI", "Human"), col = c("blue", "red"), lty = 1)
