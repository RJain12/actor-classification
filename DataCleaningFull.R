
library(dplyr)
library(ggplot2)
cleaned_full_counts <- data.frame(age = na_renamed_columns_data[, 1])
cleaned_full_counts$gender <- na_renamed_columns_data[, 3]
cleaned_full_counts$education <- na_renamed_columns_data[, 4]
cleaned_full_counts$continent <- na_renamed_columns_data[, 5]
cleaned_full_counts$discordexperience <- na_renamed_columns_data[, 6]
cleaned_full_counts$aiexperience <- na_renamed_columns_data[, 7]


cleaned_full_counts$`AI_input` <- NA
cleaned_full_counts$`Human_input` <- NA
cleaned_full_counts$`AI_correct` <- NA
cleaned_full_counts$`Human_correct` <- NA

ai_counts <- apply(na_renamed_columns_data == "AI", 1, sum)
human_counts <- apply(na_renamed_columns_data == "Human", 1, sum)
cleaned_full_counts$`AI_input` <- ai_counts
cleaned_full_counts$`Human_input` <- human_counts

ai_columns <- grep("^AI", names(na_renamed_columns_data))
ai_correct_counts <- rowSums(na_renamed_columns_data[, ai_columns] == "AI")
cleaned_full_counts$`AI_correct` <- ai_correct_counts

human_columns <- grep("^Human", names(na_renamed_columns_data))
human_correct_counts <- rowSums(na_renamed_columns_data[, human_columns] == "Human")
cleaned_full_counts$`Human_correct` <- human_correct_counts

write.csv(cleaned_full_counts, "cleaned_full_counts.csv", row.names = FALSE)
