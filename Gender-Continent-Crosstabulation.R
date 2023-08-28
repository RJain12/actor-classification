library(dplyr)
library(ggplot2)
library(reshape2)
# Initial Processing to get Gender + Continent & DV

gender_continent_tabulate <- data.frame(gender = na_renamed_columns_data[, 3])
gender_continent_tabulate$continent <- na_renamed_columns_data[, 5]

gender_continent_tabulate$`AI_input` <- NA
gender_continent_tabulate$`Human_input` <- NA
gender_continent_tabulate$`AI_correct` <- NA
gender_continent_tabulate$`Human_correct` <- NA

ai_counts <- apply(na_renamed_columns_data == "AI", 1, sum)
human_counts <- apply(na_renamed_columns_data == "Human", 1, sum)
gender_continent_tabulate$`AI_input` <- ai_counts
gender_continent_tabulate$`Human_input` <- human_counts

ai_columns <- grep("^AI", names(na_renamed_columns_data))
ai_correct_counts <- rowSums(na_renamed_columns_data[, ai_columns] == "AI")
gender_continent_tabulate$`AI_correct` <- ai_correct_counts

human_columns <- grep("^Human", names(na_renamed_columns_data))
human_correct_counts <- rowSums(na_renamed_columns_data[, human_columns] == "Human")
gender_continent_tabulate$`Human_correct` <- human_correct_counts

gender_continent_crosstab <- table(gender_continent_tabulate$gender, gender_continent_tabulate$continent, gender_continent_tabulate$AI_correct)
gender_continent_crosstab <- as.data.frame(gender_continent_crosstab)
gender_continent_crosstab <- gender_continent_crosstab[!is.na(gender_continent_crosstab$Var1) & gender_continent_crosstab$Var1 != "" & gender_continent_crosstab$Var1 != "Prefer not to say", ]
gender_continent_crosstab$Var3 <- as.numeric(gender_continent_crosstab$Var3)
contingency_table <- xtabs(Freq ~ Var1 + Var2, data = gender_continent_crosstab)
filtered_data <- gender_continent_crosstab %>%
  filter(Var1 %in% c("Male", "Female"))
t_test_result <- t.test(Var3 ~ Var1, data = filtered_data)
print(t_test_result)
fisher_result <- fisher.test(contingency_table)
print(fisher_result)
