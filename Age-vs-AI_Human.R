library(dplyr)
library(ggplot2)
# Initial Processing to get IV - age & DV

age_and_response <- data.frame(age = na_renamed_columns_data[, 1])
age_and_response$`AI_input` <- NA
age_and_response$`Human_input` <- NA
age_and_response$`AI_correct` <- NA
age_and_response$`Human_correct` <- NA

ai_counts <- apply(na_renamed_columns_data == "AI", 1, sum)
human_counts <- apply(na_renamed_columns_data == "Human", 1, sum)
age_and_response$`AI_input` <- ai_counts
age_and_response$`Human_input` <- human_counts

ai_columns <- grep("^AI", names(na_renamed_columns_data))
ai_correct_counts <- rowSums(na_renamed_columns_data[, ai_columns] == "AI")
age_and_response$`AI_correct` <- ai_correct_counts

human_columns <- grep("^Human", names(na_renamed_columns_data))
human_correct_counts <- rowSums(na_renamed_columns_data[, human_columns] == "Human")
age_and_response$`Human_correct` <- human_correct_counts

# Age vs AI/Human Correct
average_values <- aggregate(AI_correct ~ age, data = age_and_response, FUN = mean)
average_values <- average_values[average_values$age <= 30 & average_values$age >= 13, ]
lm_model <- lm(AI_correct ~ as.numeric(factor(age)), data = average_values)
lm_model
tikz("tikz/agegraph1", width = 6, height = 4, standAlone = TRUE)
age_vs_aicorrect <- ggplot(average_values, aes(x = factor(age), y = AI_correct)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_segment(aes(xend = as.numeric(factor(age)), yend = AI_correct),
               color = "gray", linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +  # Add line
  labs(title = "Average AI Correct Answers by Age",
       x = "Age",
       y = "Average AI Correct Answers") +
  theme_minimal() +
  theme(legend.position = "none")
print(age_vs_aicorrect)
dev.off()
ggsave("C:/Research/AI/actor-classification/graphs/age_vs_aicorrect_plot.jpg", plot = age_vs_aicorrect)

average_values <- aggregate(Human_correct ~ age, data = age_and_response, FUN = mean)
average_values <- average_values[average_values$age <= 30 & average_values$age >= 13, ]

# Fit a linear regression model
lm_model <- lm(Human_correct ~ as.numeric(factor(age)), data = average_values)
tikz("tikz/agegraphhuman1", width = 6, height = 4, standAlone = TRUE)

age_vs_humancorrect <- ggplot(average_values, aes(x = factor(age), y = Human_correct)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_segment(aes(xend = as.numeric(factor(age)), yend = Human_correct),
               color = "gray", linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
  labs(title = "Average Human Correct Answers by Age",
       x = "Age",
       y = "Average Human Correct Answers") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = paste("y =", round(coef(lm_model)[1], 2), "+", round(coef(lm_model)[2], 2), "* x")),
            x = 25, y = max(average_values$Human_correct), parse = TRUE, hjust = 0)

print(age_vs_humancorrect)

dev.off()




ggsave("C:/Research/AI/actor-classification/graphs/age_vs_humancorrect_plot.jpg", plot = age_vs_humancorrect)
write.csv(age_and_response, "tables/age-vs-response.csv", row.names = FALSE)


