library(dplyr)
library(caret)
library(tidyr)
library(markdown)
library(pROC)
library(ggplot2)
library(stats)

# ___ DATA CLEANING ___

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

# write.csv(cleaned_full_counts, "cleaned_full_counts.csv", row.names = FALSE)

#____ DEMOGRAPHICS TABLE____
age_table <- cleaned_full_counts %>%
  filter(age >= 13 & age <= 23) %>%
  group_by(age) %>%
  summarise(count = n())

# Filter dataframe for gender Male or Female
gender_table <- cleaned_full_counts %>%
  filter(gender %in% c("Male", "Female")) %>%
  group_by(gender) %>%
  summarise(count = n())

# Filter dataframe for education excluding "None of the above" and "In elementary/primary school"
education_table <- cleaned_full_counts %>%
  filter(education != "None of the above" & education != "In elementary/primary school") %>%
  group_by(education) %>%
  summarise(count = n())

continent_table <- cleaned_full_counts %>%
  group_by(continent) %>%
  summarise(count = n())
discordexperience_table <- cleaned_full_counts %>%
  group_by(discordexperience) %>%
  summarise(count = n())
aiexperience_table <- cleaned_full_counts %>%
  group_by(aiexperience) %>%
  summarise(count = n())


# demographic_table <- rbind(age_table, gender_table, education_table)
demographic_table <- rbind(
  data.frame(Source = "Age", Frequency = NA),
  data.frame(Source = age_table$age, Frequency = age_table$count),
  data.frame(Source = "Gender", Frequency = NA),
  data.frame(Source = gender_table$gender, Frequency = gender_table$count),
  data.frame(Source = "Education", Frequency = NA),
  data.frame(Source = education_table$education, Frequency = education_table$count),
  data.frame(Source = "Continent", Frequency = NA),
  data.frame(Source = continent_table$continent, Frequency = continent_table$count),
  data.frame(Source = "Discord Experience", Frequency = NA),
  data.frame(Source = discordexperience_table$discordexperience, Frequency = discordexperience_table$count),
  data.frame(Source = "Experience with AI", Frequency = NA),
  data.frame(Source = aiexperience_table$aiexperience, Frequency = aiexperience_table$count)
)

