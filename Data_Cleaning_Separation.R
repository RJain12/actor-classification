library(dplyr)

# Load the Survey_Data_Cleaned.csv as a dataframe
original_survey_data <- read.csv("Survey_Data_Cleaned.csv")

# Create new dataframes for each selected column using column numbers/index
age <- original_survey_data[, 1, drop = FALSE]
gender <- original_survey_data[, 3, drop = FALSE]
education <- original_survey_data[, 4, drop = FALSE]
continent <- original_survey_data[, 5, drop = FALSE]
discord_experience <- original_survey_data[, 6, drop = FALSE]
ai_experience <- original_survey_data[, 7, drop = FALSE]

colnames(age)[1] <- "age"
colnames(gender)[1] <- "gender"
colnames(education)[1] <- "education"
colnames(continent)[1] <- "continent"
colnames(discord_experience)[1] <- "discord_experience"
colnames(ai_experience)[1] <- "ai_experience"

age$age <- as.numeric(age$age)
age <- na.omit(age)
