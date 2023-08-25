coded_renamed_columns_data <- na_renamed_columns_data

coded_renamed_columns_data[, 3] <- ifelse(coded_renamed_columns_data[, 3] == "Male", 0, 1)

# Replace values in column index 3 (assuming it's the fourth column)
coded_renamed_columns_data[, 4] <- ifelse(
  coded_renamed_columns_data[, 4] == "In high school", 1,
  ifelse(coded_renamed_columns_data[, 4] == "In college/undergraduate", 2,
         ifelse(coded_renamed_columns_data[, 4] == "In graduate school", 3,
                ifelse(coded_renamed_columns_data[, 4] == "In middle school", 0, 4)
         )
  )
)

coded_renamed_columns_data[, 5] <- ifelse(
  coded_renamed_columns_data[, 5] == "Africa", 0,
  ifelse(coded_renamed_columns_data[, 5] == "Asia", 1,
         ifelse(coded_renamed_columns_data[, 5] == "Europe", 2,
                ifelse(coded_renamed_columns_data[, 5] == "North America", 3,
                       ifelse(coded_renamed_columns_data[, 5] == "Oceania", 4,
                              ifelse(coded_renamed_columns_data[, 5] == "South America", 5, NA)
                       )
                )
         )
  )
)

coded_renamed_columns_data[, 6] <- ifelse(
  coded_renamed_columns_data[, 6] == "Frequent user (uses Discord 5-7 days/week)", 0,
  ifelse(coded_renamed_columns_data[, 6] == "Novice (uses Discord 1-2 days/week)", 1,
         ifelse(coded_renamed_columns_data[, 6] == "Proficient user (uses Discord 2-4 days/week)", 2,
                ifelse(coded_renamed_columns_data[, 6] == "Never used Discord.com", 3,
                       ifelse(coded_renamed_columns_data[, 6] == "Oceania", 4,
                              ifelse(coded_renamed_columns_data[, 6] == "South America", 5, NA)
                       )
                )
         )

  )
)
count_tally_matrix <- gender_matrix
count_tally_matrix$AI_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "AI" in the row
  ai_count <- sum(grepl("AI", coded_renamed_columns_data[i, ]))
  
  # Store the count in count_tally_matrix$AI_Count
  count_tally_matrix$AI_Count[i] <- ai_count
}

count_tally_matrix$Human_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "Human" in the row
  human_count <- sum(grepl("Human", coded_renamed_columns_data[i, ]))
  
  # Store the count in count_tally_matrix$Human_Count
  count_tally_matrix$Human_Count[i] <- human_count
}

data <- data.frame(
  DependentVariable = count_tally_matrix[, 2],
  Age = coded_renamed_columns_data[, 3],
  Gender = coded_renamed_columns_data[, 4],
  EducationalBackground = coded_renamed_columns_data[, 5],
  Continent = coded_renamed_columns_data[, 6]
)

# Fit the multiple regression model
model <- lm(DependentVariable ~ Age + Gender + EducationalBackground + Continent, data)

# View the summary of the regression model
summary(model)

