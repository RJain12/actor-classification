library(dplyr)
library(caret)
library(tidyr)
library(markdown)
library(pROC)
library(ggplot2)
library(stats)
library(car)
library(lmtest)

# _____________ PREPARATION OF GENDER MATRIX ______

gender_matrix$AI_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "AI" in the row
  ai_count <- sum(grepl("AI", coded_renamed_columns_data[i, ]))
  
  # Store the count in gender_matrix$AI_Count
  gender_matrix$AI_Count[i] <- ai_count
}

gender_matrix$Human_Count <- 0

# Loop through each row of coded_renamed_columns_data
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Count the occurrences of "Human" in the row
  human_count <- sum(grepl("Human", coded_renamed_columns_data[i, ]))
  
  # Store the count in gender_matrix$Human_Count
  gender_matrix$Human_Count[i] <- human_count
}


gender_matrix <- matrix(ncol = 3, nrow = nrow(na_renamed_columns_data))
colnames(gender_matrix) <- c("Gender", "AI_Count", "Human_Count")

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
  gender_matrix[row_num, ] <- c(row_num, ai_count, human_count)
  
  # Update the total count
  ai_correct_count <- ai_correct_count + ai_count
  human_correct_count <- human_correct_count + human_count
}
gender_matrix <- as.data.frame(gender_matrix)
gender_matrix$Gender <- na_renamed_columns_data[, 3]

# _____________ GENDER DESCRIPTIVE STATISTICS ______
average_ai_count_male <- mean(gender_matrix$AI_Count[gender_matrix$Gender == "Male"], na.rm = TRUE)
average_ai_count_male <- round(average_ai_count_male, 5)
print(average_ai_count_male)
average_ai_count_female <- mean(gender_matrix$AI_Count[gender_matrix$Gender == "Female"], na.rm = TRUE)
average_ai_count_female <- round(average_ai_count_female, 5)
print(average_ai_count_female)

## ___ CONTINENT MATRIX PREPARATION ___ 
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

# _____________ CONTINENT DESCRIPTIVE STATISTICS ______
north_america_avg <- mean(continent_matrix$AI_Count[continent_matrix$Continent == "North America"], na.rm = TRUE)
south_america_avg <- mean(continent_matrix$AI_Count[continent_matrix$Continent == "South America"], na.rm = TRUE)
europe_avg <- mean(continent_matrix$AI_Count[continent_matrix$Continent == "Europe"], na.rm = TRUE)
africa_avg <- mean(continent_matrix$AI_Count[continent_matrix$Continent == "Africa"], na.rm = TRUE)
asia_avg <- mean(continent_matrix$AI_Count[continent_matrix$Continent == "Asia"], na.rm = TRUE)
oceania_avg <- mean(continent_matrix$AI_Count[continent_matrix$Continent == "Oceania"], na.rm = TRUE)
cat("North America Average:", sprintf("%.5f", north_america_avg), "\n")
cat("South America Average:", sprintf("%.5f", south_america_avg), "\n")
cat("Europe Average:", sprintf("%.5f", europe_avg), "\n")
cat("Africa Average:", sprintf("%.5f", africa_avg), "\n")
cat("Asia Average:", sprintf("%.5f", asia_avg), "\n")
cat("Oceania Average:", sprintf("%.5f", oceania_avg), "\n")
north_america_percentage <- (north_america_avg / 5) * 100
south_america_percentage <- (south_america_avg / 5) * 100
europe_percentage <- (europe_avg / 5) * 100
africa_percentage <- (africa_avg / 5) * 100
asia_percentage <- (asia_avg / 5) * 100
oceania_percentage <- (oceania_avg / 5) * 100
cat("North America Percentage:", sprintf("%.2f%%", north_america_percentage), "\n")
cat("South America Percentage:", sprintf("%.2f%%", south_america_percentage), "\n")
cat("Europe Percentage:", sprintf("%.2f%%", europe_percentage), "\n")
cat("Africa Percentage:", sprintf("%.2f%%", africa_percentage), "\n")
cat("Asia Percentage:", sprintf("%.2f%%", asia_percentage), "\n")
cat("Oceania Percentage:", sprintf("%.2f%%", oceania_percentage), "\n")

north_america_avg <- mean(continent_matrix$Human_Count[continent_matrix$Continent == "North America"], na.rm = TRUE)
south_america_avg <- mean(continent_matrix$Human_Count[continent_matrix$Continent == "South America"], na.rm = TRUE)
europe_avg <- mean(continent_matrix$Human_Count[continent_matrix$Continent == "Europe"], na.rm = TRUE)
africa_avg <- mean(continent_matrix$Human_Count[continent_matrix$Continent == "Africa"], na.rm = TRUE)
asia_avg <- mean(continent_matrix$Human_Count[continent_matrix$Continent == "Asia"], na.rm = TRUE)
oceania_avg <- mean(continent_matrix$Human_Count[continent_matrix$Continent == "Oceania"], na.rm = TRUE)
print("HUMAN COUNT")
cat("North America Average:", sprintf("%.5f", north_america_avg), "\n")
cat("South America Average:", sprintf("%.5f", south_america_avg), "\n")
cat("Europe Average:", sprintf("%.5f", europe_avg), "\n")
cat("Africa Average:", sprintf("%.5f", africa_avg), "\n")
cat("Asia Average:", sprintf("%.5f", asia_avg), "\n")
cat("Oceania Average:", sprintf("%.5f", oceania_avg), "\n")
north_america_percentage <- (north_america_avg / 5) * 100
south_america_percentage <- (south_america_avg / 5) * 100
europe_percentage <- (europe_avg / 5) * 100
africa_percentage <- (africa_avg / 5) * 100
asia_percentage <- (asia_avg / 5) * 100
oceania_percentage <- (oceania_avg / 5) * 100
print("HUMAN COUNT")
cat("North America Percentage:", sprintf("%.2f%%", north_america_percentage), "\n")
cat("South America Percentage:", sprintf("%.2f%%", south_america_percentage), "\n")
cat("Europe Percentage:", sprintf("%.2f%%", europe_percentage), "\n")
cat("Africa Percentage:", sprintf("%.2f%%", africa_percentage), "\n")
cat("Asia Percentage:", sprintf("%.2f%%", asia_percentage), "\n")
cat("Oceania Percentage:", sprintf("%.2f%%", oceania_percentage), "\n")

## ___ EDUCATION MATRIX PREPARATION ___ 
education_matrix$AI_Count <- 0

# Loop through each row of coded_renamed_columns_data
# for (i in 1:nrow(coded_renamed_columns_data)) {
#   # Count the occurrences of "AI" in the row
#   ai_count <- sum(grepl("AI", coded_renamed_columns_data[i, ]))
#   
#   # Store the count in education_matrix$AI_Count
#   education_matrix$AI_Count[i] <- ai_count
# }
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Identify columns starting with "AI"
  ai_columns <- grep("^AI", names(coded_renamed_columns_data), value = TRUE)
  
  # Count the occurrences of "AI" in the selected columns of the row
  ai_count <- sum(coded_renamed_columns_data[i, ai_columns] == "AI")
  
  # Store the count in education_matrix$AI_Count
  education_matrix$AI_Count[i] <- ai_count
}



education_matrix$Human_Count <- 0

# Loop through each row of coded_renamed_columns_data
# for (i in 1:nrow(coded_renamed_columns_data)) {
#   # Count the occurrences of "Human" in the row
#   human_count <- sum(grepl("Human", coded_renamed_columns_data[i, ]))
#   
#   # Store the count in education_matrix$Human_Count
#   education_matrix$Human_Count[i] <- human_count
# }
for (i in 1:nrow(coded_renamed_columns_data)) {
  # Identify columns starting with "Human"
  human_columns <- grep("^Human", names(coded_renamed_columns_data), value = TRUE)
  
  # Count the occurrences of "Human" in the selected columns of the row
  human_count <- sum(coded_renamed_columns_data[i, human_columns] == "Human")
  
  # Store the count in education_matrix$AI_Count
  education_matrix$Human_Count[i] <- human_count
}



education_matrix <- matrix(ncol = 3, nrow = nrow(na_renamed_columns_data))
colnames(education_matrix) <- c("Education", "AI_Count", "Human_Count")

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
  education_matrix[row_num, ] <- c(row_num, ai_count, human_count)
  
  # Update the total count
  ai_correct_count <- ai_correct_count + ai_count
  human_correct_count <- human_correct_count + human_count
}
education_matrix <- as.data.frame(education_matrix)
education_matrix$Education <- na_renamed_columns_data[, 4]

# ____ DESCRIPTIVE STATISTICS EDUCATION _____
in_high_school <- subset(education_matrix, Education == "In high school")
in_college <- subset(education_matrix, Education == "In college/undergraduate")
none_of_the_above <- subset(education_matrix, Education == "None of the above")
in_middle_school <- subset(education_matrix, Education == "In middle school")

avg_in_high_school <- round(mean(in_high_school$AI_Count), 5)
avg_in_college <- round(mean(in_college$AI_Count), 5)
avg_none_of_the_above <- round(mean(none_of_the_above$AI_Count), 5)
avg_in_middle_school <- round(mean(in_middle_school$AI_Count), 5)

cat("Average AI_Count for 'In high school':", avg_in_high_school, "\n")
cat("Average AI_Count for 'In college/undergraduate':", avg_in_college, "\n")
cat("Average AI_Count for 'None of the above':", avg_none_of_the_above, "\n")
cat("Average AI_Count for 'In middle school':", avg_in_middle_school, "\n")
avg_in_high_school <- round(mean(in_high_school$AI_Count), 5) / 5 * 100
avg_in_college <- round(mean(in_college$AI_Count), 5) / 5 * 100
avg_none_of_the_above <- round(mean(none_of_the_above$AI_Count), 5) / 5 * 100
avg_in_middle_school <- round(mean(in_middle_school$AI_Count), 5) / 5 * 100

cat("Average AI_Count for 'In high school':", avg_in_high_school, "%\n")
cat("Average AI_Count for 'In college/undergraduate':", avg_in_college, "%\n")
cat("Average AI_Count for 'None of the above':", avg_none_of_the_above, "%\n")
cat("Average AI_Count for 'In middle school':", avg_in_middle_school, "%\n")

avg_in_high_school <- round(mean(in_high_school$Human_Count), 5)
avg_in_college <- round(mean(in_college$Human_Count), 5)
# avg_none_of_the_above <- round(mean(none_of_the_above$Human_Count), 5)
avg_in_middle_school <- round(mean(in_middle_school$Human_Count), 5)

cat("Average Human_Count for 'In high school':", avg_in_high_school, "\n")
cat("Average Human_Count for 'In college/undergraduate':", avg_in_college, "\n")
cat("Average Human_Count for 'None of the above':", avg_none_of_the_above, "\n")
cat("Average Human_Count for 'In middle school':", avg_in_middle_school, "\n")
avg_in_high_school <- round(mean(in_high_school$Human_Count), 5) / 5 * 100
avg_in_college <- round(mean(in_college$Human_Count), 5) / 5 * 100
# avg_none_of_the_above <- round(mean(none_of_the_above$Human_Count), 5) / 5 * 100
avg_in_middle_school <- round(mean(in_middle_school$Human_Count), 5) / 5 * 100

cat("Average Human_Count for 'In high school':", avg_in_high_school, "%\n")
cat("Average Human_Count for 'In college/undergraduate':", avg_in_college, "%\n")
# cat("Average Human_Count for 'None of the above':", avg_none_of_the_above, "%\n")
cat("Average Human_Count for 'In middle school':", avg_in_middle_school, "%\n")