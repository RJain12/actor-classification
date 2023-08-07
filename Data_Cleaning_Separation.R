library(dplyr)
library(kableExtra)
library(webshot)
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

demographics_table <- data.frame(
  "Demographic Variable" = character(),
  "Categories" = character(),
  "count" = integer(),
  "percentage" = character(),
  stringsAsFactors = FALSE
)

# Add Age section
demographics_table <- rbind(demographics_table,
                            c("Age", "Min:", "[minimum]", ""),
                            c("", "Max:", "[maximum]", ""),
                            c("", "Mean:", "[mean]", ""),
                            c("", "Median:", "[median]", ""))

# Add Gender section
demographics_table <- rbind(demographics_table,
                            c("Gender", "Male", "[count]", "[percentage]"),
                            c("", "Female", "[count]", "[percentage]"),
                            c("", "Prefer not to say", "[count]", "[percentage]"),
                            c("", "Non-binary", "[count]", "[percentage]"))

# Add Educational Background section
demographics_table <- rbind(demographics_table,
                            c("Educational Background", "In high school", "[count]", "[percentage]"),
                            c("", "In middle school", "[count]", "[percentage]"),
                            c("", "In college/undergraduate", "[count]", "[percentage]"),
                            c("", "None of the above", "[count]", "[percentage]"))

# Add Continent section
demographics_table <- rbind(demographics_table,
                            c("Continent", "North America", "[count]", "[percentage]"),
                            c("", "Asia", "[count]", "[percentage]"),
                            c("", "Africa", "[count]", "[percentage]"),
                            c("", "Oceania", "[count]", "[percentage]"),
                            c("", "South America", "[count]", "[percentage]"),
                            c("", "Europe", "[count]", "[percentage]"))

# Add Experience with Discord section
demographics_table <- rbind(demographics_table,
                            c("Experience with Discord", "Frequent user (uses Discord 5-7 days/week)", "[count]", "[percentage]"),
                            c("", "Novice (uses Discord 1-2 days/week)", "[count]", "[percentage]"),
                            c("", "Proficient user (uses Discord 2-4 days/week)", "[count]", "[percentage]"),
                            c("", "Never used Discord.com", "[count]", "[percentage]"))

# Add Experience with AI section
demographics_table <- rbind(demographics_table,
                            c("Experience with AI", "I have used this, and thoroughly understand how it works", "[count]", "[percentage]"),
                            c("", "I have used this before, but lack an understanding of how it works", "[count]", "[percentage]"),
                            c("", "I have used this, and roughly understand how it works", "[count]", "[percentage]"),
                            c("", "I have heard of this before, but have not used it", "[count]", "[percentage]"),
                            c("", "I have not heard of this before", "[count]", "[percentage]"))

colnames(demographics_table)[1] <- "variable"
colnames(demographics_table)[2] <- "options"
colnames(demographics_table)[3] <- "count"
colnames(demographics_table)[4] <- "percentage"

# Update the 'demographics_table' with age statistics
age <- subset(age, age <= 27)
age_min <- min(age$age)
age_max <- max(age$age)
age_mean <- mean(age$age)
age_median <- median(age$age)
demographics_table[1, "count"] <- age_min
demographics_table[2, "count"] <- age_max
demographics_table[3, "count"] <- age_mean
demographics_table[4, "count"] <- age_median

count_min_age <- sum(age$age == age_min)
percentage_min_age <- (count_min_age / total_count) * 100
percentage_min_age <- sprintf("%.2f%%", percentage_min_age)
demographics_table[1, 4] <- percentage_min_age

count_max_age <- sum(age$age == age_max)
percentage_max_age <- (count_max_age / total_count) * 100
percentage_max_age <- sprintf("%.2f%%", percentage_max_age)
demographics_table[2, 4] <- percentage_max_age

count_mean_age <- sum(round(age$age) == round(age_mean))
percentage_mean_age <- (count_mean_age / total_count) * 100
percentage_mean_age <- sprintf("%.2f%%", percentage_mean_age)
demographics_table[3, 4] <- percentage_mean_age

count_median_age <- sum(round(age$age) == round(age_median))
percentage_median_age <- (count_median_age / total_count) * 100
percentage_median_age <- sprintf("%.2f%%", percentage_median_age)
demographics_table[4, 4] <- percentage_median_age

# Update the 'demographics_table' with gender statistics
count_male <- sum(gender$gender == "Male")
count_female <- sum(gender$gender == "Female")
count_prefernottosay <- sum(gender$gender == "Prefer not to say")
count_nonbinary <- sum(gender$gender == "Non-binary")


demographics_table[5, "count"] <- count_male
demographics_table[6, "count"] <- count_female
demographics_table[7, "count"] <- count_prefernottosay
demographics_table[8, "count"] <- count_nonbinary

total_count_gender <- nrow(gender)

percentage_male <- (count_male / total_count_gender) * 100
percentage_male <- sprintf("%.2f%%", percentage_male)
demographics_table[5, 4] <- percentage_male

percentage_female <- (count_female / total_count_gender) * 100
percentage_female <- sprintf("%.2f%%", percentage_female)
demographics_table[6, 4] <- percentage_female

percentage_prefernottosay <- (count_prefernottosay / total_count_gender) * 100
percentage_prefernottosay <- sprintf("%.2f%%", percentage_prefernottosay)
demographics_table[7, 4] <- percentage_prefernottosay

percentage_nonbinary <- (count_nonbinary / total_count_gender) * 100
percentage_nonbinary <- sprintf("%.2f%%", percentage_nonbinary)
demographics_table[8, 4] <- percentage_nonbinary

# Update the 'demographics_table' with education statistics

count_highschool <- sum(education$education == "In high school")
count_middleschool <- sum(education$education == "In middle school")
count_college <- sum(education$education == "In college/undergraduate")
count_edunoneoftheabove <- sum(education$education == "None of the above")


demographics_table[9, "count"] <- count_highschool
demographics_table[10, "count"] <- count_middleschool
demographics_table[11, "count"] <- count_college
demographics_table[12, "count"] <- count_edunoneoftheabove

total_count_edu <- nrow(education)

percentage_highschool <- (count_highschool / total_count_edu) * 100
percentage_highschool <- sprintf("%.2f%%", percentage_highschool)
demographics_table[9, 4] <- percentage_highschool

percentage_middleschool <- (count_middleschool / total_count_edu) * 100
percentage_middleschool <- sprintf("%.2f%%", percentage_middleschool)
demographics_table[10, 4] <- percentage_middleschool

percentage_college <- (count_college / total_count_edu) * 100
percentage_college <- sprintf("%.2f%%", percentage_college)
demographics_table[11, 4] <- percentage_college

percentage_edunoneoftheabove <- (count_edunoneoftheabove / total_count_edu) * 100
percentage_edunoneoftheabove <- sprintf("%.2f%%", percentage_edunoneoftheabove)
demographics_table[12, 4] <- percentage_edunoneoftheabove



# Update the 'demographics_table' with continent statistics
count_northamerica <- sum(continent$continent == "North America")
count_asia <- sum(continent$continent == "Asia")
count_africa <- sum(continent$continent == "Africa")
count_oceania <- sum(continent$continent == "Oceania")
count_southamerica <- sum(continent$continent == "South America")
count_europe <- sum(continent$continent == "Europe")

demographics_table[13, "count"] <- count_northamerica
demographics_table[14, "count"] <- count_asia
demographics_table[15, "count"] <- count_africa
demographics_table[16, "count"] <- count_oceania
demographics_table[17, "count"] <- count_southamerica
demographics_table[18, "count"] <- count_europe


total_count_continent <- nrow(continent)

percentage_northamerica <- (count_northamerica / total_count_continent) * 100
percentage_northamerica <- sprintf("%.2f%%", percentage_northamerica)
demographics_table[13, 4] <- percentage_northamerica

percentage_asia <- (count_asia / total_count_continent) * 100
percentage_asia <- sprintf("%.2f%%", percentage_asia)
demographics_table[14, 4] <- percentage_asia

percentage_africa <- (count_africa / total_count_continent) * 100
percentage_africa <- sprintf("%.2f%%", percentage_africa)
demographics_table[15, 4] <- percentage_africa

percentage_oceania <- (count_oceania / total_count_continent) * 100
percentage_oceania <- sprintf("%.2f%%", percentage_oceania)
demographics_table[16, 4] <- percentage_oceania

percentage_southamerica <- (count_southamerica / total_count_continent) * 100
percentage_southamerica <- sprintf("%.2f%%", percentage_southamerica)
demographics_table[17, 4] <- percentage_southamerica

percentage_europe <- (count_europe / total_count_continent) * 100
percentage_europe <- sprintf("%.2f%%", percentage_europe)
demographics_table[18, 4] <- percentage_europe

# Update the 'demographics_table' with discord statistics
count_frequent <- sum(discord_experience$discord_experience == "Frequent user (uses Discord 5-7 days/week)")
count_novice <- sum(discord_experience$discord_experience == "Novice (uses Discord 1-2 days/week)")
count_proficient <- sum(discord_experience$discord_experience == "Proficient user (uses Discord 2-4 days/week)")
count_never <- sum(discord_experience$discord_experience == "Never used Discord.com")

demographics_table[19, "count"] <- count_frequent
demographics_table[20, "count"] <- count_novice
demographics_table[21, "count"] <- count_proficient
demographics_table[22, "count"] <- count_never


total_count_discord <- nrow(continent)

percentage_northamerica <- (count_frequent / total_count_discord) * 100
percentage_novice <- (count_novice / total_count_discord) * 100
percentage_proficient <- (count_proficient / total_count_discord) * 100
percentage_never <- (count_never / total_count_discord) * 100

percentage_northamerica <- sprintf("%.2f%%", percentage_northamerica)
demographics_table[19, 4] <- percentage_northamerica

percentage_novice <- sprintf("%.2f%%", percentage_novice)
demographics_table[20, 4] <- percentage_novice

percentage_proficient <- sprintf("%.2f%%", percentage_proficient)
demographics_table[21, 4] <- percentage_proficient

percentage_never <- sprintf("%.2f%%", percentage_never)
demographics_table[22, 4] <- percentage_never


# Update the 'demographics_table' with ai statistics
count_thorough <- sum(ai_experience$ai_experience == "I have used this, and thoroughly understand how it works")
count_lack <- sum(ai_experience$ai_experience == "I have used this before, but lack an understanding of how it works")
count_roughly <- sum(ai_experience$ai_experience == "I have used this, and roughly understand how it works")
count_notused <- sum(ai_experience$ai_experience == "I have heard of this before, but have not used it")
count_notheard <- sum(ai_experience$ai_experience == "I have not heard of this before")

demographics_table[23, "count"] <- count_thorough
demographics_table[24, "count"] <- count_lack
demographics_table[25, "count"] <- count_roughly
demographics_table[26, "count"] <- count_notused
demographics_table[27, "count"] <- count_notheard

total_count_ai <- nrow(ai_experience)

percentage_thorough <- (count_thorough / total_count_discord) * 100
percentage_lack <- (count_lack / total_count_discord) * 100
percentage_roughly <- (count_roughly / total_count_discord) * 100
percentage_notused <- (count_notused / total_count_discord) * 100
percentage_notheard <- (count_notheard / total_count_discord) * 100

percentage_thorough <- sprintf("%.2f%%", percentage_thorough)
demographics_table[23, 4] <- percentage_thorough

percentage_lack <- sprintf("%.2f%%", percentage_lack)
demographics_table[24, 4] <- percentage_lack

percentage_roughly <- sprintf("%.2f%%", percentage_roughly)
demographics_table[25, 4] <- percentage_roughly

percentage_notused <- sprintf("%.2f%%", percentage_notused)
demographics_table[26, 4] <- percentage_notused

percentage_notheard <- sprintf("%.2f%%", percentage_notheard)
demographics_table[27, 4] <- percentage_notheard

write.csv(demographics_table, file = "tables/demographics_table.csv", row.names = FALSE)

