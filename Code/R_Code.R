
output_path <- ""

#############Load Packages#############
library(readr)
library(dplyr)
library(stargazer)

#############Load Data#############
celebrity <- read_csv("", locale = locale(encoding = "UTF-8"))

#############Prepare Data#############
celebrity <- celebrity %>%
  mutate(
    ethnicity = as.factor(ethnicity),
    origin = as.factor(origin),
    gender = as.factor(gender),
    education = as.factor(education),
    profession = as.factor(profession)
  )

# create a new variable called log_activity for celebrity
celebrity <- celebrity %>%
  mutate(
    log_activity = log(activity),  # Apply log transformation to 'activity'
    log_followers = log(followers) # Apply log transformation to 'followers'
  )

#############Table 3############## 
celebrity_clean <- celebrity %>%
  select(age, log_followers, ethnicity, origin, gender, repost, log_activity, education, profession) %>%
  na.omit()
celebrity_clean$profession <- relevel(celebrity_clean$profession, ref = "Other")
celebrity_clean$education <- relevel(celebrity_clean$education, ref = "High School")

# Fit the individual models for tested variables
model1 <- lm(repost ~ origin + ethnicity + gender + education + profession, data = celebrity_clean)
model2 <- lm(repost ~ age + ethnicity + gender + education + profession, data = celebrity_clean)
model3 <- lm(repost ~ log_followers + ethnicity + gender + education + profession, data = celebrity_clean)
model4 <- lm(repost ~ log_activity + ethnicity + gender + education + profession, data = celebrity_clean)
model_full <- lm(repost ~ origin + age + log_followers + log_activity + ethnicity + gender + education + profession, data = celebrity_clean)

stargazer(model1, model2, model3, model4, model_full,
          type = "html",
          dep.var.labels = "Level of Political Engagement (Reposts)",
          covariate.labels = c("Origin", "Age", "Followers", "Online Activity"),
          omit = c("ethnicity", "gender", "education", "profession"),
          omit.labels = NULL,
          add.lines = list(c("Controls", "Yes", "Yes", "Yes", "Yes", "Yes")),
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("*", "**", "***"),
          notes = c("*p<0.1; **p<0.05; ***p<0.01",
                    "Models (1)-(4) test Origin, Age, Follower, Online_Activity separately. Model (5) includes all variables.",
                    "Origin = 1 if the celebrity is from HK or Taiwan, 0 if from mainland China.",
                    "Follower and activity are measured using log transformation."),
          notes.align = "l",
          notes.append = FALSE,
          no.space = TRUE,
          single.row = TRUE,
          out = paste0(output_path, "table_3.html"))

#############Table 9: Full Table for Table 3#############
stargazer(model1, model2, model3, model4, model_full,
          type = "html",
          dep.var.labels = "Level of Political Engagement (Reposts)",
          covariate.labels = c("Origin", "Age", "Number of Followers", "Online Activity", "Ethnic Minority", "Female","College Degree", "Profession: Actor/Actress", "Profession: Idol", "Profession: Singer"),
          omit.labels = NULL,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("*", "**", "***"),
          notes = c("*p<0.1; **p<0.05; ***p<0.01",
                    "Models (1)-(4) test Origin, Age, Follower, Online_Activity separately. Model (5) includes all variables.",
                    "Origin = 1 if the celebrity is from HK or Taiwan, 0 if from mainland China.",
                    "Follower and activity are measured using log transformation.",
                    "College Degree = 1 if the celebrity achieves college degree, 0 if high school or below.",
                    "Reference class for profession = other, including influencers, hosts and directors."),
          notes.align = "l",
          notes.append = FALSE,
          no.space = TRUE,
          single.row = TRUE,
          out = paste0(output_path, "table_9.html"))

#############Table 4############## 
celebrity_clean <- celebrity %>%
  mutate(across(c(ethnicity, origin, gender), as.factor)) %>%
  select(age, log_followers, ethnicity, origin, gender, log_activity, 
         sovereignty_repost, nationalism_repost, party_line_repost,
         culture_repost, gov_announcement_repost,
         moral_society_repost, non_political_repost, education, profession) %>%
  na.omit()

# List of dependent variables (repost categories)
repost_vars <- c("sovereignty_repost", "nationalism_repost", "party_line_repost", 
                 "culture_repost", "gov_announcement_repost", 
                 "moral_society_repost", "non_political_repost")

# Model 1: Just individual characteristics
individual_models <- lapply(repost_vars, function(var) {
  formula <- as.formula(paste(var, "~ age + log_followers + log_activity"))
  lm(formula, data = celebrity_clean)
})

# Model 2: Full models with demographic controls (including ethnicity, gender, education, profession)
full_models <- lapply(repost_vars, function(var) {
  formula <- as.formula(paste(var, "~ age + log_followers + log_activity + origin + ethnicity + gender + education + profession"))
  lm(formula, data = celebrity_clean)
})

# Combine the individual and full models into one list 
all_models <- c(individual_models, full_models)

# Create the regression table using stargazer for the full models
stargazer(full_models, 
          type = "html", 
          dep.var.labels = c("Sovereignty", "Nationalism", "Party Line", "Culture", 
                             "Gov Announcement", "Moral Society", "Non-Political"), 
          covariate.labels = c("Age", "Number of Followers", "Online Activity", "Origin"),
          omit = c("ethnicity", "gender", "education", "profession"), 
          add.lines = list(c("Controls", rep("Yes", 7))),
          star.cutoffs = c(0.1, 0.05, 0.01),  
          star.char = c("*", "**", "***"),     
          notes = c("*p<0.1; **p<0.05; ***p<0.01",
                    "Models (1)-(4) test Origin, Age, Follower, Online_Activity separately. Model (5) includes all variables.",
                    "Origin = 1 if the celebrity is from HK or Taiwan, 0 if from mainland China.",
                    "Follower and activity are measured using log transformation."),
          notes.align = "l",
          notes.append = FALSE,
          no.space = TRUE,
          out = paste0(output_path, "table_4.html"))

#############Table 10: Full Table for Table 4#############
celebrity_clean <- celebrity %>%
  mutate(across(c(ethnicity, origin, gender), as.factor)) %>%
  select(age, log_followers, ethnicity, origin, gender, log_activity, 
         sovereignty_repost, nationalism_repost, party_line_repost,
         culture_repost, gov_announcement_repost,
         moral_society_repost, non_political_repost, education, profession) %>%
  na.omit()

# List of dependent variables (repost categories)
repost_vars <- c("sovereignty_repost", "nationalism_repost", "party_line_repost", 
                 "culture_repost", "gov_announcement_repost", 
                 "moral_society_repost", "non_political_repost")

# Model 1: Just individual characteristics
individual_models <- lapply(repost_vars, function(var) {
  formula <- as.formula(paste(var, "~ age + log_followers + log_activity"))
  lm(formula, data = celebrity_clean)
})

# Model 2: Full models with demographic controls (including ethnicity, gender, education, profession)
full_models <- lapply(repost_vars, function(var) {
  formula <- as.formula(paste(var, "~ age + log_followers + log_activity + origin + ethnicity + gender + education + profession"))
  lm(formula, data = celebrity_clean)
})

# Combine the individual and full models into one list 
all_models <- c(individual_models, full_models)

stargazer(full_models, 
          type = "html", 
          dep.var.labels = c("Sovereignty", "Nationalism", "Party Line", "Culture", 
                             "Gov Announcement", "Moral Society", "Non-Political"), 
          covariate.labels = c("Age", "Number of Followers", "Online Activity", "Origin", "Ethnic Minority", "Female","College Degree", "Profession: Actor/Actress", "Profession: Idol", "Profession: Singer"),
          add.lines = list(c("Controls", rep("Yes", 7))),
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("*", "**", "***"),
          notes = c("*p<0.1; **p<0.05; ***p<0.01",
                    "Models (1)-(4) test Origin, Age, Follower, Online_Activity separately. Model (5) includes all variables.",
                    "Origin = 1 if the celebrity is from HK or Taiwan, 0 if from mainland China.",
                    "Follower and activity are measured using log transformation.",
                    "College Degree = 1 if the celebrity achieves college degree, 0 if high school or below.",
                    "Reference class for profession = other, including influencers, hosts and directors."),
          notes.align = "l",
          notes.append = FALSE,
          no.space = TRUE,
          out = paste0(output_path, "table_10.html"))
