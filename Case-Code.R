

#install packages and environment
library("broom")
library(dplyr)
library("tidyverse")
library(readr)

#read file into dataset
claims <- read.csv("claims.csv")

# Question 3
# Sum of the TREAT/CONTROL
sum(claims$treat == '1') # calculate sum of treatment group
sum(claims$treat == '0') # calculate sum of control group


# The number of people in the treatment group who complete the initial screening
sum(claims$treat== 1 &claims$completed_screening_nomiss_2016 ==1)

# Question 4
# Define a list of pre-randomization variables to analyze
pre_randomization_vars <- c("covg_0715_0716","covg_1015_0716","diabetes_1015_0716", "hyperlipidemia_1015_0716","hypertension_1015_0716", "pcp_any_office_1015_0716", "pcp_any_visits_1015_0716","pcp_total_office_1015_0716", "pcp_total_visits_1015_0716", "pos_er_critical_1015_0716","pos_hospital_1015_0716","pos_office_outpatient_1015_0716", "spendOff_0715_0716", "spendRx_0715_0716", "spendHosp_0715_0716","spend_0715_0716", "nonzero_spend_0715_0716")

# Initialize an empty data frame for results
results_table <- data.frame(
  "Variable Description" = character(),
  "Control Group Mean" = numeric(),
  "Treatment Group Mean" = numeric(),
  "P-value" = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each variable, calculate means and perform linear regression
for (var in pre_randomization_vars) {
  
  # Calculate control and treatment group means
  control_mean <- mean(claims[[var]][claims$treat == 0], na.rm = TRUE)
  treatment_mean <- mean(claims[[var]][claims$treat == 1], na.rm = TRUE)
  
  # Perform linear regression
  model <- lm(claims[[var]] ~ treat, data = claims)
  p_value <- summary(model)$coefficients[2, 4]
  
  # Append the results to the table
  results_table <- rbind(
    results_table,
    data.frame(
      "Variable Description" = var,
      "Control Group Mean" = round(control_mean, 2),
      "Treatment Group Mean" = round(treatment_mean, 2),
      "P-value" = round(p_value, 4)
    )
  )
}

# Print the results table
results_table




# Question 5
#with demographic control first year after randomization
model_hosp_1 <- lm(spendHosp_0816_0717~treat+male+white+age37_49+age50, data = claims)
model_off_1<- lm(spendOff_0816_0717~treat+male+white+age37_49+age50, data = claims)
model_rx_1 <- lm(spendRx_0816_0717~treat+male+white+age37_49+age50, data = claims)
model_total_1 <- lm(spend_0816_0717~treat+male+white+age37_49+age50, data = claims)
model_nonzero_1 <- lm(nonzero_spend_0816_0717 ~ treat+male+white+age37_49+age50, data = claims)
model_covg_1 <- lm(covg_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_diabetes_1 <- lm(diabetes_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_hyperlipidemia_1 <- lm(hyperlipidemia_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_hypertension_1 <- lm(hypertension_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_pcp_any_office_1 <- lm(pcp_any_office_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_pcp_any_visits_1 <- lm(pcp_any_visits_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_pcp_total_office_1 <- lm(pcp_total_office_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_pcp_total_visits_1 <- lm(pcp_total_visits_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_pos_er_critical_1 <- lm(pos_er_critical_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_pos_hospital_1 <- lm(pos_hospital_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)
model_pos_office_outpatient_1 <- lm(pos_office_outpatient_0816_0717 ~ treat + male + white + age37_49 + age50, data = claims)

#combine the results of linear regression into dataframe "result1"  and adding a new variable columns
results_1 <- bind_rows(
  tidy(model_off_1) %>% mutate(Variable = "spendOff_0816_0717",),
  tidy(model_rx_1) %>% mutate(Variable = "spendRx_0816_0717"),
  tidy(model_hosp_1) %>% mutate(Variable = "spendHosp_0816_0717"),
  tidy(model_total_1) %>% mutate(Variable = "spend_0816_0717"),
  tidy(model_nonzero_1) %>% mutate(Variable = "nonzero_spend_0816_0717"),
  tidy(model_covg_1) %>% mutate(Variable = "covg_0816_0717"),
  tidy(model_diabetes_1) %>% mutate(Variable = "diabetes_0816_0717"),
  tidy(model_hyperlipidemia_1) %>% mutate(Variable = "hyperlipidemia_0816_0717"),
  tidy(model_hypertension_1) %>% mutate(Variable = "hypertension_0816_0717"),
  tidy(model_pcp_any_office_1) %>% mutate(Variable = "pcp_any_office_0816_0717"),
  tidy(model_pcp_any_visits_1) %>% mutate(Variable = "pcp_any_visits_0816_0717"),
  tidy(model_pcp_total_office_1) %>% mutate(Variable = "pcp_total_office_0816_0717"),
  tidy(model_pcp_total_visits_1) %>% mutate(Variable = "pcp_total_visits_0816_0717"),
  tidy(model_pos_er_critical_1) %>% mutate(Variable = "pos_er_critical_0816_0717"),
  tidy(model_pos_hospital_1) %>% mutate(Variable = "pos_hospital_0816_0717"),
  tidy(model_pos_office_outpatient_1) %>% mutate(Variable = "pos_office_outpatient_0816_0717")
  
)

#filter and extract the estimated difference between treatment and control groups with demographic control
results_2 <-results_1 %>%
  filter(term == "treat") %>%
  select(Variable, estimate_difference_with = estimate, std.error)


#without demographic control
model_off_1_without <- lm(spendHosp_0816_0717~treat, data = claims)
model_rx_1_without <-lm(spendOff_0816_0717~treat, data = claims)
model_hosp_1_without <-lm(spendRx_0816_0717~treat, data = claims)
model_total_1_without <-lm(spend_0816_0717~treat, data = claims)
model_nonzero_1_without <- lm(nonzero_spend_0816_0717 ~ treat, data = claims)
model_covg_1_without <- lm(covg_0816_0717 ~ treat, data = claims)
model_diabetes_1_without <- lm(diabetes_0816_0717 ~ treat, data = claims)
model_hyperlipidemia_1_without <- lm(hyperlipidemia_0816_0717 ~ treat, data = claims)
model_hypertension_1_without <- lm(hypertension_0816_0717 ~ treat, data = claims)
model_pcp_any_office_1_without <- lm(pcp_any_office_0816_0717 ~ treat, data = claims)
model_pcp_any_visits_1_without <- lm(pcp_any_visits_0816_0717 ~ treat, data = claims)
model_pcp_total_office_1_without <- lm(pcp_total_office_0816_0717 ~ treat, data = claims)
model_pcp_total_visits_1_without <- lm(pcp_total_visits_0816_0717 ~ treat, data = claims)
model_pos_er_critical_1_without <- lm(pos_er_critical_0816_0717 ~ treat, data = claims)
model_pos_hospital_1_without <- lm(pos_hospital_0816_0717 ~ treat, data = claims)
model_pos_office_outpatient_1_without <- lm(pos_office_outpatient_0816_0717 ~ treat, data = claims)


#combine the results of linear regression into dataframe "result3" and adding a new variable columns
results_3 <- bind_rows(
  tidy(model_off_1_without) %>% mutate(Variable = "spendOff_0816_0717",),
  tidy(model_rx_1_without) %>% mutate(Variable = "spendRx_0816_0717"),
  tidy(model_hosp_1_without) %>% mutate(Variable = "spendHosp_0816_0717"),
  tidy(model_total_1_without) %>% mutate(Variable = "spend_0816_0717"),
  tidy(model_nonzero_1_without) %>% mutate(Variable = "nonzero_spend_0816_0717"),
  tidy(model_covg_1_without) %>% mutate(Variable = "covg_0816_0717"),
  tidy(model_diabetes_1_without) %>% mutate(Variable = "diabetes_0816_0717"),
  tidy(model_hyperlipidemia_1_without) %>% mutate(Variable = "hyperlipidemia_0816_0717"),
  tidy(model_hypertension_1_without) %>% mutate(Variable = "hypertension_0816_0717"),
  tidy(model_pcp_any_office_1_without) %>% mutate(Variable = "pcp_any_office_0816_0717"),
  tidy(model_pcp_any_visits_1_without) %>% mutate(Variable = "pcp_any_visits_0816_0717"),
  tidy(model_pcp_total_office_1_without) %>% mutate(Variable = "pcp_total_office_0816_0717"),
  tidy(model_pcp_total_visits_1_without) %>% mutate(Variable = "pcp_total_visits_0816_0717"),
  tidy(model_pos_er_critical_1_without) %>% mutate(Variable = "pos_er_critical_0816_0717"),
  tidy(model_pos_hospital_1_without) %>% mutate(Variable = "pos_hospital_0816_0717"),
  tidy(model_pos_office_outpatient_1_without) %>% mutate(Variable = "pos_office_outpatient_0816_0717")
  
)
#filter and extract the estimated difference between treatment and control groups without demographic control
results_4 <- results_3 %>%
  filter(term == "treat") %>%
  select(Variable, estimate_difference_without = estimate, std.error_without= std.error)  

# merge estimated difference between treatment and control group without demographic with estimated difference with demographic
df <- merge(results_4, results_2, by = "Variable")

#round the estimate result and change it to character type
for (col in names(df)){
  if(is.numeric(df[[col]])) {
    df[[col]] <- round(df[[col]],3)
    df[[col]] <- as.character(df[[col]])
  }
}
#combine std.error with the group estimate
# Add parentheses around the std.error values
df$std.error_without <- paste0("(", df$std.error_without, ")")
df$std.error<- paste0("(", df$std.error, ")")

# Merge the two columns into estimate difference column
df$estimate_difference_without <- paste(df$estimate_difference_without, df$std.error_without)
df$estimate_difference_with <- paste(df$estimate_difference_with, df$std.error)

#delete the std error column
df <- df %>% select (-std.error,-std.error_without)
print(df)





# Question 6
#filter and count the number of particpants in the first year after randomization

claims_clean <- claims %>%
  filter(claims$hra_c_yr1 == 1 |  claims$hra_c_yr1 == 0)
sum(claims_clean$hra_c_yr1 ==1)
sum(claims_clean$hra_c_yr1 ==0)

#calculate estimated difference between Participant and non participant without demographic control using linear regression
model_hosp_part <-tidy(lm(spendHosp_0816_0717~hra_c_yr1, data = claims_clean))
model_off_part <- tidy(lm(spendOff_0816_0717~hra_c_yr1, data = claims_clean))
model_rx_part <- tidy(lm(spendRx_0816_0717~hra_c_yr1, data = claims_clean))
model_total_part <- tidy(lm(spend_0816_0717~hra_c_yr1, data = claims_clean))
model_nonzero_part <- tidy(lm(nonzero_spend_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_covg_part <- tidy(lm(covg_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_diabetes_part <- tidy(lm(diabetes_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_hyperlipidemia_part <- tidy(lm(hyperlipidemia_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_hypertension_part <- tidy(lm(hypertension_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_pcp_any_office_part <- tidy(lm(pcp_any_office_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_pcp_any_visits_part <- tidy(lm(pcp_any_visits_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_pcp_total_office_part <- tidy(lm(pcp_total_office_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_pcp_total_visits_part <- tidy(lm(pcp_total_visits_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_pos_er_critical_part <- tidy(lm(pos_er_critical_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_pos_hospital_part <- tidy(lm(pos_hospital_0816_0717 ~ hra_c_yr1, data = claims_clean))
model_pos_office_outpatient_part <- tidy(lm(pos_office_outpatient_0816_0717 ~ hra_c_yr1, data = claims_clean))


results_5 <- bind_rows(
  model_hosp_part %>% mutate(Variable = "spendHosp_0816_0717"),
  model_off_part %>% mutate(Variable = "spendOff_0816_0717"),
  model_rx_part %>% mutate(Variable = "spendRx_0816_0717"),
  model_total_part %>% mutate(Variable = "spend_0816_0717"),
  model_nonzero_part %>% mutate(Variable = "nonzero_spend_0816_0717"),
  model_covg_part %>% mutate(Variable = "covg_0816_0717"),
  model_diabetes_part %>% mutate(Variable = "diabetes_0816_0717"),
  model_hyperlipidemia_part %>% mutate(Variable = "hyperlipidemia_0816_0717"),
  model_hypertension_part %>% mutate(Variable = "hypertension_0816_0717"),
  model_pcp_any_office_part %>% mutate(Variable = "pcp_any_office_0816_0717"),
  model_pcp_any_visits_part %>% mutate(Variable = "pcp_any_visits_0816_0717"),
  model_pcp_total_office_part %>% mutate(Variable = "pcp_total_office_0816_0717"),
  model_pcp_total_visits_part %>% mutate(Variable = "pcp_total_visits_0816_0717"),
  model_pos_er_critical_part %>% mutate(Variable = "pos_er_critical_0816_0717"),
  model_pos_hospital_part %>% mutate(Variable = "pos_hospital_0816_0717"),
  model_pos_office_outpatient_part %>% mutate(Variable = "pos_office_outpatient_0816_0717"))


results_6 <- results_5 %>%
  filter(term == "hra_c_yr1") %>%
  select(Variable, estimate_difference_without = estimate, std.error_without= std.error)

#Participant and non participant difference with demographic control
model_hosp_part_control <-tidy(lm(spendHosp_0816_0717~hra_c_yr1+male+white+age37_49+age50, data = claims_clean))
model_off_part_control <-tidy(lm(spendOff_0816_0717~hra_c_yr1+male+white+age37_49+age50, data = claims_clean))
model_rx_part_control <- tidy(lm(spendRx_0816_0717~hra_c_yr1+male+white+age37_49+age50, data = claims_clean))
model_total_part_control <- tidy(lm(spend_0816_0717~hra_c_yr1+male+white+age37_49+age50, data = claims_clean))
model_nonzero_part_control <- tidy(lm(nonzero_spend_0816_0717~hra_c_yr1+male+white+age37_49+age50, data = claims_clean))
model_covg_part_control <- tidy(lm(covg_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_diabetes_part_control <- tidy(lm(diabetes_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_hyperlipidemia_part_control <- tidy(lm(hyperlipidemia_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_hypertension_part_control <- tidy(lm(hypertension_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_pcp_any_office_part_control <- tidy(lm(pcp_any_office_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_pcp_any_visits_part_control <- tidy(lm(pcp_any_visits_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_pcp_total_office_part_control <- tidy(lm(pcp_total_office_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_pcp_total_visits_part_control <- tidy(lm(pcp_total_visits_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_pos_er_critical_part_control <- tidy(lm(pos_er_critical_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_pos_hospital_part_control <- tidy(lm(pos_hospital_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))
model_pos_office_outpatient_part_control <- tidy(lm(pos_office_outpatient_0816_0717 ~ hra_c_yr1 + male + white + age37_49 + age50, data = claims_clean))

results_7 <- bind_rows(
  model_hosp_part_control  %>% mutate(Variable = "spendHosp_0816_0717"),
  model_off_part_control  %>% mutate(Variable = "spendOff_0816_0717"),
  model_rx_part_control  %>% mutate(Variable = "spendRx_0816_0717"),
  model_total_part_control  %>% mutate(Variable = "spend_0816_0717"),
  model_nonzero_part_control  %>% mutate(Variable = "nonzero_spend_0816_0717"),
  model_covg_part_control %>% mutate(Variable = "covg_0816_0717"),
  model_diabetes_part_control %>% mutate(Variable = "diabetes_0816_0717"),
  model_hyperlipidemia_part_control %>% mutate(Variable = "hyperlipidemia_0816_0717"),
  model_hypertension_part_control %>% mutate(Variable = "hypertension_0816_0717"),
  model_pcp_any_office_part_control %>% mutate(Variable = "pcp_any_office_0816_0717"),
  model_pcp_any_visits_part_control %>% mutate(Variable = "pcp_any_visits_0816_0717"),
  model_pcp_total_office_part_control %>% mutate(Variable = "pcp_total_office_0816_0717"),
  model_pcp_total_visits_part_control %>% mutate(Variable = "pcp_total_visits_0816_0717"),
  model_pos_er_critical_part_control %>% mutate(Variable = "pos_er_critical_0816_0717"),
  model_pos_hospital_part_control %>% mutate(Variable = "pos_hospital_0816_0717"),
  model_pos_office_outpatient_part_control %>% mutate(Variable = "pos_office_outpatient_0816_0717")
  
  )
results_8 <- results_7 %>%
  filter(term == "hra_c_yr1") %>%
  select(Variable, estimate_difference_with = estimate, std.error_with= std.error)


# merge estimated difference between treatment and control group without demographic with estimated difference with demographic into one dataframe df_1
df_1 <- merge(results_6, results_8, by = "Variable")

#round the estimate result
for (col in names(df_1)){
  if(is.numeric(df_1[[col]])) {
    df_1[[col]] <- round(df_1[[col]],3)
    df_1[[col]] <- as.character(df_1[[col]])
  }
}
#combine std.error with the group estimate
# Add parentheses around the std.error values
df_1$std.error_without <- paste0("(", df_1$std.error_without, ")")
df_1$std.error_with<- paste0("(", df_1$std.error_with, ")")

# Merge the two columns into the estimate difference column
df_1$estimate_difference_without <- paste(df_1$estimate_difference_without, df_1$std.error_without)
df_1$estimate_difference_with <- paste(df_1$estimate_difference_with, df_1$std.error_with)

#delete the std error column
df_1 <- df_1 %>% select (-std.error_with,-std.error_without)
print(df_1)
