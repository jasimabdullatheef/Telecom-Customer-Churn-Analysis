# Week 1 - Data Cleaning and Preliminary Analysis with R
# Virtual R Data Analyst Internship
# Dataset: Telecom Customer Churn

# 1. Packages
packages <- c("dplyr", "ggplot2", "tidyr", "readr", "scales")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# 2. Import
data <- read_csv("telecom_customer_churn.csv", show_col_types = FALSE)

# 3. Initial inspection
head(data)
str(data)
dim(data)
summary(data)

# Missing-value counts
missing_summary <- data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing")

print(missing_summary %>% filter(Missing > 0) %>% arrange(desc(Missing)))

# Duplicate check
sum(duplicated(data))

# 4. Data cleaning
cleaned <- data

# Business-logic based missing-value treatment:
# Offer blank = no offer; internet-service fields blank = no internet service;
# Multiple Lines blank = no phone service; churn fields blank = not churned.
cleaned$Offer <- replace_na(cleaned$Offer, "No Offer")
cleaned$Multiple.Lines <- ifelse(is.na(cleaned$Multiple.Lines) &
                                   cleaned$`Phone Service` == "No",
                                 "No Phone Service",
                                 cleaned$Multiple.Lines)
cleaned$Internet.Type <- ifelse(is.na(cleaned$Internet.Type) &
                                  cleaned$`Internet Service` == "No",
                                "No Internet Service",
                                cleaned$Internet.Type)

internet_cols <- c("Online Security","Online Backup","Device Protection Plan",
                   "Premium Tech Support","Streaming TV","Streaming Movies",
                   "Streaming Music","Unlimited Data")
for (v in internet_cols) {
  cleaned[[v]] <- ifelse(is.na(cleaned[[v]]) &
                           cleaned$`Internet Service` == "No",
                         "No Internet Service", cleaned[[v]])
}

cleaned$`Churn Category` <- replace_na(cleaned$`Churn Category`, "Not Applicable")
cleaned$`Churn Reason` <- replace_na(cleaned$`Churn Reason`, "Not Applicable")

# Numerical missing values: median imputation, documented as a robust
# central-value treatment for the remaining missing numeric observations.
num_impute <- c("Avg Monthly Long Distance Charges",
                "Avg Monthly GB Download")
for (v in num_impute) {
  med <- median(cleaned[[v]], na.rm = TRUE)
  cleaned[[v]][is.na(cleaned[[v]])] <- med
}

# Invalid negative monthly charges are treated as data-quality errors.
cleaned$`Monthly Charge`[cleaned$`Monthly Charge` < 0] <- NA
cleaned$`Monthly Charge`[is.na(cleaned$`Monthly Charge`)] <-
  median(cleaned$`Monthly Charge`, na.rm = TRUE)

# Re-check missing values
print(colSums(is.na(cleaned)))

# 5. Outlier detection using IQR
numeric_vars <- names(cleaned)[sapply(cleaned, is.numeric)]

iqr_outliers <- lapply(numeric_vars, function(v) {
  x <- cleaned[[v]]
  q1 <- quantile(x, .25, na.rm = TRUE)
  q3 <- quantile(x, .75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  sum(x < lower | x > upper, na.rm = TRUE)
})
outlier_summary <- data.frame(
  Variable = numeric_vars,
  IQR_Outliers = unlist(iqr_outliers)
)
print(outlier_summary)

# 6. Normalization (min-max) for selected numeric variables
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
cleaned$Monthly_Charge_Normalized <- normalize(cleaned$`Monthly Charge`)
cleaned$Tenure_Normalized <- normalize(cleaned$`Tenure in Months`)
cleaned$Total_Revenue_Normalized <- normalize(cleaned$`Total Revenue`)

# 7. Encoding categorical variables
cleaned$Gender_Code <- as.integer(factor(cleaned$Gender))
cleaned$Contract_Code <- as.integer(factor(cleaned$Contract))
cleaned$Customer_Status_Code <- as.integer(factor(cleaned$`Customer Status`))

# 8. Descriptive statistics
print(summary(cleaned))

# 9. Correlation matrix for selected numeric variables
cor_data <- cleaned %>%
  select(Age, `Tenure in Months`, `Monthly Charge`, `Total Charges`,
         `Total Revenue`, `Total Refunds`,
         `Total Extra Data Charges`, `Total Long Distance Charges`)
print(cor(cor_data, use = "complete.obs"))

# 10. Visualizations
ggplot(cleaned, aes(x = `Customer Status`)) +
  geom_bar() +
  labs(title = "Customer Status Distribution",
       x = "Customer Status", y = "Number of Customers") +
  theme_minimal()

ggplot(cleaned, aes(x = Contract, fill = `Customer Status`)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Customer Status by Contract Type",
       x = "Contract", y = "Proportion") +
  theme_minimal()

ggplot(cleaned, aes(x = `Monthly Charge`)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Monthly Charges",
       x = "Monthly Charge", y = "Count") +
  theme_minimal()

ggplot(cleaned, aes(x = `Tenure in Months`, y = `Total Revenue`,
                    color = `Customer Status`)) +
  geom_point(alpha = 0.4) +
  labs(title = "Tenure vs Total Revenue",
       x = "Tenure in Months", y = "Total Revenue") +
  theme_minimal()

# 11. Export cleaned data
write_csv(cleaned, "telecom_customer_churn_cleaned.csv")
