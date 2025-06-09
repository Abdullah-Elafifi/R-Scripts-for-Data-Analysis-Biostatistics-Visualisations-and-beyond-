library(dplyr)
library(ggplot2)
library(readxl)

biostat1 <- read.csv("EX Biostat P1.csv") #I have provided the absolute path here (it may cause an error for you!)

shapiro_age <- shapiro.test(biostat1$Age)
shapiro_hosp_days <- shapiro.test(biostat1$hospitalization_days) #I have changed the hospitalization days to be in the snake case of hospitalization_ days

cat("Shapiro-Wilk Test for Age: p-value =", shapiro_age$p.value, "\n")
cat("Shapiro-Wilk Test for Hospitalization Days: p-value =", shapiro_hosp_days$p.value, "\n")

age_summary <- ifelse(shapiro_age$p.value > 0.05,
                      paste0("Mean ± SD: ", round(mean(biostat1$Age, na.rm = TRUE), 2), " ± ", round(sd(biostat1$Age, na.rm = TRUE), 2)),
                      paste0("Median (IQR): ", round(median(biostat1$Age, na.rm = TRUE), 2), " (", round(IQR(biostat1$Age, na.rm = TRUE), 2), ")"))

hosp_days_summary <- ifelse(shapiro_hosp_days$p.value > 0.05,
                            paste0("Mean ± SD: ", round(mean(biostat1$hospitalization_days, na.rm = TRUE), 2), " ± ", round(sd(biostat1$hospitalization_days, na.rm = TRUE), 2)),
                            paste0("Median (IQR): ", round(median(biostat1$hospitalization_days, na.rm = TRUE), 2), " (", round(IQR(biostat1$hospitalization_days, na.rm = TRUE), 2), ")"))

cat("Age Descriptive Statistics:", age_summary, "\n")
cat("Hospitalization Days Descriptive Statistics:", hosp_days_summary, "\n")




# Age Boxplot
ggplot(biostat1, aes(y = Age)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot for Age", y = "Age") +
  theme_minimal()

# Hospitalization Days Boxplot
ggplot(biostat1, aes(y = hospitalization_days)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot for Hospitalization Days", y = "Hospitalization Days") +
  theme_minimal()

#Trying to detect the outliers
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(which(x < lower_bound | x > upper_bound))
}

outliers_age <- find_outliers(biostat1$Age)
outliers_hosp_days <- find_outliers(biostat1$hospitalization_days)

cat("Outliers in Age:", outliers_age, "\n")
cat("Outliers in Hospitalization Days:", outliers_hosp_days, "\n")

biostat1_N <- biostat1 %>%
  filter(!(row_number() %in% c(outliers_age, outliers_hosp_days)))

shapiro_age_new <- shapiro.test(biostat1_N$Age)
shapiro_hosp_days_new <- shapiro.test(biostat1_N$hospitalization_days)

cat("New Shapiro-Wilk Test for Age: p-value =", shapiro_age_new$p.value, "\n")
cat("New Shapiro-Wilk Test for Hospitalization Days: p-value =", shapiro_hosp_days_new$p.value, "\n")

age_summary_new <- ifelse(shapiro_age_new$p.value > 0.05,
                          paste0("Mean ± SD: ", round(mean(biostat1_N$Age, na.rm = TRUE), 2), " ± ", round(sd(biostat1_N$Age, na.rm = TRUE), 2)),
                          paste0("Median (IQR): ", round(median(biostat1_N$Age, na.rm = TRUE), 2), " (", round(IQR(biostat1_N$Age, na.rm = TRUE), 2), ")"))

hosp_days_summary_new <- ifelse(shapiro_hosp_days_new$p.value > 0.05,
                                paste0("Mean ± SD: ", round(mean(biostat1_N$hospitalization_days, na.rm = TRUE), 2), " ± ", round(sd(biostat1_N$hospitalization_days, na.rm = TRUE), 2)),
                                paste0("Median (IQR): ", round(median(biostat1_N$hospitalization_days, na.rm = TRUE), 2), " (", round(IQR(biostat1_N$hospitalization_days, na.rm = TRUE), 2), ")"))

cat("New Age Descriptive Statistics:", age_summary_new, "\n")
cat("New Hospitalization Days Descriptive Statistics:", hosp_days_summary_new, "\n")
