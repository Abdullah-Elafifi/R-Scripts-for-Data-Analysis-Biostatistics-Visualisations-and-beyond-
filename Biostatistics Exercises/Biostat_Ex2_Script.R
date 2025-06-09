
library(readxl)
biostat2 <- read_excel("EX Biostat P2.xlsx")  

GenderGrade <- table(biostat2$Gender, biostat2$Grade)

total_sample <- sum(GenderGrade)
females_with_grade3 <- GenderGrade["Female", "III"]
percentage_females_grade3_total <- (females_with_grade3 / total_sample) * 100

total_females <- sum(GenderGrade["Female", ])
percentage_grade3_within_females <- (females_with_grade3 / total_females) * 100

GenderGrade_with_totals <- addmargins(GenderGrade, margin = 2) 
gender_totals <- rowSums(GenderGrade)
total_males <- gender_totals["Male"]
total_females <- gender_totals["Female"]

GenderGrade_with_totals

chi_test <- chisq.test(GenderGrade)
chi_test_result <- if (chi_test$p.value < 0.05) {
  "There is a significant association between gender and tumor grade."
} else {
  "There is no significant association between gender and tumor grade."
}

print(paste("Percentage of females with Grade III tumor in the total sample:", percentage_females_grade3_total))
print(paste("Percentage of Grade III tumors within females:", percentage_grade3_within_females))
print(chi_test_result)
