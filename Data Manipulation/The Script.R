#Q1
new_data = read.csv("iris.csv")
filtered_for_iris = subset(new_data, Sepal.Length > 5 & Species == "setosa")
selected_columns = filtered_iris[, grep("Sepal", names(filtered_iris))]
selected_columns$Sepal.Ratio = selected_columns$Sepal.Length / selected_columns$Sepal.Width

result = selected_columns[order(-selected_columns$Sepal.Ratio), ]

#Q2
patients_not_assigned <- anti_join(patients, departments, by = "PatientID")
