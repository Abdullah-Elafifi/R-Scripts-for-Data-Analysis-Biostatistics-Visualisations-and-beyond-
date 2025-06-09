
#creating the data frame columns
new_data <- data.frame(A = c(rep('patients', 30), rep('controls', 20)), 
                       B = c(sample(45),rep(NA,5)),
                       C = c(rnorm(50, mean=80, sd=2)))

# Rounding the column C to round number 
new_data["D"] = round(new_data$C)
  
# Creating new ABD Data frame (containing A,B, and D columns only)
ABD = data.frame(A = new_data$A, B = new_data$B, D = new_data$D)

# simple EDA and removing the missing data (NA)
is.na(ABD)
ABDnew = data.frame(na.omit(ABD))

#Creating the matrix
#I love snake_case declaration btw
new_matrix = matrix(data = c(rnorm(20)),nrow = 4,ncol= 5) 