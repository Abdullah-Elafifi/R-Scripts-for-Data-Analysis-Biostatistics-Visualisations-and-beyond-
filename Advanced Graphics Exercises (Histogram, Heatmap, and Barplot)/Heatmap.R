#Heatmap
library(readxl)
library(gplots)

new_data <- read_excel("LP.xlsx") #I have provided the absolute path here (it may cause an error for you!)
lp_matrix <- as.matrix(new_data[, -1])  
rownames(lp_matrix) <- new_data[[1]]   

row_sd <- apply(lp_matrix, 1, sd)
threshold <- quantile(row_sd, 0.75)  
lp_matrix_filtered <- lp_matrix[row_sd >= threshold, ]

heatmap.2(
  lp_matrix_filtered,             
  scale = "row",                   
  col = bluered(200),              
  trace = "none",                  
  density.info = "none",            
  keysize = 2,                      
  labRow = NULL,                   
  labCol = colnames(lp_matrix),     
  cexCol = 1.5,                    
  margins = c(8, 6),               
  main = "Heatmap of Lipid Profile"  
)

