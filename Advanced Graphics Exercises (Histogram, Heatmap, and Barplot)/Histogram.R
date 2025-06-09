#Histogram 
library(ggplot2)
library(readxl)

new_data <- read_excel("RCC.xlsx") #I have provided the absolute path here (it may cause an error for you!)

p <- ggplot(new_data, aes(x = MALAT1, fill = as.factor(Grade)))

p <- p + geom_histogram(binwidth = 3, 
                        color = "white", 
                        position = "dodge", 
                        alpha = 0.7)

p <- p + geom_density(aes(y = ..count..), 
                      color = NA, 
                      fill = "gray", 
                      alpha = 0.7)
p <- p + facet_wrap(~ Side)

p <- p + scale_fill_manual(values = c("purple", "pink", "blue", "green", "orange")) +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "gray80")) +
  labs(title = "Histogram of MALAT1 Expression by Grade",
       x = "MALAT1 Expression",
       y = "Count",
       fill = "Grade")
print(p)
