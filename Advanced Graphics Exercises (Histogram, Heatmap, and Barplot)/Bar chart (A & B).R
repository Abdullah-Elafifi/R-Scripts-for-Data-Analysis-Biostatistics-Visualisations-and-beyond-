#Bar chart [A]

library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(RColorBrewer)

stroke_data <- read_excel("Stroke.xlsx")  #I have provided the absolute path here (it may cause an error for you!)

strokemelted <- melt(stroke_data, id.vars = "Group")

strokesummary <- strokemelted %>%
  group_by(Group, variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE)) %>%
  arrange(desc(mean_value)) 

ggplot(strokesummary, aes(x = reorder(variable, -mean_value), y = mean_value, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value, color = Group),
                position = position_dodge(0.9), size = 1, width = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(x = NULL, y = "mg/dl") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#Bar chart [B]

strokegrouped <- strokemelted %>%
  group_by(Group, variable)

strokesummary2 <- strokegrouped %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  group_by(variable) %>%
  mutate(percentage = mean_value / sum(mean_value) * 100)

ggplot(strokesummary2, aes(x = variable, y = percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = "Proportion") +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



