library(tidyverse)
library(ggplot2)

# Input data
data <- tibble(
  Forest_Patch = c("Agou", "Koui", "Elavagnon-Todji", "Hlanzoun", "Ewè-Adakplamè", "Ikot swamp forest", "Iko", "Mbangassina", "Ngam-Kondomeyos"),
  `2000` = c(2355.12, 133.2, 697.77, 547.38, 619.2, 1270.8, 3557.7, 1012.23, 1283.58),
  `2005` = c(2346.12, 130.86, 691.56, 558.18, 594.72, 1271.7, 3559.23, 1009.98, 1285.02),
  `2010` = c(2346.03, 130.95, 680.76, 568.44, 586.89, 1270.71, 3554.19, 1007.01, 1285.02),
  `2015` = c(2327.22, 129.6, 662.31, 577.44, 573.48, 1254.51, 3555.63, 997.83, 1284.93),
  `2020` = c(2281.68, 126.63, 649.8, 559.26, 528.84, 1253.07, 3428.64, 993.15, 1284.84),
  `2022` = c(2280.85, 125.63, 648.41, 548.61, 505.42, 1252.97, 3423.52, 991.23, 1283.9)
)


# Convert to long format
data_long <- data %>%
  pivot_longer(cols = -Forest_Patch, names_to = "Year", values_to = "Area")

# Calculate percentages relative to 2000
data_percent <- data_long %>%
  group_by(Forest_Patch) %>%
  mutate(
    Percent = Area / first(Area) * 100,
    Year = as.numeric(Year)
  )

# Create a list of plots, one for each forest patch
plot_list <- lapply(unique(data_percent$Forest_Patch), function(patch) {
  ggplot(data_percent %>% filter(Forest_Patch == patch), aes(x = Year, y = Percent)) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
    geom_line(size = 0.5, color = "black") +
    geom_point(size = 2.5, color = "black") +
    
    scale_x_continuous(breaks = seq(2000, 2022, by = 5)) +
    scale_y_continuous(limits = c(80, 110)) +
    labs(
      title = paste(patch),
      x = "Year",
      y = "Forest Cover (%)"
    ) +
    theme_classic(base_size = 10) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8)
    )
})


# Arrange plots in a grid
library(gridExtra)
grid.arrange(grobs = plot_list, ncol = 3)

