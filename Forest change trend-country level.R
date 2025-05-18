# Load required libraries
library(tidyverse)
library(ggplot2)

# Input data
data <- tibble(
  Country = c("Togo", "Benin", "Nigeria", "Cameroon"),
  `2000` = c(2.2405, 4.8844, 21.6564, 35.7728),
  `2005` = c(2.215, 4.8427, 21.5731, 35.775),
  `2010` = c(2.2123, 4.8237, 21.5462, 35.7796),
  `2015` = c(2.1627, 4.6828, 21.0318, 35.4358),
  `2020` = c(2.1131, 4.5478, 20.4099, 35.048),
  `2022` = c(2.111, 4.547, 20.106, 34.75)
)

# Convert to long format
data_long <- data %>%
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Forest_cover")

# Calculate percentages relative to 2000
data_percent <- data_long %>%
  group_by(Country) %>%
  mutate(
    Percent = Forest_cover / first(Forest_cover) * 100,
    Year = as.numeric(Year)
  )

# Define the exact years we want on the x-axis
year_labels <- c(2000, 2005, 2010, 2015, 2020, 2022)

# Create the plot
ggplot(data_percent, aes(x = Year, y = Percent, color = Country)) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  geom_line(size = 1) +
  geom_point(size = 3) +
  
  # Label all specific years on the x-axis
  scale_x_continuous(breaks = year_labels, labels = year_labels) +
  scale_y_continuous(limits = c(92, 102), breaks = seq(92, 100, by = 2)) +
  labs(
    title = "Forest Cover Trend (2000-2022)",
    subtitle = "Year 2000 as baseline (100%)",
    x = "Year",
    y = "Forest Cover (%)",
    color = "Country"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 18),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    # Rotate x-axis labels 45 degrees
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = "right"
  )

# Save the plot
ggsave("forest_cover_trend.png", width = 12, height = 8, dpi = 300)

