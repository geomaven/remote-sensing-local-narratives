# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Create the dataframe directly from the provided 10-year data
forest_data_10yr <- read.table(text = "Forest patch	Reason for change_10ys [1]	percentage
Agou	Logging	53.1
Agou	Slash-and-burn	14.2
Agou	Population growth	8.5
Agou	Bush fires	7.7
Agou	Nothing to report	4.3
Agou	Fragmentation	2.8
Agou	Drought	2.6
Agou	Effective law enforcement	1.4
Agou	Charcoal production	1.1
Agou	Forest regrowth	1.1
Agou	No change	1.1
Agou	Clearing for Constructions	0.6
Agou	NTFP collection	0.6
Agou	Forest restoration	0.3
Agou	Lack of protection measures	0.3
Agou	Less pressure	0.3
Elavagnon-Todji	Bush fires	41.5
Elavagnon-Todji	Logging	31.7
Elavagnon-Todji	Fragmentation	8.8
Elavagnon-Todji	Slash-and-burn	5.2
Elavagnon-Todji	Nothing to report	3.9
Elavagnon-Todji	Charcoal production	2.9
Elavagnon-Todji	Population growth	1.6
Elavagnon-Todji	Effective law enforcement	1.3
Elavagnon-Todji	Clearing for Constructions	0.7
Elavagnon-Todji	No change	0.7
Elavagnon-Todji	Behavioral factors	0.3
Elavagnon-Todji	Drought	0.3
Elavagnon-Todji	Forest regrowth	0.3
Elavagnon-Todji	Lack of protection measures	0.3
Elavagnon-Todji	Pastoralism	0.3
Ewè-Adakplamè	Nothing to report	27
Ewè-Adakplamè	Bush fires	25.1
Ewè-Adakplamè	Logging	14
Ewè-Adakplamè	Population growth	10.7
Ewè-Adakplamè	Slash-and-burn	9.1
Ewè-Adakplamè	Fragmentation	3.9
Ewè-Adakplamè	NTFP collection	2.3
Ewè-Adakplamè	No change	2
Ewè-Adakplamè	Effective law enforcement	1.6
Ewè-Adakplamè	Clearing for Constructions	1.3
Ewè-Adakplamè	Hunting	0.7
Ewè-Adakplamè	Lack of protection measures	0.7
Ewè-Adakplamè	Local governance failure	0.7
Ewè-Adakplamè	Charcoal production	0.3
Ewè-Adakplamè	Customary rules	0.3
Ewè-Adakplamè	Restricted access	0.3
Hlanzoun	River increases	27.2
Hlanzoun	Effective law enforcement	15.1
Hlanzoun	Nothing to report	13.5
Hlanzoun	Logging	12.8
Hlanzoun	Slash-and-burn	8.7
Hlanzoun	Population growth	5.4
Hlanzoun	No change	4.2
Hlanzoun	Forest regrowth	3.8
Hlanzoun	Less pressure	1.3
Hlanzoun	NTFP collection	1.3
Hlanzoun	Bush fires	1
Hlanzoun	Charcoal production	1
Hlanzoun	Customary rules	1
Hlanzoun	Drought	1
Hlanzoun	Local governance failure	1
Hlanzoun	Firewood collection	0.3
Hlanzoun	Forest restoration	0.3
Hlanzoun	Fragmentation	0.3
Hlanzoun	Less agriculture	0.3
Hlanzoun	Natural death of trees	0.3
Hlanzoun	Restricted access	0.3
Iko	Slash-and-burn	27.6
Iko	Logging	23.8
Iko	Population growth	20.6
Iko	No change	7
Iko	NTFP collection	6.7
Iko	Boundaries respected	3.8
Iko	Nothing to report	3.5
Iko	Effective law enforcement	1.2
Iko	Hunting	1.2
Iko	Less agriculture	1.2
Iko	Clearing for Constructions	0.6
Iko	Forest regrowth	0.6
Iko	Fragmentation	0.6
Iko	Land grabbing	0.6
Iko	Less pressure	0.6
Iko	Restricted access	0.6
Ikot	Nothing to report	27.1
Ikot	Logging	24.7
Ikot	NTFP collection	14.4
Ikot	No change	9.3
Ikot	Slash-and-burn	5.9
Ikot	Clearing for Constructions	2.7
Ikot	Population growth	2.7
Ikot	Less pressure	2.1
Ikot	Forest regrowth	1.9
Ikot	Forest restoration	1.9
Ikot	Fragmentation	1.9
Ikot	Firewood collection	0.8
Ikot	Hunting	0.8
Ikot	Lack of protection measures	0.8
Ikot	Soil quality	0.8
Ikot	Drought	0.5
Ikot	Less agriculture	0.5
Ikot	Behavioral factors	0.3
Ikot	Effective law enforcement	0.3
Ikot	Land grabbing	0.3
Ikot	Local governance failure	0.3
Ikot	Restricted access	0.3
Koui	Effective law enforcement	22
Koui	Restricted access	21.3
Koui	Bush fires	20
Koui	Forest regrowth	15.3
Koui	Nothing to report	11.3
Koui	Logging	3.3
Koui	Slash-and-burn	2
Koui	Charcoal production	0.7
Koui	Forest restoration	0.7
Koui	Fragmentation	0.7
Koui	Less Bush fires	0.7
Koui	Less pressure	0.7
Koui	Natural death of trees	0.7
Koui	No change	0.7
Mbangassina	Logging	51
Mbangassina	Slash-and-burn	24.2
Mbangassina	Nothing to report	8.6
Mbangassina	Population growth	8.3
Mbangassina	Drought	3.6
Mbangassina	NTFP collection	1.3
Mbangassina	Bush fires	0.7
Mbangassina	Clearing for Constructions	0.7
Mbangassina	Forest restoration	0.7
Mbangassina	Fragmentation	0.3
Mbangassina	Hunting	0.3
Mbangassina	Soil quality	0.3
Ngam-Kondomeyos	Logging	72.1
Ngam-Kondomeyos	Slash-and-burn	11
Ngam-Kondomeyos	Nothing to report	9.9
Ngam-Kondomeyos	Drought	1.7
Ngam-Kondomeyos	Clearing for Constructions	1.2
Ngam-Kondomeyos	Population growth	1.2
Ngam-Kondomeyos	Behavioral factors	0.6
Ngam-Kondomeyos	Forest regrowth	0.6
Ngam-Kondomeyos	Fragmentation	0.6
Ngam-Kondomeyos	Land grabbing	0.6
Ngam-Kondomeyos	No change	0.6", 
                               sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# Rename columns for clarity
colnames(forest_data_10yr) <- c("Forest_patch", "Reason", "Percentage")

# Define forest patch order
patch_order <- c("Agou", "Elavagnon-Todji", "Koui", "Ewè-Adakplamè", "Hlanzoun",
                 "Iko", "Ikot", "Mbangassina", "Ngam-Kondomeyos")

# Convert Forest_patch to factor with specified order
forest_data_10yr$Forest_patch <- factor(forest_data_10yr$Forest_patch, levels = patch_order)

# Define categories for reasons
positive_drivers <- c(
  "Effective law enforcement",
  "Forest regrowth",
  "Forest restoration",
  "No change",
  "Less pressure",
  "Less Bush fires",
  "Less agriculture",
  "Restricted access",
  "Boundaries respected",
  "Customary rules",
  "River increases"
  
)

negative_drivers <- c(
  "Logging",
  "Slash-and-burn",
  "Population growth",
  "Bush fires",
  "Fragmentation",
  "Drought",
  "Charcoal production",
  "NTFP collection",
  "Clearing for Constructions",
  "Land grabbing",
  "Pastoralism",
  "Behavioral factors",
  "Firewood collection",
  "Hunting",
  "Soil quality",
  "Lack of protection measures",
  "Local governance failure",
  "Natural death of trees"
)

neutral <- c("Nothing to report")

# Create a new column for categorizing reasons
forest_data_10yr$Category <- ifelse(forest_data_10yr$Reason %in% positive_drivers, "Positive",
                                    ifelse(forest_data_10yr$Reason %in% negative_drivers, "Negative", "Neutral"))

# Create a new column for ordering reasons within categories
forest_data_10yr$Reason_ordered <- factor(
  forest_data_10yr$Reason,
  levels = c(positive_drivers, neutral, negative_drivers)
)

# Create a color palette based on the three categories
forest_data_10yr$Color <- ifelse(forest_data_10yr$Category == "Positive", "#27AE60",
                                 ifelse(forest_data_10yr$Category == "Negative", "#E74C3C", "#F1C40F"))

# Plot with reasons ordered by category (positive on top, neutral in middle, negative at bottom)
ggplot(forest_data_10yr, aes(x = Forest_patch, y = Reason_ordered, size = Percentage, color = Category)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(1, 10), name = "Percentage (%)") +
  scale_color_manual(values = c("Negative" = "#E74C3C", "Neutral" = "#F1C40F", "Positive" = "#27AE60"),
                     name = "Driver Category") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  labs(
    #title = "Drivers of Forest Change by Forest Patch (10-Year Data)",
    #subtitle = "Positive drivers (green) on top, negative drivers (red) on bottom",
    x = "Forest Patch",
    y = "Reason for Change"
  )

# Add facet to clearly separate categories
ggplot(forest_data_10yr, aes(x = Forest_patch, y = reorder(Reason, -as.numeric(Category)), size = Percentage, color = Category)) +
  geom_point(alpha = 0.8) +
  facet_grid(Category ~ ., scales = "free_y", space = "free_y") +
  scale_size_continuous(range = c(1, 9), name = "Percentage (%)") +
  scale_color_manual(values = c("Negative" = "#E74C3C", "Neutral" = "#F1C40F", "Positive" = "#27AE60"),
                     name = "Driver Type") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    strip.background = element_rect(fill = "grey80"),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  labs(
    title = "Drivers of Forest Change by Forest Patch (10-Year Data)",
    subtitle = "Categorized as positive, neutral, and negative drivers",
    x = "Forest Patch",
    y = "Reason for Change"
  )

# If you want to save the plots
# ggsave("forest_change_10yr_categorized_dotplot.png", width = 14, height = 16, dpi = 300)
# ggsave("forest_change_10yr_faceted_dotplot.png", width = 14, height = 16, dpi = 300)