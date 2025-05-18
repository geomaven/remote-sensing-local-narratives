# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Create the dataframe directly from the provided data
forest_data <- read.table(text = "Forest_patch\tReason_for_change_5ys\tpercentage
Agou\tLogging\t50
Agou\tSlash-and-burn\t14.2
Agou\tPopulation growth\t9.1
Agou\tBush fires\t8.5
Agou\tFragmentation\t4.3
Agou\tNothing to report\t4
Agou\tDrought\t2.8
Agou\tEffective law enforcement\t1.7
Agou\tCharcoal production\t1.4
Agou\tNTFP Collection\t1.1
Agou\tClearing for Constructions\t0.9
Agou\tForest regrowth\t0.6
Agou\tForest restoration\t0.6
Agou\tNo change\t0.6
Agou\tLand grabbing\t0.3
Elavagnon-Todji\tBush fires\t44.8
Elavagnon-Todji\tLogging\t30.4
Elavagnon-Todji\tFragmentation\t9.2
Elavagnon-Todji\tSlash-and-burn\t3.9
Elavagnon-Todji\tNothing to report\t2.9
Elavagnon-Todji\tCharcoal production\t2
Elavagnon-Todji\tPopulation growth\t2
Elavagnon-Todji\tEffective law enforcement\t1
Elavagnon-Todji\tNo change\t0.7
Elavagnon-Todji\tPastoralism\t0.7
Elavagnon-Todji\tBehavioral factors\t0.3
Elavagnon-Todji\tClearing for Constructions\t0.3
Elavagnon-Todji\tDrought\t0.3
Elavagnon-Todji\tForest regrowth\t0.3
Elavagnon-Todji\tLack of protection measures\t0.3
Elavagnon-Todji\tLess Bush fires\t0.3
Elavagnon-Todji\tLess pressure\t0.3
Elavagnon-Todji\tRestricted access\t0.3
Ewè-Adakplamè\tNothing to report\t30.6
Ewè-Adakplamè\tBush fires\t27.7
Ewè-Adakplamè\tLogging\t9.8
Ewè-Adakplamè\tPopulation growth\t9.8
Ewè-Adakplamè\tSlash-and-burn\t7.5
Ewè-Adakplamè\tFragmentation\t2.6
Ewè-Adakplamè\tNTFP Collection\t2.6
Ewè-Adakplamè\tClearing for Constructions\t2
Ewè-Adakplamè\tEffective law enforcement\t2
Ewè-Adakplamè\tNo change\t2
Ewè-Adakplamè\tFirewood collection\t0.7
Ewè-Adakplamè\tHunting\t0.7
Ewè-Adakplamè\tLack of protection measures\t0.7
Ewè-Adakplamè\tCommuniy conflicts\t0.3
Ewè-Adakplamè\tCustomary rules\t0.3
Ewè-Adakplamè\tForest restoration\t0.3
Ewè-Adakplamè\tLocal governance failure\t0.3
Ewè-Adakplamè\tRestricted access\t0.3
Hlanzoun\tRiver increases\t30.4
Hlanzoun\tEffective law enforcement\t18.6
Hlanzoun\tLogging\t11.5
Hlanzoun\tNothing to report\t9.6
Hlanzoun\tSlash-and-burn\t9
Hlanzoun\tPopulation growth\t4.8
Hlanzoun\tNo change\t4.2
Hlanzoun\tForest regrowth\t2.2
Hlanzoun\tCharcoal production\t1.6
Hlanzoun\tNTFP Collection\t1.6
Hlanzoun\tBush fires\t1.3
Hlanzoun\tLess agriculture\t1.3
Hlanzoun\tDrought\t1
Hlanzoun\tLess pressure\t1
Hlanzoun\tCustomary rules\t0.6
Hlanzoun\tRestricted access\t0.6
Hlanzoun\tForest restoration\t0.3
Hlanzoun\tNatural death of trees\t0.3
Iko\tSlash-and-burn\t29.7
Iko\tLogging\t28.8
Iko\tPopulation growth\t19.8
Iko\tNTFP Collection\t5.5
Iko\tBoundaries respected\t4.7
Iko\tNo change\t4.7
Iko\tNothing to report\t2
Iko\tEffective law enforcement\t1.7
Iko\tLand grabbing\t1.2
Iko\tForest regrowth\t0.6
Iko\tFragmentation\t0.6
Iko\tHunting\t0.6
Iko\tRestricted access\t0.3
Ikot\tLogging\t23.4
Ikot\tNTFP Collection\t21
Ikot\tNothing to report\t15.4
Ikot\tNo change\t12.5
Ikot\tSlash-and-burn\t7.7
Ikot\tClearing for Constructions\t2.7
Ikot\tForest restoration\t2.7
Ikot\tPopulation growth\t2.7
Ikot\tFragmentation\t1.9
Ikot\tForest regrowth\t1.6
Ikot\tLack of protection measures\t1.6
Ikot\tLess pressure\t1.3
Ikot\tEffective law enforcement\t1.1
Ikot\tFirewood collection\t1.1
Ikot\tSoil quality\t1.1
Ikot\tDrought\t0.8
Ikot\tLess agriculture\t0.8
Ikot\tRestricted access\t0.5
Ikot\tLocal governance failure\t0.3
Koui\tEffective law enforcement\t24.7
Koui\tRestricted access\t22.7
Koui\tBush fires\t20.7
Koui\tForest regrowth\t11.3
Koui\tNothing to report\t10
Koui\tLogging\t2.7
Koui\tFragmentation\t1.3
Koui\tLess pressure\t1.3
Koui\tNo change\t1.3
Koui\tSlash-and-burn\t1.3
Koui\tCharcoal production\t0.7
Koui\tForest restoration\t0.7
Koui\tLess Bush fires\t0.7
Koui\tNatural death of trees\t0.7
Mbangassina\tSlash-and-burn\t41.1
Mbangassina\tLogging\t37.4
Mbangassina\tPopulation growth\t8.9
Mbangassina\tDrought\t3
Mbangassina\tNTFP Collection\t3
Mbangassina\tBush fires\t1.7
Mbangassina\tNothing to report\t1.7
Mbangassina\tClearing for Constructions\t1.3
Mbangassina\tEffective law enforcement\t0.7
Mbangassina\tForest restoration\t0.7
Mbangassina\tNo change\t0.3
Mbangassina\tSoil quality\t0.3
Ngam-Kondomeyos\tLogging\t68
Ngam-Kondomeyos\tSlash-and-burn\t16.9
Ngam-Kondomeyos\tClearing for Constructions\t3.5
Ngam-Kondomeyos\tDrought\t2.9
Ngam-Kondomeyos\tPopulation growth\t2.3
Ngam-Kondomeyos\tNothing to report\t1.7
Ngam-Kondomeyos\tBehavioral factors\t1.2
Ngam-Kondomeyos\tSoil quality\t1.2
Ngam-Kondomeyos\tBoundaries respected\t0.6
Ngam-Kondomeyos\tForest restoration\t0.6
Ngam-Kondomeyos\tHunting\t0.6
Ngam-Kondomeyos\tNTFP Collection\t0.6", 
                          sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# Rename columns for clarity
colnames(forest_data) <- c("Forest_patch", "Reason", "Percentage")

# Define forest patch order
patch_order <- c("Agou", "Elavagnon-Todji", "Koui", "Ewè-Adakplamè", "Hlanzoun",
                 "Iko", "Ikot", "Mbangassina", "Ngam-Kondomeyos")

# Convert Forest_patch to factor with specified order
forest_data$Forest_patch <- factor(forest_data$Forest_patch, levels = patch_order)

# Define categories for reasons
positive_drivers <- c(
  "Effective law enforcement",
  "Forest regrowth",
  "Forest restoration",
  "No change",
  "Restricted access",
  "Boundaries respected",
  "Customary rules",
  "Less pressure",
  "Less Bush fires",
  "Less agriculture",
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
  "NTFP Collection",
  "Clearing for Constructions",
  "Land grabbing",
  "Pastoralism",
  "Behavioral factors",
  "Firewood collection",
  "Hunting",
  "Soil quality",
  "Lack of protection measures",
  "Communiy conflicts",
  "Local governance failure",
  "Natural death of trees"
)

neutral <- c("Nothing to report")

# Create a new column for categorizing reasons
forest_data$Category <- ifelse(forest_data$Reason %in% positive_drivers, "Positive",
                               ifelse(forest_data$Reason %in% negative_drivers, "Negative", "Neutral"))

# Create a new column for ordering reasons within categories
forest_data$Reason_ordered <- factor(
  forest_data$Reason,
  levels = c(positive_drivers, neutral, negative_drivers)
)

# Create a color palette based on the three categories
forest_data$Color <- ifelse(forest_data$Category == "Positive", "#27AE60",
                            ifelse(forest_data$Category == "Negative", "#E74C3C", "#F1C40F"))

# Plot with reasons ordered by category (positive on top, neutral in middle, negative at bottom)
ggplot(forest_data, aes(x = Forest_patch, y = Reason_ordered, size = Percentage, color = Category)) +
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
    #title = "Drivers of Forest Change by Forest Patch",
    #subtitle = "Positive drivers (green) on top, negative drivers (red) on bottom",
    x = "Forest Patch",
    y = "Reason for Change"
  )

# Add facet to clearly separate categories
ggplot(forest_data, aes(x = Forest_patch, y = reorder(Reason, -as.numeric(Category)), size = Percentage, color = Category)) +
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
    title = "Drivers of Forest Change by Forest Patch",
    subtitle = "Categorized as positive, neutral, and negative drivers",
    x = "Forest Patch",
    y = "Reason for Change"
  )

# If you want to save the plots
# ggsave("forest_change_categorized_dotplot.png", width = 14, height = 16, dpi = 300)
# ggsave("forest_change_faceted_dotplot.png", width = 14, height = 16, dpi = 300)