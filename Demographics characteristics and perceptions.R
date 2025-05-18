# ------------------------------- #
#          Load Libraries         #
# ------------------------------- #

# Install missing packages if necessary
required_packages <- c("tidyverse", "ggplot2", "readxl", "dplyr", "tidyr", 
                       "stats", "reshape2", "viridis", "gridExtra", "scales", "dunn.test")

installed_packages <- rownames(installed.packages())
for(pkg in required_packages){
  if(!(pkg %in% installed_packages)){
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(stats)
library(reshape2)
library(viridis)
library(gridExtra)
library(scales)
library(dunn.test)

# ------------------------------- #
#         Read and Clean Data     #
# ------------------------------- #

# Define the path to the Excel data file
data_path <- "C:/Users/chima/OneDrive - Universitaet Bern/Documents/SUSTAINFORESTS/Papers/SUSTAINFORESTS Papers/Local perspectives and Spatiotemporal dynamics/Data/Livelihood database_cleaned.xlsx"

# Read the Excel file
raw_data <- read_excel(data_path)

# Clean and preprocess the data
clean_data <- raw_data %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%  # Convert empty strings to NA
  mutate(
    Age = as.factor(Age),
    `Living period in the community` = as.factor(`Living period in the community`),
    `People living in your household` = as.numeric(`People living in your household`)
  ) %>%
  filter(!is.na(`Forest patch`))  # Remove rows without a forest patch

# ------------------------------- #
#   Reason for Change (10 Years)  #
# ------------------------------- #

reason_change_10ys <- clean_data %>%
  filter(!is.na(`Reason for change_10ys`)) %>%
  count(`Forest patch`, `Reason for change_10ys`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(`Forest patch`, `Reason for change_10ys`)

cat("\nPercentage of entries for 'Reason for change_10ys' per Forest Patch:\n")
print(reason_change_10ys, n = Inf)

# ------------------------------- #
#       Demographic Plots         #
# ------------------------------- #

plot_demographics <- function(data_input) {
  
  # Plot for Age (Percentage Plot)
  age_plot <- data_input %>%
    filter(!is.na(Age)) %>%
    count(`Forest patch`, Age) %>%
    group_by(`Forest patch`) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    ggplot(aes(x = `Forest patch`, y = percentage, fill = Age)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    labs(
      title = "Age Distribution by Forest Patch",
      x = "Forest Patch",
      y = "Percentage"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(age_plot)
  
  # Print the percentage of age distribution
  cat("\nAge distribution by forest patch:\n")
  age_distribution <- data_input %>%
    filter(!is.na(Age)) %>%
    count(`Forest patch`, Age) %>%
    group_by(`Forest patch`) %>%
    mutate(percentage = n / sum(n) * 100)
  print(age_distribution, n = 100)
  
  # Plot for Living Period in the Community (Percentage Plot)
  living_period_plot <- data_input %>%
    filter(!is.na(`Living period in the community`)) %>%
    count(`Forest patch`, `Living period in the community`) %>%
    group_by(`Forest patch`) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    ggplot(aes(x = `Forest patch`, y = percentage, fill = `Living period in the community`)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    labs(
      title = "Living Period Distribution by Forest Patch",
      x = "Forest Patch",
      y = "Percentage"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(living_period_plot)
  
  # Plot for Gender (Percentage Plot)
  gender_plot <- data_input %>%
    filter(!is.na(Gender)) %>%
    count(`Forest patch`, Gender) %>%
    group_by(`Forest patch`) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    ggplot(aes(x = `Forest patch`, y = percentage, fill = Gender)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
    labs(
      title = "Gender Distribution by Forest Patch",
      x = "Forest Patch",
      y = "Percentage"
    ) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(gender_plot)
}

# Calculate and print percentages for Gender
cat("\nPercentage of Men by Forest Patch:\n")
Man_percentage <- clean_data %>%
  filter(!is.na(`Gender`)) %>%
  count(`Forest patch`, `Gender`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(`Gender` == "Man") %>%
  select(`Forest patch`, percentage)
print(Man_percentage)

cat("\nPercentage of Women by Forest Patch:\n")
Woman_percentage <- clean_data %>%
  filter(!is.na(`Gender`)) %>%
  count(`Forest patch`, `Gender`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(`Gender` == "Woman") %>%
  select(`Forest patch`, percentage)
print(Woman_percentage)

# Call demographic plotting function
plot_demographics(clean_data)

# ------------------------------- #
#     Main Profession Plotting    #
# ------------------------------- #

profession_plot <- clean_data %>%
  filter(!is.na(`Main profession`)) %>%
  count(`Forest patch`, `Main profession`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = `Forest patch`, y = percentage, fill = `Main profession`)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  labs(
    title = "Main Profession Distribution by Forest Patch",
    x = "Forest Patch",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("\nPlot for Main Profession Distribution by Forest Patch:\n")
print(profession_plot)

# ------------------------------- #
#       Education Level Plot      #
# ------------------------------- #

education_plot <- clean_data %>%
  filter(!is.na(`Education level`)) %>%
  mutate(`Education level` = factor(
    `Education level`,
    levels = c("University level", "High school", "Primary school", 
               "Uneducated", "No report")
  )) %>%
  count(`Forest patch`, `Education level`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = `Forest patch`, y = percentage, fill = `Education level`)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  labs(
    title = "Education Level Distribution by Forest Patch",
    x = "Forest Patch",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("\nPlot for Education Level Distribution by Forest Patch:\n")
print(education_plot)

# ------------------------------- #
#    Residency Duration Plot      #
# ------------------------------- #

residency_plot <- clean_data %>%
  filter(!is.na(`Living period in the community`)) %>%
  mutate(`Living period in the community` = factor(
    `Living period in the community`,
    levels = c("Above 20", "Between 15 and 20", "Between 10 and 15", 
               "Between 5 and 10", "Less than 5", "No Report")
  )) %>%
  count(`Forest patch`, `Living period in the community`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = `Forest patch`, y = percentage, fill = `Living period in the community`)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  labs(
    title = "Residency Duration Distribution by Forest Patch",
    x = "Forest Patch",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("\nPlot for Residency Duration Distribution by Forest Patch:\n")
print(residency_plot)

# ------------------------------- #
#  Profession & Forest Activity   #
# ------------------------------- #

cat("\nPercentage of 'Farming' as Main Profession by Forest Patch:\n")
farming_percentage <- clean_data %>%
  filter(!is.na(`Main profession`)) %>%
  count(`Forest patch`, `Main profession`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(`Main profession` == "Farming") %>%
  select(`Forest patch`, percentage)
print(farming_percentage)

cat("\nPercentage of 'Collection and sale of NTFPs' as Main Forest Activity by Forest Patch:\n")
NTFPs_percentage <- clean_data %>%
  filter(!is.na(`What is the MAIN activity you do in the forest that brings you income?`)) %>%
  count(`Forest patch`, `What is the MAIN activity you do in the forest that brings you income?`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(`What is the MAIN activity you do in the forest that brings you income?` == "Collection and sale of NTFPs") %>%
  select(`Forest patch`, percentage)
print(NTFPs_percentage)

cat("\nPercentage of 'Unemployed or No Report' as Main Profession by Forest Patch:\n")
unemployed_percentage <- clean_data %>%
  filter(!is.na(`Main profession`)) %>%
  count(`Forest patch`, `Main profession`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(`Main profession` == "Unemployed or No Report") %>%
  select(`Forest patch`, percentage)
print(unemployed_percentage)

cat("\nPercentage of 'Charcoal production' as Main Forest Activity by Forest Patch:\n")
Charcoal_percentage <- clean_data %>%
  filter(!is.na(`What is the MAIN activity you do in the forest that brings you income?`)) %>%
  count(`Forest patch`, `What is the MAIN activity you do in the forest that brings you income?`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(`What is the MAIN activity you do in the forest that brings you income?` == "Charcoal production") %>%
  select(`Forest patch`, percentage)
print(Charcoal_percentage)

# ------------------------------- #
#   Average Household per Patch   #
# ------------------------------- #

average_household <- clean_data %>%
  filter(!is.na(`People living in your household`)) %>%
  group_by(`Forest patch`) %>%
  summarize(average_household_size = mean(`People living in your household`, na.rm = TRUE))

cat("\nAverage Number of People per Household by Forest Patch:\n")
print(average_household)

# ------------------------------- #
#       Living Period Summary     #
# ------------------------------- #

living_period_summary <- clean_data %>%
  filter(!is.na(`Living period in the community`)) %>%
  count(`Forest patch`, `Living period in the community`) %>%
  group_by(`Forest patch`) %>%
  mutate(proportion = n / sum(n)) %>%
  select(`Forest patch`, `Living period in the community`, n, proportion) %>%
  arrange(`Forest patch`, `Living period in the community`)

cat("\nSummary of Living Period Distribution by Forest Patch:\n")
print(living_period_summary, n = Inf)

# ------------------------------- #
#   Perceptions of Forest Change  #
# ------------------------------- #

# Perceptions of Forest Cover Change (Last 5 Years)
forest_change_5yrs_plot <- clean_data %>%
  filter(!is.na(`Forest change_5yrs`)) %>%
  count(`Forest patch`, `Forest change_5yrs`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = `Forest patch`, y = percentage, fill = `Forest change_5yrs`)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  labs(
    title = "Perceptions of Forest Cover Change (Last 5 Years)",
    x = "Forest Patch",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("\nPlot: Perceptions of Forest Cover Change (Last 5 Years):\n")
print(forest_change_5yrs_plot)

# Perceptions of Forest Cover Change (Last 10 Years)
forest_change_10yrs_plot <- clean_data %>%
  filter(!is.na(`Forest change_10 ys`)) %>%
  count(`Forest patch`, `Forest change_10 ys`) %>%
  group_by(`Forest patch`) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = `Forest patch`, y = percentage, fill = `Forest change_10 ys`)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  labs(
    title = "Perceptions of Forest Cover Change (Last 10 Years)",
    x = "Forest Patch",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("\nPlot: Perceptions of Forest Cover Change (Last 10 Years):\n")
print(forest_change_10yrs_plot)

# ------------------------------- #
#  Statistical Tests by Patch     #
# ------------------------------- #

# This function performs either Fisher's Exact Test or Chi-square Test on a
# per-forest-patch basis, depending on the structure and counts in the
# contingency tables.

perform_statistical_test_by_patch <- function(data, var) {
  patch_results <- list()
  
  # Iterate through unique forest patches
  for (patch in unique(data$`Forest patch`)) {
    patch_subset <- data[data$`Forest patch` == patch, ]
    
    # Use tryCatch to handle potential errors (e.g., insufficient data)
    tryCatch({
      # Build the contingency table for "Forest change_10 ys" vs. the variable of interest
      contingency_table <- table(patch_subset$`Forest change_10 ys`, patch_subset[[var]])
      
      # Decide which test to use based on table dimensions and counts
      if (any(contingency_table < 5) || ncol(contingency_table) > 2 || nrow(contingency_table) > 2) {
        test_result <- fisher.test(contingency_table, simulate.p.value = TRUE)
        test_name <- "Fisher's Exact Test"
      } else {
        test_result <- chisq.test(contingency_table)
        test_name <- "Chi-square Test"
      }
      
      # Store results for each patch
      patch_results[[patch]] <- list(
        test_name = test_name,
        p_value = test_result$p.value,
        contingency_table = contingency_table
      )
    }, error = function(e) {
      # If an error occurs, store the error message instead
      patch_results[[patch]] <- list(error = conditionMessage(e))
    })
  }
  
  return(patch_results)
}

# Select variables to test
variables <- c("Gender", "Marital status", "Age", "Education level", "Main Profession")

# Conduct tests for each variable
test_results_by_patch <- lapply(variables, function(var) {
  perform_statistical_test_by_patch(clean_data, var)
})

# Print statistical test results
for (i in seq_along(variables)) {
  var_name <- variables[i]
  cat("\n", var_name, "vs. Forest change_10 ys by Forest Patch:\n")
  
  # Iterate through forest patches and print results
  for (patch_name in names(test_results_by_patch[[i]])) {
    cat("\nForest Patch:", patch_name, "\n")
    patch_result <- test_results_by_patch[[i]][[patch_name]]
    
    if ("error" %in% names(patch_result)) {
      cat("Error:", patch_result$error, "\n")
    } else {
      cat("Test:", patch_result$test_name, "\n")
      cat("P-value:", patch_result$p_value, "\n")
      cat("Contingency Table:\n")
      print(patch_result$contingency_table)
    }
  }
}

# ------------------------------- #
#   Contingency Table Visualization
# ------------------------------- #

# Visualization functions for contingency tables by forest patch with standardized scale

# Function to calculate the maximum percentage value across all patches
calculate_max_percentage <- function(data, col_name, group_name) {
  max_percentage <- 0
  unique_patches <- unique(data$`Forest patch`)
  
  for (patch in unique_patches) {
    patch_subset <- data[data$`Forest patch` == patch, ]
    
    if (nrow(patch_subset) > 0) {
      contingency_table <- table(patch_subset[[col_name]], patch_subset[[group_name]])
      melted_table <- melt(contingency_table)
      melted_table$percentage <- (melted_table$value / sum(melted_table$value)) * 100
      max_percentage <- max(max_percentage, max(melted_table$percentage, na.rm = TRUE))
    }
  }
  return(max_percentage)
}

# Function to create a plot for a given patch
create_plot <- function(patch_subset, col_name, group_name, max_percentage, patch) {
  contingency_table <- table(patch_subset[[col_name]], patch_subset[[group_name]])
  melted_table <- melt(contingency_table)
  melted_table$percentage <- (melted_table$value / sum(melted_table$value)) * 100
  total_population <- sum(melted_table$value)
  
  ggplot(melted_table, aes(x = Var1, y = Var2, fill = percentage)) +
    geom_tile(color = "white") +
    scale_fill_gradient(
      low = "lightblue", 
      high = "darkblue", 
      limits = c(0, max_percentage),
      labels = scales::percent_format(scale = 1)
    ) +
    labs(
      title = paste("Contingency Table:", col_name, "vs", group_name, "in", patch),
      x = group_name,
      y = col_name,
      fill = paste("Percentage\n(n =", total_population, ")")
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

# Function to plot contingency tables by forest patch with standardized scale
plot_contingency_table_by_patch <- function(data, col_name, group_name) {
  # Input validation
  if (!all(c(col_name, group_name) %in% names(data))) {
    stop("Required columns are missing from the dataset")
  }
  
  plot_list <- list()
  
  # Calculate the maximum percentage value across all patches
  max_percentage <- calculate_max_percentage(data, col_name, group_name)
  
  # Create plots with standardized scale
  unique_patches <- unique(data$`Forest patch`)
  for (patch in unique_patches) {
    patch_subset <- data[data$`Forest patch` == patch, ]
    
    if (nrow(patch_subset) > 0) {
      plot <- create_plot(patch_subset, col_name, group_name, max_percentage, patch)
      plot_list[[patch]] <- plot
    }
  }
  
  return(plot_list)
}

# Generate and print the contingency table heatmaps for each variable
for (var in variables) {
  cat("\nHeatmaps for 'Forest change_10 ys' vs.", var, "by Forest Patch:\n")
  plots <- plot_contingency_table_by_patch(clean_data, "Forest change_10 ys", var)
  
  # Print the resulting plots
  for (patch_name in names(plots)) {
    print(plots[[patch_name]])
  }
}

# ------------------------------- #
#      Kruskal-Wallis by Patch    #
# ------------------------------- #

kruskal_tests <- lapply(unique(clean_data$`Forest patch`), function(patch) {
  patch_subset <- clean_data[clean_data$`Forest patch` == patch, ]
  
  list(
    Community = tryCatch({
      kruskal.test(`Forest change_10 ys` ~ Community, data = patch_subset)
    }, error = function(e) e),
    
    `Education level` = tryCatch({
      kruskal.test(`Forest change_10 ys` ~ `Education level`, data = patch_subset)
    }, error = function(e) e)
  )
})

# Print Kruskal-Wallis Test Results
for (i in seq_along(unique(clean_data$`Forest patch`))) {
  patch_name <- unique(clean_data$`Forest patch`)[i]
  cat("\nForest Patch:", patch_name, "\n")
  
  for (var_name in names(kruskal_tests[[i]])) {
    cat("\n", var_name, ":\n")
    if (inherits(kruskal_tests[[i]][[var_name]], "error")) {
      cat("Error:", kruskal_tests[[i]][[var_name]]$message, "\n")
    } else {
      print(kruskal_tests[[i]][[var_name]])
    }
  }
}

################################################################################
# End of Script
################################################################################
