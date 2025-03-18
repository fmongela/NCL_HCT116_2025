# Load required packages
library(data.table)   # Faster data import
library(tidyverse)    # Data manipulation and visualization
library(gridExtra)    # Multi-plot arrangement
library(scales)       # Percentage formatting

# Define file path
file_path <- "D:/smyl/Labo/projects/KD NCL HCT116/Analyses HCS/#4281/Evaluation8/Objects_Population - Nuclei Valid Selected.txt"

# Source external function
source("D:/smyl/Labo/projects/KD NCL HCT116/Analyses HCS/R/get_file_info.R")
file_info <- get_file_info(file_path)

# Load column names to keep
cols_to_load <- fread("D:/smyl/Labo/projects/KD NCL HCT116/Analyses HCS/R/toto2.tsv") %>%
  filter(is_loaded == "y") %>%
  select(old_name, new_name) %>%
  deframe()  # Convert tibble to named vector

# Read & preprocess data (use fread for speed)
data <- fread(file_path, skip = 9, dec = ",") %>%
  select(names(cols_to_load)) %>%
  rename_with(~ cols_to_load[.x]) %>%
  mutate(
    `Nuclei class` = case_when(
      `Nuclei class` == "A" ~ 1L,
      `Nuclei class` == "B" ~ 2L,
      `Nuclei class` == "C" ~ 3L,
      TRUE ~ 4L
    ),
    `Cell type` = factor(`Cell type`, levels = c("NS-1", "22-2", "22-6")),
    `Compound` = factor(`Compound`, levels = c("noDox", "Dox"))
  ) %>%
  pivot_longer(cols = -c(Compound, `Cell type`, Antibody), names_to = "Measurements", values_to = "Value")

# Define parameters to plot
measurements_to_plot <- c(
  "NCL mean Intensity",
  "NCL median intensity",
  "FBL mean intensity",
  "FBL median intensity",
  "Nucleus Area [µm²]"
)

# Optimize the plotting function
Harry <- function(param_to_be_plotted) {
  ggplot(filter(data, Measurements == param_to_be_plotted), aes(x = Compound, y = Value, fill = Compound)) +
    geom_violin(trim = FALSE) + 
    geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +  
    facet_wrap(~ `Cell type`) +
    labs(title = param_to_be_plotted, x = "Treatment", y = param_to_be_plotted) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Call Harry, the ploting function
plots_list <- map(measurements_to_plot, Harry)

# Process Nuclei class separately and create stacked histogram 
data_for_Nuclei_class_plot <- data %>%
  filter(Measurements == "Nuclei class") %>%
  mutate(Value = factor(
    case_when(
      Value == 1 ~ "Normal",
      Value == 2 ~ "Round",
      Value == 3 ~ "Dispersed",
      TRUE ~ "UnClassified"
    ),
    levels = c("UnClassified", "Dispersed", "Round", "Normal")
  ))

plot_nuclei_class <- ggplot(data_for_Nuclei_class_plot, aes(Compound, fill = Value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ `Cell type`) +
  theme_minimal() +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Percentage") +
  labs(title = "Nuclei class") +
  theme(plot.title = element_text(hjust = 0.5))

# Add Nuclei class plot for final plot list
plots_list[[length(plots_list) + 1]] <- plot_nuclei_class

# Display plots
do.call(grid.arrange, plots_list)
