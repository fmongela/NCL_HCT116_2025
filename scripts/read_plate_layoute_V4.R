library(xml2)
library(tidyverse)
library(here)

# Load the XML file
xml_file <- "2577f4a7-3b8f-4c5c-aece-cda68e3d69c7.xml"  # Change this to the correct path
xml_data <- read_xml(here("data", xml_file))

# Load the XML as raw text
xml_file <- here("data", xml_file)  # Change this to the correct path
xml_text <- readLines(xml_file, warn = FALSE)  # Read as text

# Remove namespace declarations (forcefully remove "xmlns=...")
xml_text_clean <- gsub('xmlns="[^"]+"', '', xml_text)

# Re-parse the cleaned XML
xml_data <- read_xml(paste(xml_text_clean, collapse = "\n"))

# Extract all well nodes
wells <- xml_find_all(xml_data, "//Well")

# Convert row numbers to letters
row_map <- setNames(LETTERS[1:8], 1:8)

# Extract all unique ContentIDs
content_ids <- xml_find_all(xml_data, "//Content") %>%
  xml_attr("ContentID") %>%
  unique()

# Parse well information into a tibble
plate_data <- map_df(wells, function(well) {
  well_id <- xml_attr(well, "WellID")
  row_num <- as.integer(xml_attr(well, "Row"))
  col_num <- as.integer(xml_attr(well, "Column"))
  
  # Dynamically extract all ContentID values
  content_values <- map(content_ids, function(content_id) {
    value_node <- xml_find_first(well, sprintf(".//Content[@ContentID='%s']/Value", content_id))
    if (!is.na(value_node)) {
      xml_text(value_node)
    } else {
      NA_character_  # Use NA for missing values
    }
  })
  
  # Create a named list for the content values
  names(content_values) <- content_ids
  
  # Combine with well information
  tibble(
    WellID = well_id,
    Row = row_map[as.character(row_num)],  # Convert row numbers to letters
    Column = col_num,
    !!!content_values  # Use the bang-bang-bang operator to splice the named list into the tibble
  )
})

# Print the transformed tibble
print(plate_data)

# Function to plot heatmap grid for a given variable
plot_plate <- function(data, variable, title) {
  # Ensure the variable exists in the data and is not NA
  if (!variable %in% colnames(data) || all(is.na(data[[variable]]))) {
    stop(sprintf("Variable '%s' is missing or contains only NA values.", variable))
  }
  
  ggplot(data, aes(x = Column, y = Row, fill = .data[[variable]])) +
    geom_tile(color = "black") +
    scale_y_discrete(limits = rev(LETTERS[1:8])) +  # Ensure A is at the top
    scale_x_continuous(breaks = 1:12) +
    labs(title = title, x = "Column", y = "Row") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 0),
      axis.text.y = element_text(size = 10),
      legend.title = element_blank()
    )
}

# Plot each variable
plots <- map(content_ids, function(content_id) {
  tryCatch({
    plot_plate(plate_data, content_id, sprintf("Plate Layout - %s", content_id))
  }, error = function(e) {
    message(sprintf("Skipping plot for '%s': %s", content_id, e$message))
    NULL  # Skip plotting if there's an error
  })
})

# Remove NULL plots (those that failed)
plots <- Filter(Negate(is.null), plots)

# Display plots
if (length(plots) > 0) {
  library(gridExtra)
  do.call(grid.arrange, c(plots, ncol = 1))
} else {
  message("No plots to display.")
}