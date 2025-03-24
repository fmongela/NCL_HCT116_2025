library(xml2)
library(tidyverse)

# Load the XML file
xml_file <- "2577f4a7-3b8f-4c5c-aece-cda68e3d69c7.xml"  # Change this to the correct path
xml_data <- read_xml(here("data",xml_file))

# Load the XML as raw text
xml_file <- here("data",xml_file)  # Change this to the correct path
xml_text <- readLines(xml_file, warn = FALSE)  # Read as text

# Remove namespace declarations (forcefully remove "xmlns=...")
xml_text_clean <- gsub('xmlns="[^"]+"', '', xml_text)

# Re-parse the cleaned XML
xml_data <- read_xml(paste(xml_text_clean, collapse = "\n"))

# Extract all well nodes
wells <- xml_find_all(xml_data, "//Well")

# Convert row numbers to letters
row_map <- setNames(LETTERS[1:8], 1:8)

# Parse well information into a tibble
plate_data <- map_df(wells, function(well) {
  well_id <- xml_attr(well, "WellID")
  row_num <- as.integer(xml_attr(well, "Row"))
  col_num <- as.integer(xml_attr(well, "Column"))
  
  compound <- xml_text(xml_find_first(well, ".//Content[@ContentID='Compound']/Value"))
  cell_type <- xml_text(xml_find_first(well, ".//Content[@ContentID='CellType_1']/Value"))
  antibody <- xml_text(xml_find_first(well, ".//Content[@ContentID='Antibody_2']/Value"))
  
  tibble(
    WellID = well_id,
    Row = row_map[as.character(row_num)],  # Convert row numbers to letters
    Column = col_num,
    Compound = compound,
    CellType = cell_type,
    Antibody = antibody
  )
})

# Print the transformed tibble
print(plate_data)

# Function to plot heatmap grid for a given variable
plot_plate <- function(data, variable, title) {
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
p1 <- plot_plate(plate_data, "Compound", "Plate Layout - Compound")
p2 <- plot_plate(plate_data, "CellType", "Plate Layout - Cell Type")
p3 <- plot_plate(plate_data, "Antibody", "Plate Layout - Antibody")

# Display plots
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 1)