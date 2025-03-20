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
    Row = row_num,
    Column = col_num,
    Compound = compound,
    CellType = cell_type,
    Antibody = antibody
  )
})

# Print the tibble
print(plate_data)
