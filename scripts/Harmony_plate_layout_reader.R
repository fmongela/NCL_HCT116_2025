# Parse and plot Revvity's Harmony XML plate assay definitions.
# should work with any user defined parameters, are these are extracted, not hardcoded
# tested w/ full 96well plates, should work for other plate formats & partial layout (?)
# rem: Revvity XML spec sheet not found on www

rm(list = ls())
starttime <- Sys.time()
lib2load <- c("here", "xml2", "tidyverse", "patchwork", "openxlsx")
lapply(lib2load, library, character.only = TRUE)

xml_file <- "2577f4a7-3b8f-4c5c-aece-cda68e3d69c7.xml"
doc <- read_xml(here("data", xml_file)) # keep stuff in github tracked dirs

ns <- xml_ns_rename(xml_ns(doc), d1 = "ns") # Rename namespace

# Extract GroupName attributes from <Registration> section
registration_contents <- xml_find_all(doc, ".//ns:Registration/ns:Content", ns)
group_names <- tibble(
  Content_ID = xml_attr(registration_contents, "ID"),
  GroupName = xml_attr(registration_contents, "GroupName"))

# Extract <Well> elements  
wells <- xml_find_all(doc, ".//ns:Well", ns)

# Extract well data dynamically (<Wells> is parent, <Well> is child in XML)
data_list <- lapply(wells, function(well) {
  well_data <- list(
    Well_ID = xml_attr(well, "WellID"),
    Row = as.integer(xml_attr(well, "Row")),
    Column = as.integer(xml_attr(well, "Column")))
  
  for (i in seq_along(group_names$Content_ID)) {
    content_id <- group_names$Content_ID[i]
    group_name <- group_names$GroupName[i]
    value <- xml_text(xml_find_first(well, paste0("./ns:Content[@ContentID='", content_id, "']/ns:Value"), ns))
    well_data[[group_name]] <- value
  }
  
  return(well_data)
})

# assemble and clean final tibble
plate_layout_tibble <- bind_rows(data_list) %>% 
  select(where(~!all(is.na(.))))

# Try to deal w/ any plate format and try to deal with partial definition
nb_rows <- length(unique(plate_layout_tibble$Row))
first_row_number <- min(plate_layout_tibble$Row, na.rm = TRUE)
last_row_number <- max(plate_layout_tibble$Row, na.rm = TRUE)

plate_layout_tibble <- plate_layout_tibble %>%
  mutate(Row = LETTERS[as.numeric(Row)]) # in XML column ID is numeric 
print(plate_layout_tibble)



# Function to plot grid for a given variable
plot_plate <- function(data, variable, title) 
{ggplot(data, aes(x = Column, y = Row, fill = .data[[variable]])) +
    geom_tile(color = "black") +
    scale_y_discrete(limits = rev(LETTERS[first_row_number:last_row_number])) +  # Ensure A is at the top
    scale_x_continuous(breaks = 1:12) +
    labs(title = title, x = "Column", y = "Row") +
    theme_minimal() +
    theme(legend.position = "right") +
    theme(
      plot.title = element_text(size = 10),
      axis.text.x = element_text(size =5, angle = 0),
      axis.text.y = element_text(size = 5),
      legend.title = element_blank()
    )
}

# Plot each variable and prevent plotting of eventual N/A columns 
var_list <- intersect (group_names$GroupName, names(plate_layout_tibble))

plot_list <- map(var_list, function(content_id) {
  plot_plate(plate_layout_tibble, content_id, sprintf("Variable: %s", content_id))
})

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  current_script <- basename(rstudioapi::getSourceEditorContext()$path)
} else {
  current_script <- "unknown_script.R"
}

combined_plot <- wrap_plots(plot_list) # to work w/ library patchwork
combined_plot +
  plot_layout(ncol = 2,
              widths = unit(c(12 / 2.25), "cm"),  
              heights = unit(c(8 / 2.25), "cm")) +
  plot_annotation(
    title = paste("Layout: ", xml_file),
    subtitle = " ",
    caption = paste0("Script: ", current_script, ", ", today()),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5))
    )
print(Sys.time() - starttime)

# save excel sheets

# Function to generate color styles based on unique values
get_color_styles <- function(values) {
  unique_values <- unique(values)
  colors <- grDevices::rainbow(length(unique_values))  # Generate unique colors
  styles <- setNames(lapply(colors, function(color) createStyle(fgFill = color)), unique_values)
  return(styles)
}


# Create a new workbook
wb <- createWorkbook()

# Apply transformation and add to the workbook
for (content_id in var_list) {
  sub_layout <- plate_layout_tibble %>% 
    select(all_of(c("Row", "Column", content_id))) %>%
    pivot_wider(names_from = Column, values_from = content_id)
  
  addWorksheet(wb, content_id)  # Create a new sheet for each content_id
  writeData(wb, content_id, sub_layout, withFilter = TRUE)
  
  # Generate styles based on unique values
  unique_values <- unlist(sub_layout[, -1])  # Exclude first column (Row)
  styles <- get_color_styles(unique_values)
  
  # Apply styles to the cells
  for (row in 2:(nrow(sub_layout) + 1)) {
    for (col in 2:(ncol(sub_layout) + 1)) {
      cell_value <- as.character(sub_layout[row - 1, col - 1])  # Adjust for Excel indexing
      if (!is.na(cell_value) && cell_value %in% names(styles)) {
        addStyle(wb, content_id, styles[[cell_value]], rows = row, cols = col, gridExpand = FALSE)
      }
    }
  }
}

# Save the workbook
saveWorkbook(wb, here("results", "sub_layouts.xlsx"), overwrite = TRUE)