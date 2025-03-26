# Parse and graph Revvity XML plate assay definitions.
# should work with any user defined parameters are these are not hard coded
# Work for 96well plates, needs work to adapt to other formats
# not good yet for partial scan.
# rem: Revvity XML specs not found on www

rm(list = ls())
lib2load <- c("here", "xml2", "tidyverse", "patchwork")
lapply(lib2load, library, character.only = TRUE)

xml_file <- "2577f4a7-3b8f-4c5c-aece-cda68e3d69c7.xml"
doc <- read_xml(here("data", xml_file))

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

# assembleand clean final tibble
plate_layout_tibble <- bind_rows(data_list) %>% 
  select(where(~!all(is.na(.)))) %>%
  mutate(Row = LETTERS[as.numeric(Row)]) # in XML column ID is numeric 
print(plate_layout_tibble)

nb_rows <- length(unique(plate_layout_tibble$Row))

# Function to plot grid for a given variable
plot_plate <- function(data, variable, title) 
{ggplot(data, aes(x = Column, y = Row, fill = .data[[variable]])) +
    geom_tile(color = "black") +
    scale_y_discrete(limits = rev(LETTERS[1:nb_rows])) +  # Ensure A is at the top
    scale_x_continuous(breaks = 1:12) +
    labs(title = title, x = "Column", y = "Row") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 8, angle = 0),
      axis.text.y = element_text(size = 8),
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

combined_plot <- wrap_plots(plot_list)
combined_plot +
  plot_layout(ncol = 1,
              widths = unit(c(12 / 2.25), "cm"),  
              heights = unit(c(8 / 2.25), "cm")) +
  plot_annotation(
    title = paste("Layout: ", xml_file),
    subtitle = " ",
    caption = paste0("Script: ", current_script, ", ", today()),
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(color = "gray40")
    )
  )
