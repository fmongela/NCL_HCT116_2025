# Parse and graph Revvity XML plate assay definition.
# should work with any user defined parameters
# and any plate types are these are not hard coded
library(tidyverse)
rm(list = ls())
lib2load <- c("here", "xml2", "tidyverse", "gridExtra")
lapply(lib2load, library, character.only = TRUE)

# The XML to be treated
xml_file <- "2577f4a7-3b8f-4c5c-aece-cda68e3d69c7.xml"
doc <- read_xml(here("data", xml_file))


ns <- xml_ns_rename(xml_ns(doc), d1 = "ns") # Rename namespace

# Extract GroupName attributes from Registration section
registration_contents <- xml_find_all(doc, ".//ns:Registration/ns:Content", ns)
group_names <- tibble(
  Content_ID = xml_attr(registration_contents, "ID"),
  GroupName = xml_attr(registration_contents, "GroupName"))

# Extract well data dynamically
wells <- xml_find_all(doc, ".//ns:Well", ns)

data_list <- lapply(wells, function(well) {
  well_data <- list(
    Well_ID = xml_attr(well, "WellID"),
    Row = xml_attr(well, "Row"),
    Column = xml_attr(well, "Column"))
  
  for (i in seq_along(group_names$Content_ID)) {
    content_id <- group_names$Content_ID[i]
    group_name <- group_names$GroupName[i]
    
    value <- xml_text(xml_find_first(well, paste0("./ns:Content[@ContentID='", content_id, "']/ns:Value"), ns))
    well_data[[group_name]] <- value
  }
  
  return(well_data)
})

plate_layout_tibble <- bind_rows(data_list) %>% 
  select(where(~!all(is.na(.)))) %>%
  mutate(Row = LETTERS[as.numeric(Row)]) 
# Print the tibble
print(plate_layout_tibble)

# Function to plot grid for a given variable
plot_plate <- function(data, variable, title) {
  
  ggplot(data, aes(x = Column, y = Row, fill = .data[[variable]])) +
    geom_tile(color = "black") +
    scale_y_discrete(limits = rev(LETTERS[1:8])) +  # Ensure A is at the top
    scale_x_discrete(breaks = 1:12) +
    labs(title = title, x = "Column", y = "Row") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10, angle = 0),
      axis.text.y = element_text(size = 10),
      legend.title = element_blank()
    )
}

# Plot each variable
var_list <- group_names$GroupName
plots <- map(var_list, function(content_id) {
  
  plot_plate(plate_layout_tibble, content_id, sprintf("Plate Layout - %s", content_id))
  
})

# 
# # Remove NULL plots (those that failed), if any
# plots <- Filter(Negate(is.null), plots)
# do.call(grid.arrange, c(plots, ncol = 1))
# 
#   
# plots