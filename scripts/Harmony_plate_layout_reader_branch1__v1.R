# Parse and plot Revvity's Harmony XML plate assay definitions.
# should work with any user defined parameters, are these are extracted, not hardcoded
# tested w/ full 96well plates, should work for other plate formats & partial layout (?)
# rem: Revvity XML spec sheet not found on www
# save layout as ggplots in PDF. Also generates a 3-tabs Excel file
rm(list = ls())
starttime <- Sys.time()

library(openxlsx2)
library(here)
library(xml2)
library(tidyverse)
library(patchwork)
library(tools)
library(RColorBrewer)

# Define file paths
xml_file <- "2577f4a7-3b8f-4c5c-aece-cda68e3d69c7.xml"
filename_root <- file_path_sans_ext(xml_file)
doc <- read_xml(here("data", xml_file))

# Set up namespace
ns <- xml_ns_rename(xml_ns(doc), d1 = "ns")

# Extract GroupName attributes from <Registration>
group_names <- xml_find_all(doc, ".//ns:Registration/ns:Content", ns) %>%
  map_df( ~ tibble(
    Content_ID = xml_attr(.x, "ID"),
    GroupName = xml_attr(.x, "GroupName")
  ))

# Extract well data dynamically
plate_layout_tibble <- xml_find_all(doc, ".//ns:Well", ns) %>%
  map_df( ~ {
    well <- .x
        data <- tibble(
      # Create a tibble
      Well_ID = xml_attr(well, "WellID"),
      Row = as.integer(xml_attr(well, "Row")),
      Column = as.integer(xml_attr(well, "Column"))
    )
    # Add columns corresponding to each experimental variable
    for (i in seq_along(group_names$Content_ID)) {
      content_id <- group_names$Content_ID[i]
      group_name <- group_names$GroupName[i]
      value <- xml_text(xml_find_first(
        well,
        paste0("./ns:Content[@ContentID='", content_id, "']/ns:Value"),
        ns
      ))
      data[[group_name]] <- ifelse(is.na(value) |
                                     value == "", NA, value)
    }
    data
  }) %>%
  select(where( ~ !all(is.na(.)))) %>%
  mutate(Row = LETTERS[pmin(Row, length(LETTERS))])

# Define plot function
plot_plate <- function(data, variable, title) {
  ggplot(data, aes(x = Column, y = Row, fill = .data[[variable]])) +
    geom_tile(color = "black") +
    scale_y_discrete(limits = rev(unique(data$Row))) +
    scale_x_continuous(breaks = 1:max(data$Column, na.rm = TRUE)) +
    labs(title = title, x = "Column", y = "Row") +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 10),
      axis.text = element_text(size = 5)
    )
}

# Generate plots
var_list <- intersect(group_names$GroupName, names(plate_layout_tibble))
plot_list <- map(var_list, ~ plot_plate(plate_layout_tibble, .x, sprintf("Variable: %s", .x)))

# Combine plots
combined_plot <- wrap_plots(plot_list) +
  plot_layout(ncol = 2,
              widths = unit(c(12 / 2), "cm"),  
              heights = unit(c(8 / 2), "cm")) +
  plot_annotation(title = paste("Layout:", xml_file))

combined_plot

# Save PDF
ggsave(
  here("Results", paste0(filename_root, ".pdf")),
  plot = combined_plot,
  device = "pdf",
  width = 21,
  height = 29.7,
  units = "cm",
  dpi = 300
)

# Create Excel workbook
# Create a workbook
wb <- wb_workbook()

walk(var_list, ~ {
  sub_layout <- plate_layout_tibble %>% # make a tibble per experimental variable
    select(Row, Column, .x) %>%
    pivot_wider(names_from = Column, values_from = .x) #
  print(sub_layout)
  var_en_cours <- .x
  
  # Add sample data
      wb <- wb_add_worksheet(wb, var_en_cours)
      wb <- wb_add_data(wb, sheet = var_en_cours, x = sub_layout, start_row = 1, start_col = 1)
  
      unique_var_values <- sub_layout %>%
    slice(-1) %>%  # Remove first row
    select(-1) %>%  # Remove first column
    unlist() %>%
    table() %>%
    names()
  
num_colors <- length(unique_var_values)
palette_colors <- head(brewer.pal(min(num_colors, 12), "Set3"),num_colors)   
color_map <- tibble (unique_var_values, palette_colors)
       
# # Apply conditional formatting for each unique value
# 
#  for (value in unique_var_values) {
#    
#    # Create a style with pink background
#    wb <- wb_add_dxfs_style(wb, name = "pink_fill", bg_fill = wb_color(hex = "FFC0CB"))
#    
#    # Apply conditional formatting with the specified rule and style
#    wb <- wb_add_conditional_formatting(wb, sheet = var_en_cours, cols = 1, rows = 2:5,
#         type = "containsText", rule = sprintf(value), style = "pink_fill")
#  }  
})


wb_save(wb, here("results", paste0(filename_root, ".xlsx")), overwrite = TRUE)

print(Sys.time() - starttime)
