# Parse and plot Revvity's Harmony XML plate assay definitions.
# should work with any user defined parameters, are these are extracted, not hardcoded
# tested w/ full 96well plates, should work for other plate formats & partial layout (?)
# rem: Revvity XML spec sheet not found on www
# save layout as ggplots in PDF. Also generates a 3-tabs Excel file
rm(list = ls())
starttime <- Sys.time()

library(here)
library(xml2)
library(tidyverse)
library(patchwork)
library(openxlsx)
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
  map_df(~ tibble(Content_ID = xml_attr(.x, "ID"), GroupName = xml_attr(.x, "GroupName")))

# Extract well data dynamically
plate_layout_tibble <- xml_find_all(doc, ".//ns:Well", ns) %>%
  map_df(~ {
    well <- .x
    print(well)
    data <- tibble( # Create a tibble
      Well_ID = xml_attr(well, "WellID"),
      Row = as.integer(xml_attr(well, "Row")),
      Column = as.integer(xml_attr(well, "Column"))
    )
    # Add columns corresponding to each experimental variable    
    for (i in seq_along(group_names$Content_ID)) {
      content_id <- group_names$Content_ID[i]
      group_name <- group_names$GroupName[i]
      value <- xml_text(xml_find_first(well, paste0("./ns:Content[@ContentID='", content_id, "']/ns:Value"), ns))
      data[[group_name]] <- ifelse(is.na(value) | value == "", NA, value)
    }
    data
  }) %>%
  select(where(~ !all(is.na(.)))) %>%
  mutate(Row = LETTERS[pmin(Row, length(LETTERS))])

# Define plot function
plot_plate <- function(data, variable, title) {
  ggplot(data, aes(x = Column, y = Row, fill = .data[[variable]])) +
    geom_tile(color = "black") +
    scale_y_discrete(limits = rev(unique(data$Row))) +
    scale_x_continuous(breaks = 1:max(data$Column, na.rm = TRUE)) +
    labs(title = title, x = "Column", y = "Row") +
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(size = 10),
          axis.text = element_text(size = 5))
}

# Generate plots
var_list <- intersect(group_names$GroupName, names(plate_layout_tibble))
plot_list <- map(var_list, ~ plot_plate(plate_layout_tibble, .x, sprintf("Variable: %s", .x)))

# Combine plots
combined_plot <- wrap_plots(plot_list) +
  plot_layout(ncol = 2) +
  plot_annotation(title = paste("Layout:", xml_file))

combined_plot

# Save PDF
ggsave(here("Results", paste0(filename_root, ".pdf")), plot = combined_plot, device = "pdf", width = 21, height = 29.7, units = "cm", dpi = 300)

# Create Excel workbook
wb <- createWorkbook()

get_color_styles <- function(values) {
  unique_values <- unique(na.omit(values))
  colors <- if (length(unique_values) < 3) {
    brewer.pal(3, "Set3")[seq_along(unique_values)]
  } else if (length(unique_values) <= 12) {
    brewer.pal(length(unique_values), "Set3")
  } else {
    colorRampPalette(brewer.pal(9, "Spectral"))(length(unique_values))
  }
  setNames(lapply(colors, function(color) createStyle(fgFill = color)), as.character(unique_values))
}

walk(var_list, ~ {
  sub_layout <- plate_layout_tibble %>%
    select(Row, Column, .x) %>%
    pivot_wider(names_from = Column, values_from = .x) #
  addWorksheet(wb, .x)
  writeData(wb, .x, sub_layout, withFilter = TRUE)
  
  styles <- get_color_styles(unlist(sub_layout[, -1]))
  for (row in seq_len(nrow(sub_layout))) {
    for (col in seq_len(ncol(sub_layout))[-1]) {
      cell_value <- sub_layout[row, col, drop = TRUE]
      if (!is.na(cell_value) && cell_value %in% names(styles)) {
        addStyle(wb, .x, styles[[cell_value]], rows = row + 1, cols = col, gridExpand = FALSE)
      }
    }
  }
})

saveWorkbook(wb, here("results", paste0(filename_root, ".xlsx")), overwrite = TRUE)
print(Sys.time() - starttime)
