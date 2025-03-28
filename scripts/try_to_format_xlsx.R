library(openxlsx2)

# Create a workbook
wb <- wb_workbook()

# Add a worksheet
wb <- add_sheet(wb, "Sheet1")

# Add sample data
data <- data.frame(col1 = c("toto", "hello", "world", "toto"))
write_data(wb, sheet = "Sheet1", x = data)

# Create a style with pink background (using direct fill)
pink_fill <- wb_fill(color = "FFC0CB")  # Light pink fill

# Apply conditional formatting
wb <- add_conditional_formatting(
  wb, 
  sheet = "Sheet1",
  dims = "A2:A5",  # Data starts at A2 (header in A1)
  type = "containsText",
  rule = "toto",
  style = pink_fill  # Apply the fill directly
)

# Save the workbook
save_wb(wb, "conditional_format_toto.xlsx")
