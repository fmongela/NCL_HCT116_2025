# Parse and graph Revvity XML plate assay definition.
# should work with any user defined parameters
# and any plate types are these are not hard coded

lib2load <- c("here", "xml2", "tidyverse", "gridExtra")
lapply(lib2load, library, character.only = TRUE)

rm(list = ls())

# The XML to be treated
xml_file <- "2577f4a7-3b8f-4c5c-aece-cda68e3d69c7.xml"

# Load XML; here() gives github/R directory structure consistency
# clean the toxic Revvity declarations; no  specs from Revvity www
file_path <- here("data", xml_file)
 
# Read the XML file
doc <- read_xml(file_path)


# Inspect and print namespace structure
ns <- xml_ns(doc)
print(ns)

# Rename namespace for easier use 
ns <- xml_ns_rename(ns, d1 = "ns")

# Extract GroupName attributes from Registration section
registration_contents <- xml_find_all(doc, ".//ns:Registration/ns:Content", ns)

group_names <- tibble(
  Content_ID = xml_attr(registration_contents, "ID"),
  GroupName = xml_attr(registration_contents, "GroupName")
)

print(group_names) 

# Extract well data dynamically
wells <- xml_find_all(doc, ".//ns:Well", ns)

data_list <- lapply(wells, function(well) {
  well_data <- list(
    Well_ID = xml_attr(well, "WellID"),
    Row = xml_attr(well, "Row"),
    Column = xml_attr(well, "Column")
  )
  
  for (i in seq_along(group_names$Content_ID)) {
    content_id <- group_names$Content_ID[i]
    group_name <- group_names$GroupName[i]
    
    value <- xml_text(xml_find_first(well, paste0("./ns:Content[@ContentID='", content_id, "']/ns:Value"), ns))
    well_data[[group_name]] <- value
  }
  
  return(well_data)
})

data <- bind_rows(data_list)

# Print the tibble
print(data)

