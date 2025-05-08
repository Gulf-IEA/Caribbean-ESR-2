#' Write parsed GitHub issue data to a Quarto (.qmd) file
#'
#' @param clean_df A named list or data frame containing cleaned issue data (output from create_object)
#' @param output_path Path to the .qmd file to be written (including filename)
#' @export
write_qmd <- function(clean_df, output_path = "issue.qmd") {
  
  # Helper function to turn Define Variables text into a markdown table
  process_define_variables <- function(text) {
    # Split based on "Name: " but skip any empty splits
    chunks <- strsplit(text, "Name: ")[[1]]
    chunks <- chunks[nzchar(chunks)]
    
    rows <- lapply(chunks, function(chunk) {
      parts <- unlist(strsplit(chunk, ",\\s*"))
      
      name <- trimws(parts[1])
      definition <- trimws(sub("^Definition:\\s*", "", parts[2]))
      units <- trimws(sub("^Units:\\s*", "", parts[3]))
      
      c(name, definition, units)
    })
    
    # Build markdown table
    table <- c(
      "| Name | Definition | Units |",
      "|------|------------|-------|"
    )
    for (r in rows) {
      table <- c(table, paste0("| ", paste(r, collapse = " | "), " |"))
    }
    paste(table, collapse = "\n")
  }
  
  # Extract and clean the title for the YAML header
  raw_title <- clean_df[["Data Name (This will be the displayed title in the Technical Documentation)"]]
  cleaned_title <- gsub("^\\s+|\\s+$", "", raw_title) # trim whitespace
  cleaned_title <- gsub("\n", "", cleaned_title)      # remove line breaks
  
  # Start YAML
  content <- c(
    "---",
    paste0("title: \"", cleaned_title, "\""),
    "format: html",
    "---",
    ""
  )
  
  # Loop through all fields except the title
  for (field in names(clean_df)) {
    if (field != "Data Name (This will be the displayed title in the Technical Documentation)") {
      cleaned_field <- gsub("\\.", " ", field)  # replace periods with spaces
      value <- trimws(clean_df[[field]])
      
      # Handle checkboxes: keep only checked items, strip markdown
      if (grepl("- \\[", value)) {
        checked_items <- regmatches(value, gregexpr("- \\[x\\] [^\n]+", value, ignore.case = TRUE))[[1]]
        value <- gsub("- \\[x\\] ?", "", checked_items)
        value <- paste(value, collapse = ", ")
      }
      
      # Handle Define Variables section as a table
      if (field == "Define Variables") {
        value <- process_define_variables(value)
      }
      
      # Add to document
      content <- c(content, paste0("## ", cleaned_field), "", value, "")
    }
  }
  
  # Write to file
  writeLines(content, con = output_path)
  message("Quarto file written to ", output_path)
}
