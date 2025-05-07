#' Write parsed GitHub issue data to a Quarto (.qmd) file
#'
#' @param clean_df A named list or data frame containing cleaned issue data (output from create_object)
#' @param output_path Path to the .qmd file to be written (including filename)
#' @export
write_qmd <- function(clean_df, output_path = "issue.qmd") {
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
      content <- c(content, paste0("## ", cleaned_field), "", value, "")
    }
  }
  
  # Write to file
  writeLines(content, con = output_path)
  message("Quarto file written to ", output_path)
}
