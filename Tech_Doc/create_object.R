#' Create a cleaned named list from parsed GitHub issue data
#'
#' @param parsed A named list from `parse_issue()`
#' @return A cleaned list with trimmed text and preserved checkboxes
#' @export
create_object <- function(parsed) {
  lapply(parsed, function(x) {
    # If it includes checkbox markdown, preserve it line-by-line
    if (grepl("- \\[", x)) {
      # Keep each line, trim whitespace
      lines <- unlist(strsplit(x, "\n"))
      cleaned_lines <- trimws(lines)
      cleaned_lines <- cleaned_lines[cleaned_lines != ""]
      paste(cleaned_lines, collapse = "\n")
    } else {
      # Otherwise clean up normally
      x <- gsub("\n", " ", x)
      x <- gsub("\\s+", " ", x)
      trimws(x)
    }
  })
}
