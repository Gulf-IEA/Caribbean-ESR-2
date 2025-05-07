#' Parse sections of a GitHub issue body into a named list
#'
#' This function parses a GitHub issue body (typically written using a Markdown template)
#' into a named list where each element corresponds to the content under a `###` header.
#' The headers are used as list names, and the text between each header and the next
#' header is stored as the value.
#'
#' @param issue_body A character string representing the full Markdown-formatted body of a GitHub issue.
#'
#' @return A named list, where each name is a header (without the `### ` prefix), and each
#' value is a character string containing the text below that header (until the next `###`).
#'
#' @examples
#' body_text <- "### Section One\nSome content here.\n\n### Section Two\n- [x] Option A\n- [ ] Option B"
#' parse_issue(body_text)
#' # Returns: list("Section One" = "Some content here.", "Section Two" = "- [x] Option A\n- [ ] Option B")
#'
#' @export
parse_issue <- function(issue_body) {
  lines <- unlist(strsplit(issue_body, "\n"))
  
  # Identify the lines that are headers
  header_lines <- grep("^### ", lines)
  
  # Extract header names (remove the ### and leading/trailing whitespace)
  headers <- trimws(gsub("^###\\s*", "", lines[header_lines]))
  
  # Initialize result list
  result <- list()
  
  # Loop through headers and extract the section content
  for (i in seq_along(header_lines)) {
    start <- header_lines[i] + 1
    end <- if (i < length(header_lines)) header_lines[i + 1] - 1 else length(lines)
    
    section <- lines[start:end]
    # Remove trailing empty lines
    section <- sub("\\s+$", "", section)
    section_text <- paste(section, collapse = "\n")
    
    result[[headers[i]]] <- section_text
  }
  
  return(result)
}
