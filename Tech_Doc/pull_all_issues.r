#' Pull all GitHub issues labeled "submission"
#'
#' @param repo Character. GitHub repo in the format "owner/repo", e.g., "Gulf-IEA/Caribbean-ESR-2".
#' @param state Character. One of "open", "closed", or "all". Defaults to "open".
#' @param per_page Numeric. Number of issues per page (max 100).
#'
#' @return A list of issues (each issue is a named list).
#' @export
pull_all_issues <- function(repo = "Gulf-IEA/Caribbean-ESR-2", state = "open", per_page = 100) {
  message(glue::glue("Pulling all '{state}' issues labeled 'submission' from {repo}..."))
  
  base_url <- glue::glue("https://api.github.com/repos/{repo}/issues")
  page <- 1
  all_issues <- list()
  
  repeat {
    query_url <- glue::glue("{base_url}?state={state}&per_page={per_page}&page={page}")
    response <- jsonlite::fromJSON(query_url)
    
    if (length(response) == 0) break
    
    # Filter issues that contain the "submission" label
    submission_issues <- response[sapply(response$labels, function(lbls) {
      "submission" %in% lbls$name
    })]
    
    all_issues <- append(all_issues, submission_issues)
    page <- page + 1
  }
  
  message(glue::glue("Retrieved {length(all_issues)} submission issues."))
  return(all_issues)
}
