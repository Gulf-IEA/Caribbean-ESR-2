#' Pull a single GitHub issue (must be labeled 'submission')
#'
#' @param issueNum Numeric. GitHub issue number.
#' @param repo Character. GitHub repo in the format "owner/repo". Default is "Gulf-IEA/Caribbean-ESR-2".
#'
#' @return A list with the parsed issue content.
#' @export
pull_single_issue <- function(issueNum, repo = "Gulf-IEA/Caribbean-ESR-2") {
  
  message(glue::glue("Pulling issue #{issueNum} from GitHub repo {repo}..."))
  
  # Construct API URL
  api_url <- paste0("https://api.github.com/repos/", repo, "/issues/", issueNum)
  
  # Try pulling the issue
  tryCatch({
    issue <- jsonlite::fromJSON(api_url)
  }, error = function(e) {
    stop("Failed to pull issue from GitHub. Check issue number and repo.")
  })
  
  # Check for 'submission' label
  if (!"submission" %in% issue$labels$name) {
    stop(glue::glue("Issue #{issueNum} is not labeled as 'submission'."))
  }
  
  return(list(
    issue_number = issueNum,
    title = issue$title,
    body = issue$body,
    labels = issue$labels$name,
    issue_data = issue
  ))
}
