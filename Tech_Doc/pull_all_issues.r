#' Pulls all issues from GitHub API
#'
#' Issues that we are interested in have a "submission" tag associated with them.
#' This submission tag is assigned in GitHub to the the issue Template 
#' so when a submission is entered it is automatically tagged
#'
#'
#' @return list object
#' \item{issueData}{a list of all issues . Each element is an issue}
#' \item{submissions}{the issue numbers that are "submissions"}



pull_all_issues <- function() {
  # Define repo information, pull issues from GH API and subset 'submissions'
  repo <- 'https://api.github.com/repos/Gulf-IEA/Caribbean-ESR-2/issues'
  # pull all issues and select all submission issues
  issueData <- list()
  # pulls 100 issues
  repopull <- paste0(repo,"?per_page=200")
  issues <- jsonlite::fromJSON(repopull)
  # make sure it has the "submission" tag associated with it
  indices <- which(unlist(lapply(issues$labels,function(x) {if(length(x$name)==0){F}else{x$name=="submission"}})))
  submissions <- issues$number[indices]
  # save pull to rds for debugging
  issueData$issues <- issues
  issueData$submissions <- submissions
  saveRDS(issueData,here::here("data-raw/submissionIssueNumbers.rds"))
  
  return(issueData)
  
}