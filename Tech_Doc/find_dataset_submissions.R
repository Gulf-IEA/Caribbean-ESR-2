#' Find data entry GitHub issues
#'
#' Finds indicator name and issue number for all indicators submitted
#'
#' @return character vector

source(here::here("R/parse_issue.r"))

find_dataset_submissions <- function(){
  
  issueData <- readRDS(here::here("data-raw/submissionIssueNumbers.rds"))
  submissions <- issueData$submissions
  out <- NULL
  ic <- 0
  for (i in submissions) {
    ic <- ic + 1
    parsedIssues <- parse_issue(issueData,i)
    out[ic] <- paste0(i, " - ",parsedIssues$`### Indicator Name (as exists in ecodata)`)
    message(out[ic])
  }
  
}