#' Pull a single issue from Github
#' 
#' @param issueNum numeric. The number of the issue as assigned by GitHub
#' 
#' @return list of containing issue
#' 

pull_single_issue <- function(issueNum){
  
  message("Pulling from GitHub")
  issueData <- list()
  repo <- 'https://api.github.com/repos/Gulf-IEA/Caribbean-ESR-2/issues'
  issue <- jsonlite::fromJSON(paste0(repo,"/",issueNum))

  if(issue$labels$name == "submission") {
    submissions <- issueNum
  } else {
    stop(paste0("This issue number (#",issueNum,") is not associated with GitHub issue tagged as a 'submission' and can not be parsed"))
  }
  
  issueData$issues <- issue
  issueData$submissions <- submissions

  return(issueData)
  
}
