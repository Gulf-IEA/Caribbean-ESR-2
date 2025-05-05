#' Reads in issues from GitHub and create rmds
#' 
#' Some of the GitHub issues in catalog are tagged as "submission".
#' These issues are filtered and parsed then used to build catalog rmds.
#' Each rmd will represent a catalog index
#' 
#' @param pullAllIssues Boolean. Should all issues be pulled from Github (Default = F). When TRUE the pull is saved locally
#' @param pullSingleIssue Boolean. should a single issue be pulled from Github (Default = F)
#' @param issueNum numeric. The GitHub issue number to be parsed  (Default = 1). If issueNum = NULL then 
#' ALL issues will be processed from a previous pull using (pullAllIssues = T) 
#' 
#' @return list
#' \item{listobject}{list formatted in a way to pass to make_rmd function. Mainly used for debugging}

# Source required functions
source(here::here("R/parse_issue.R"))
source(here::here("R/create_listobject.R"))
source(here::here("R/make_rmd.R"))
source(here::here("R/pull_all_issues.R"))
source(here::here("R/pull_single_issue.R"))


build_rmd_from_issue <- function(pullAllIssues=F,pullSingleIssue= F,issueNum = 1) {

  if (pullAllIssues){ 
    # this will pull all issues from GitHub API
    issueData <- pull_all_issues()
  } else {
    if(pullSingleIssue) {
      # pull single issue from GitHub
      issueData <- pull_single_issue(issueNum)
    } else {
      # read in previously pulled set of GitHub Issues
      issueData <- readRDS(here::here("data-raw/submissionIssueNumbers.rds"))
      if(is.null(issueNum)) {
        # Process  all
      } else {
        # Process single issue
        issueData$submissions <- issueNum
      }
        
    }
  }
  
  # select all submission issues
  submissions <- issueData$submissions
  

  # Process the issues to create an rmd for bookdown
  for (issuenum in submissions) {
    message(paste0("Issue number: ",issuenum))
    ### Parse issue 
    parsedIssue <- parse_issue(issueData,issuenum)
    
    # create list of parsed issue values
    listobject <- create_listobject(parsedIssue)
    message(paste0("making ... chapters/",listobject$indicatorname,".rmd"))
    ### make an rmd from content
    make_rmd(listobject)
    
  }
  
  message("Look in the 'chapters' folder to see the rmds")
  
  return(invisible(listobject))

}

