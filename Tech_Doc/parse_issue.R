#' Parse a single Catalog submission issue from Github
#' 
#' @param issueData list. output from GitHub API pull of ALL issues
#' @param issueNum numeric. The number of the issue as assigned by GitHub
#' 
#' @return list of containing issue heading names with associated content
#' 

parse_issue <- function(issueData,issueNum){
  
  objectList <- list()
  if (length(issueData$issues$number)==1) {
    # single issue
    body <- issueData$issues$body
  } else {
    # multiple issues. pick the right one
    id <- which(issueData$issues$number==issueNum)
    body <- issueData$issues[id,]$body
  }
  
  headings <- unlist(stringr::str_extract_all(body,"\n###\\s+[a-zA-Z (.)\"\\?,]+"))

  for (ahead in headings) {
    modifiedHead <- gsub("\\","",ahead,fixed=T)
    modifiedHead <- gsub("(","\\(",modifiedHead,fixed=T)
    modifiedHead <- gsub(")","\\)",modifiedHead,fixed=T)
    modifiedHead <- gsub("?","\\?",modifiedHead,fixed=T)
    byhead <- unlist(strsplit(body,modifiedHead))[2]
    res <- unlist(strsplit(byhead,"\n### "))[1]
    # remove beginning and trailing \n (line feed) and \r (carriage return)
    modifiedRes <- trimws(res)
    # modifiedRes <- sub("\\n+$","",modifiedRes)
    # modifiedRes <- sub("^\\r\\n+","",modifiedRes)
    # modifiedRes <- sub("\\r\\n+$","",modifiedRes)
    ahead <- trimws(ahead)
    objectList[ahead] <- modifiedRes
  }

  return(objectList)
  
}