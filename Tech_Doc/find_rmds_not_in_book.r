# Check to see if all rmds are present in bookdown.yml


find_rmds_not_in_book <- function(){
  
  # find all rmds listed int he chapters folder
  rmdfiles <- list.files(here::here("chapters"))
  
  # find all the rmds referenced in the bookdown yml
  ymlfile <- readLines(con = here::here("_bookdown.yml"))
  rmdrefs <- NULL
  for (iline in 1:length(ymlfile)) {
    if (grepl("chapters",ymlfile[iline])) {
      if (!grepl("sectionHeaders",ymlfile[iline])) {
        ## these are rmds in book
        fname <- stringr::str_extract(ymlfile[iline],"[a-zA-Z_]+.rmd")
        rmdrefs <- c(rmdrefs,fname)
        
      }
    }
  }

  # find the rmdfiles not referenced in the yml
  missingrmds <- setdiff(rmdfiles,rmdrefs)
  message("The following rmds are not found in the _bookdown.yml")
  #print(missingrmds)
  
  return(missingrmds)
  
  
  
  
}
