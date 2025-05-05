#' Create markdown file using parsed github issue
#' 
#' Template structure for the rmd is hard coded in this function. 
#' The template used data from github parsed github submission issue
#' 
#' @param listobject list. parsed issue from github
#' 
#' @return creates an rmd in chapters folder. The name of the rmd is the indicator name

make_rmd <- function(listobject){
  
  # line for the indicator name
  #indicator_name <- listobject$name
  # create filename based on indicator name. Replace all spaces with underscore
  #filename <- gsub("\\s+","_",indicator_name)
  filename <- listobject$indicatorname
  
  # create rmd with name of indicator
  con <- file(here::here("chapters",paste0(filename,".rmd")),open="w")
     
  # start to create the Rmd
  #cat(paste0("# ",stringr::str_to_title(indicator_name)),append=T,fill=T,file=con)  
  ### DESCRIPTION, CONTRIBUTORS, AFFILIATION, FAMILY
  cat(paste0("# ",listobject$dataname," {#",listobject$indicatorname,"}"),append=T,fill=T,file=con)    
  #cat(paste0("# ",listobject$dataname),append=T,fill=T,file=con)    
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Description**: ",listobject$description),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat("**Indicator family**: ",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$family,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Contributor(s)**: ",listobject$contributors),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Affiliations**: ",listobject$affiliations),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  # chunk script
  cat("```{r echo=FALSE}",append=T,fill=T,file=con)
  cat("knitr::opts_chunk$set(echo = F)",append=T,fill=T,file=con)
  cat("library(ecodata)",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)
  
  ### DESCRIPTION OF INDICATOR
  cat("## Introduction to Indicator",append=T,fill=T,file=con)
  cat(listobject$whatsthis,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### PLOT INDICATOR
  cat("## Key Results and Visualizations",append=T,fill=T,file=con)
  cat(listobject$visualizations,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  
  ### SPATIAL + TEMPORAL SCALE
  cat("",append=T,fill=T,file=con) # add space
  cat("## Indicator statistics ",append=T,fill=T,file=con)
  cat(paste0("Spatial scale: ",listobject$indicatorStatsSpatial),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("Temporal scale: ",listobject$indicatorStatsTemporal),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### SYNTHESIS THEME
  cat("**Synthesis Theme**:",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$synthesisTheme,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  # r chunk for autostats
  cat(paste0("```{r autostats_",filename,"}"),append=T,fill=T,file=con)
  cat("# Either from Contributor or ecodata",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### IMPLICATIONS 
  cat("## Implications",append=T,fill=T,file=con)
  cat(listobject$implications,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  
  cat("## Get the data",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### CONTACT PERSON
  cat(paste0("**Point of contact**: [",listobject$poc,"](mailto:",listobject$poc,"){.email}"),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  if (exists(paste0(listobject$indicatorname))) {
    cat(paste0("**ecodata name**: `ecodata::",listobject$indicatorname,"`"),append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
  } else {
    cat(paste0("**ecodata name**: No dataset"),append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con)
  }
  ### VARIABLES FOUND IN DATA
  cat("**Variable definitions**",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space

  # grabs the defined variables and writes out as an unordered list
  cat(unlist(strsplit(listobject$defineVariables,"\\n")),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space

  # check to see if data name exists. Some do not but need a catalog page
  if (exists(paste0(listobject$indicatorname))) {  
    # check to see if Var field exists
    if ("Var" %in% names(eval(parse(text=paste0("ecodata::",listobject$indicatorname))))) {
      # r code chunk to make table
      cat(paste0("```{r vars_",listobject$indicatorname,"}"),append=T,fill=T,file=con)
      cat("# Pull all var names",append=T,fill=T,file=con)
    
      cat(paste0("vars <- ecodata::",listobject$indicatorname," |>"),append=T,fill=T,file=con)
      if (eval(parse(text=paste0("'Units' %in% names(ecodata::",listobject$indicatorname,")")))) {
        cat("   dplyr::select(Var, Units) |>",append=T,fill=T,file=con)
      } else {
        cat("   dplyr::select(Var) |>",append=T,fill=T,file=con)
      }
      cat("   dplyr::distinct()",append=T,fill=T,file=con)
      cat("",append=T,fill=T,file=con) # add space
      cat("DT::datatable(vars)",append=T,fill=T,file=con) # add space
      cat("```",append=T,fill=T,file=con)
    }
  } else {
    cat("",append=T,fill=T,file=con) # add space
    cat("No Data",append=T,fill=T,file=con) # add space
    cat("",append=T,fill=T,file=con) # add space
  }
  
  ### INDICATOR CATEGORY
  cat("**Indicator Category**:",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$indicatoryCategory,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  if(!(listobject$other == "_No response_")) {
    cat("**Indicator Category**:",append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
    cat(listobject$other,append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
  }
  
  ### PUBLIC AVAILABILITY + ACCESSIBILITY
  
  cat("## Public Availability",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$publicAvailability,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  cat("## Accessibility and Constraints",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$accessibility,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  

  # write catalog link if ecodata data is present
  if (exists(paste0(listobject$indicatorname))) {
    cat("**tech-doc link**",append=T,fill=T,file=con)
    
    cat(paste0("<https://noaa-edab.github.io/tech-doc/",listobject$indicatorname,".html>"),append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
    
  } else {
    # No link since this is a synthesis type of page
  }
  
  # References are generated automatically  

  
  #close the connection
  close(con)
  
 
}