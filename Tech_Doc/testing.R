
library(fs)

issue <- pull_single_issue(9)
parsed <- parse_issue(issue$body)
clean_df <- create_object(parsed)
write_qmd(clean_df, output_path = "Tech_Doc/Test_data.qmd")



issue2 <- pull_single_issue(10)
parsed2 <- parse_issue(issue2$body)
clean_df2 <- create_object(parsed2)
write_qmd(clean_df2, output_path = "Tech_Doc/Test_data_2.qmd")


# Render Tech_Doc in place
quarto::quarto_render("Tech_Doc/index.qmd")

# Then copy the output to _book/tech-doc
fs::dir_copy("Tech_Doc/_site", "_book/tech-doc", overwrite = TRUE)







library(httr)
library(jsonlite)
library(glue)
source("Tech_Doc/pull_single_issue.R")
source("Tech_Doc/parse_issue.R")
source("Tech_Doc/create_object.R")
source("Tech_Doc/write_qmd.R")

# --- Get all issues from the repo ---
get_all_issues <- function(repo) {
  url <- glue("https://api.github.com/repos/{repo}/issues?state=all&per_page=100")
  fromJSON(url)
}

# --- Filter issues with 'submission' label ---
get_submission_issues <- function(issues) {
  issues[sapply(issues$labels, function(labs) {
    any(labs$name == "submission")
  }), ]
}

repo = "Gulf-IEA/Caribbean-ESR-2"
output_dir = "Tech_Doc/issues"
i=2
issue_num=9

# --- Process all submission issues ---
process_all_submissions <- function(repo = "Gulf-IEA/Caribbean-ESR-2", output_dir = "Tech_Doc/issues") {
  issues <- get_all_issues(repo)
  submissions <- get_submission_issues(issues)
  
  message(glue("Found {nrow(submissions)} issues labeled 'submission'."))
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  for (i in seq_len(nrow(submissions))) {
    issue_num <- submissions$number[i]
    title <- submissions$title[i]
    
    message(glue("Processing issue #{issue_num}: {title}"))
    
    # Pull full issue content (includes body)
    issue_data <- pull_single_issue(issue_num, repo)
    
    # Parse the markdown-formatted body
    parsed <- parse_issue(issue_data$body)
    
    # Clean and structure into a final list
    cleaned <- create_object(parsed)
    
    # Clean up the filename based on title
    safe_title <- title |>
      gsub("^submission_", "", x = _) |>          # Remove "submission_" from the start of the title
      gsub("[^a-zA-Z0-9_]+", "_", x = _) |>       # Replace non-alphanumeric characters with underscores
      gsub("_+", "_", x = _) |>                   # Collapse multiple underscores
      gsub("^_|_$", "", x = _)                    # Remove leading/trailing underscores
    
    # Check if safe_title is empty and handle that case
    if (safe_title == "") {
      safe_title <- "untitled_issue"  # Default name if the title was empty after cleaning
    }
    
    filename <- file.path(output_dir, paste0(safe_title, ".qmd"))
    
    # Write the .qmd file
    write_qmd(cleaned, filename)
  }
}



process_all_submissions()
