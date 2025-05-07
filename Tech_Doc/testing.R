issue <- pull_single_issue(9)
parsed <- parse_issue(issue$body)
str(parsed, max.level = 1)


parsed <- parse_issue(issue$body)
clean_df <- create_object(parsed)
write_qmd(clean_df, output_path = "Tech_Doc/Test_data.qmd")
