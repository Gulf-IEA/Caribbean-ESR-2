issue <- pull_single_issue(9)
parsed <- parse_issue(issue$body)
clean_df <- create_object(parsed)
write_qmd(clean_df, output_path = "Tech_Doc/Test_data.qmd")



issue2 <- pull_single_issue(10)
parsed2 <- parse_issue(issue2$body)
clean_df2 <- create_object(parsed2)
write_qmd(clean_df2, output_path = "Tech_Doc/Test_data_2.qmd")
