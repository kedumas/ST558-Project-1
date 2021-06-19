rmarkdown::render("NHL API Vignette.Rmd",
                  output_format = github_document(toc = TRUE),
                  output_file = "README")