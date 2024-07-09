# ST558Proj2MB

This app allows the user to query clinical and gene expression (mRNA) data for 
3 types of cancer using the FireBrowse API. The user can select 6 combinations
of endpoints and cancers and subsequently view the data, subsest the data and view
graphical and numerical summaries.

Packages needed:
shiny
tidyverse
httr
jsonlite
DT

All packages can be installed using this code:
install.packages(c("shiny", "tidyverse", "httr", "jsonlite", "DT"))


shiny::runGitHub("ST558Proj2MB", "mbb477", subdir = "Project2App")






