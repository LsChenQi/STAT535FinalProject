# set the working directory to use relative path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

tables_html = readLines("../Data/Tables.html", warn = FALSE)