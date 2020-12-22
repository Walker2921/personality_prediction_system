my_packages = c("shiny", "shinythemes", "knitr", "shinyWidgets", "data.table", "ggplot2", "RCurl", "tidyverse", "base", "datasets", "dplyr", "forcats", "graphics", "grDevices", "methods", "purrr", "readr", "stats", "stringr", "tibble", "tidyr", "utils")

                
install_if_missing = function(k) {
    if (k %in% rownames(installed.packages()) == FALSE) {
        install.packages(k)
    }
}

invisible(sapply(my_packages, install_if_missing))
