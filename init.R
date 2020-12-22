my_packages = c("shiny", "shinythemes", "knitr", "shinyWidgets", "data.table", "ggplot2", "caret", "RCurl", "FNN", "class", "gmodels", "splitstackshape", "tidyverse", "RDS", "packrat", "randomForest")

install_if_missing = function(k) {
    if (k %in% rownames(installed.packages()) == FALSE) {
        install.packages(k)
    }
}

invisible(sapply(my_packages, install_if_missing))
