# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "shinythemes", "rgl", "DT", "dplyr", "leaflet", "gt", "scales", "hash", "plotly",
              "chron","ggplot2","viridis","scales","tidyverse","cowplot","extrafont","ggforce","comprehenr",
               "stringr","LaplacesDemon","data.table","lutz","lubridate")
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))