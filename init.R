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

install.packages("leaflet.tar.gz", repos=NULL, type="source")
install.packages("cowplot.tar.gz", repos=NULL, type="source")
install.packages("extrafont.tar.gz", repos=NULL, type="source")
install.packages("ggforce.tar.gz", repos=NULL, type="source")
install.packages("comprehenr.tar.gz", repos=NULL, type="source")
install.packages("stringr.tar.gz", repos=NULL, type="source")
install.packages("LaplacesDemon.tar.gz", repos=NULL, type="source")
install.packages("lutz.tar.gz", repos=NULL, type="source")
install.packages("lubridate.tar.gz", repos=NULL, type="source")
install.packages("tidyverse.tar.gz", repos=NULL, type="source")
