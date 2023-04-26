# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "shinythemes", "rgl", "DT", "dplyr", "leaflet", "gt", "scales", "hash", "plotly",
              "chron","viridis","scales","tidyverse","cowplot","extrafont","ggforce","comprehenr",
               "stringr","LaplacesDemon","data.table","lutz","lubridate","geomtextpath")
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

install.packages("packages/leaflet.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/cowplot.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/extrafont.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/ggforce.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/comprehenr.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/stringr.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/LaplacesDemon.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/lutz.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/lubridate.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/tidyverse.tar.gz", dependencies = TRUE, repos=NULL, type="source")
install.packages("packages/ggplot2.tar.gz", dependencies = TRUE, repos=NULL, type="source")
