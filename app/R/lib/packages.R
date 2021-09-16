# Title     : Loads required packages, installing if required
# Created by: PatientYesterday10
# Created on: 2020-12-06

pkgs <- c("data.table", "ggplot2", "patchwork", "nord", "httr", "zoo", "ggrepel", "readr","lubridate","forecast","caret","e1071","gbm","randomForest","xgboost","h2o")
lapply(pkgs, FUN = function(x) {
  if (!(x %in% installed.packages())) {
    install.packages(pkgs = x,repos = "http://cran.us.r-project.org")
  }
  library(package = x, character.only = TRUE)
})
