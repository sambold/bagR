#' init_packages: installiert Packages
#'
#' Installiert eine Liste von Packages. Werden keine Packages angegeben, wird
#' eine Liste von Standardpackages installiert. Dabei kann angegeben werden, ob
#' nur Packages installiert werden sollen, die nicht schon vorhanden sind oder
#' ob eine (Neu)installation erzwungen werden soll. 
#'
#' @param pkge_list String, Standardwert = NULL, Liste zu installierender Packages
#'     wenn NULL, werden Standardpackages installiert
#' @param force_install Boolean, Standardwert = FALSE, gibt an, ob Installation
#'     auch dann stattfinden soll, wenn Package bereits vorhanden
#' @return Vektor mit Installationsstatus der einzelnen Packages: TRUE = Installation
#'     hat funktioniert, FALSE = Installation fehlgeschlagen
#' @export
#'
init_packages <- function(pkg_list=NULL,
                          force_install=FALSE){
  if (is.null(pkg_list)){
    pkg_list <- c(
      # Allgemein
      "tidyverse",
      "beepr",
      "parallel",
      "RColorBrewer",
      "ggsci",
      "extrafont",
      "roxygen2",
      "devtools",
      "secret",
      # Import
      "xml2",
      "httr",
      "rvest",
      "jsonlite",
      "RCurl",
      # Daten manipulieren
      "plyr",
      # Analyse
      "tm",
      "SnowballC",
      "tidytext",
      "tidygraph",
      #"igraph",
      "caret",
      "outliers",
      "twitteR",
      #"nnet",
      # Visualisierung
      "maps",
      "ggmap",
      "plotly",
      "wordcloud",
      #"labeling",
      # Report
      "shiny",
      "shinydashboard",
      "knitr",
      "rmarkdown",
      "xtable",
      "rJython",
      "RSelenium")
    
  }
  if (!force_install) pkg_list <- pkg_list[!(pkg_list %in% installed.packages()[, "Package"])]
  if (length(pkg_list)>0) install.packages(pkg_list, dependencies = TRUE)
  sapply(pkg_list, require, character.only = TRUE)
}
