#' check_for_images:
#' 
#' @description prueft ueber Terminal-Input, ob ein Image schon vorhanden ist.
#' @param opts List, Standardwert NULL; wenn NULL wird opts ueber die Funktion
#'     set_auto_sudo() befuellt. Dabei werden zwei Listelemente zugewiesen
#'     \itemize{
#'         \item opt: gibt an, ob Passwort automatisch ("-Ks") oder manuell ("") 
#'             im Terminal eingegeben werden muss
#'         \item sudo_pswd: Passwort zur Bestaetigung, wenn opt="-Ks"
#'     }
#' @param image String, Pfad zum Image, das geprueft werden soll
#' @return Boolean
#' @export
#' 
check_for_image <- function(opts=NULL,
                            image="selenium/standalone-firefox-debug:latest"){
  # Optionen fuer Sudo festlegen 
  if(is.null(opts)){
    opts <- set_auto_sudo()
  }
  # pruefen, ob image bereits vorhanden
  filter <- gsub(":"," *",image)
  res <- system(glue::glue("sudo {opts$opt} docker images | grep '{filter}'"),
                input=ifelse(opts$use_vault, bagR::get_secret(opts$secret),opts$secret),
                intern=T) %>%
    length(.)>0
  return(res)
}