#' list_container:
#' 
#' @description Listet alle laufenden Container auf
#' @param opts List, Standardwert NULL; wenn NULL wird opts ueber die Funktion
#'     set_auto_sudo() befuellt. Dabei werden zwei Listelemente zugewiesen
#'     \itemize{
#'         \item opt: gibt an, ob Passwort automatisch ("-Ks") oder manuell ("") 
#'             im Terminal eingegeben werden muss
#'         \item sudo_pswd: Passwort zur Bestaetigung, wenn opt="-Ks"
#'     }
#' @return String
#' @export
#' 
list_container <- function(opts=NULL){
  if (is.null(opts)){
    opts <- set_auto_sudo()
  }
  # list docker container
  system(glue::glue("sudo {opts$opt} docker ps"),
         input=ifelse(opts$use_vault, bagR::get_secret(opts$secret),opts$secret))
}