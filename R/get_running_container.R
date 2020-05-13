#' get_running_container:
#' 
#' @description listet die Namen laufender Container auf
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
get_running_container <- function(opts=NULL){
  if (is.null(opts)){
    opts <- set_auto_sudo()
  }
  # Namen extrahieren
  res <- system(glue::glue("sudo {opts$opt} docker ps --format 'table {{{{.Names}}}}'"),
                input=ifelse(opts$use_vault, bagR::get_secret(opts$secret),opts$secret),
                intern=T) %>%
    .[.!="NAMES"] 
  if (length(res)==0) res <- NULL
  return(res)
}