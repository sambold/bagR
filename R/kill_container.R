#' kill_container:
#' 
#' @description beendet bestimmte(n) Container
#' @param opts List, Standardwert NULL; wenn NULL wird opts ueber die Funktion
#'     set_auto_sudo() befuellt. Dabei werden zwei Listelemente zugewiesen
#'     \itemize{
#'         \item opt: gibt an, ob Passwort automatisch ("-Ks") oder manuell ("") 
#'             im Terminal eingegeben werden muss
#'         \item sudo_pswd: Passwort zur Bestaetigung, wenn opt="-Ks"
#'     }
#' @param container String, Name eines Containers, der beendet werden soll. 
#'     Alternativ zu einem String, kann auch eine Liste mit Strings uebergeben
#'     werden. Wenn NULL uebergeben wird, werden alle laufenden Container beendet.
#' @return Information zu beendeten Containern
#' @export
#' 

# Beende bestimmte(n) Container
kill_container <- function(opts=NULL,
                           container=NULL){
  if (is.null(opts)){
    opts <- set_auto_sudo()
  }
  if (is.null(container)){
    container <- get_running_container(opts=opts)
  }
  lapply(container,function(f) {
    system(glue::glue("sudo {opts$opt} docker stop {f}"),
           input=ifelse(opts$use_vault, bagR::get_secret(opts$secret),opts$secret))
  })
}