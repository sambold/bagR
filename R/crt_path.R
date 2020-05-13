#' crt_path: Verzeichnis anlegen
#'
#' versucht Verzeichnis anzulegen und gibt Statusinfo zurueck.
#'
#' @param x Character, anzulegender Pfad
#' @return Boolean, gibt an, ob Verzeichnis erfolgreich angelegt wurde oder nicht.
#' @export
#'
crt_path <- function(x){
    crt <- NA
    if (!file.exists(x)) {
        crt <- dir.create(x)
        if (crt) {
            message("## Pfad wurde erfolgreich angelegt: ",x,"...\n")
        } else {
            message("## Pfad konnte nicht angelegt werden: ",x,"...\n")
        }
    } else {
        message("## Pfad bereits vorhanden: ",x,"...\n")
    }
    return(crt)
}