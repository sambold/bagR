#' conv_number: wandelt Zahlenformat um
#'
#' Wandelt Zahlen-Strings in europaeischem Format in R-Zahlenwerte (ohne 
#' Gruppierungstrenner, nur mit Punkt als Komma) um. Zudem koennen R-Zahlenwerte
#' fuer die Ausgabe in Strings umgewandelt werden, die eine entsprechende 
#' Formatierung besitzen (US/DE):
#' \itemize{
#'     \item US: Gruppierungstrennzeichen="," - Komma="."
#'     \item DE: Gruppierungstrennzeichen="." - Komma=","
#' }
#' Die Auswahl von US/DE gibt einen String, die Auswahl R einen numerischen Wert
#' zurueck.
#'
#' @param x String/Numeric; Wert, der umgewandelt werden soll
#' @param to String, Standardwert = "R"; gibt an in welches Format umgewandelt 
#'     werden soll:
#'     \itemize{
#'         \item R: Umwandlung in R-spezifischen, numerischen Wert
#'         \item US: Umwandlung in amerikan. Zahlenformat
#'         \item DE: Umwandlung in europ. Zahlenformat
#'     }
#' @param ... weitere Parameter fuer format
#' @return Numeric (to=R), String (to=US/DE)
#' @examples
#' conv_number(x="1234,56")
#' conv_number(x="1.234,56")
#' conv_number(x=1234.56,to="US")
#' conv_number(x=1234.56,to="DE")
#' conv_number(x="1234.56",to="DE")
#' conv_number(x=123456789,99,to="DE",digits=15)
#' @export
#'
conv_number <- function(x,to="R",...){
  library(magrittr)
  if (to=="R"){
    res <- x %>%
      gsub("\\.","",.) %>%
      gsub(",","\\.",.) %>%
      as.numeric()
  } else if (to=="US"){
    res <- x %>%
      format(big.mark=",",small.mark=",",decimal.mark=".",...)
  } else if (to=="DE"){
    res <- x %>%
      format(big.mark=".",small.mark=".",decimal.mark=",",...)
  } else {
    warning(paste0("Ungueltiges Konvertierungsformat angegeben.", 
                   "Nur die Optionen R, US und DE stehen zur Verfuegung."))
  }
  return(res)
}
