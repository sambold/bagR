#' euro_format: wandelt uebergebene Zahlenwerte ins Euro-Format um
#'
#' Zahlenwerte werden ins Euro-Format umgewandelt. Dabei werden Tausender-Trennzeichen
#' und Komma-Angaben nach europaeischem Standard verwendet. Zudem wird ein €-Symbol
#' der Zahl vorangestellt. Wenn ein Character-Wert uebergeben wird, wird lediglich
#' ein €-Symbol vorangestellt. Ansonsten wird der Character-String nicht weiter
#' formatiert.
#'
#' @param x Numeric, Zahlenwert, der in Euro-Zahlenformat umgewandelt werden soll
#' @param digits Numeric, Standard = 0; Anzahl der Nachkommastellen
#' @param g_check Boolean, Standardwert = TRUE; uebergebene NAs werden in 'n.a.'
#'     umformatiert. Wenn FALSE, werden NAs durch '' angegeben.
#' @return Character, in Euro-Zahlenformat umgewandelter Inputwert
#' @export
#'
euro_format <- function(x,digits=0,g_check=T){
    if (g_check){
        ifelse(is.na(x),"n.a.",
               paste0("\u20ac ",formatC(x,format="f",big.mark=".",decimal.mark=",",
                                        digits=digits)))
    } else {
        ifelse(is.na(x),"",
               paste0("\u20ac ",formatC(x,format="f",big.mark=".",decimal.mark=",",
                                        digits=digits)))
    }
    
}