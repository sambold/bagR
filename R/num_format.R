#' num_format: europaeisches Zahlenformat
#'
#' wandelt Zahl in europaeisches Zahlenformat um: Tausendertrennzeichen wird als
#' Punkt, Komma als Beistrich dargestellt. Zudem wird ein Euro-Symbol vorangestellt.
#' Wird ein numerischer Wert uebergeben, kommt es zu diesen Umformatierungen. Wird
#' ein String uebergeben, wird lediglich ein Euro-Symbol vorangestellt. Der 
#' String-Wert bleibt dabei unformatiert.
#'
#' @param x Numeric, Wert, der in europaeisches Zahlenformat umformatiert werden soll
#' @param digits Numeric, Standardwert = 0, Anzahl der anzuzeigenden Nachkommastellen
#' @param g_check Boolean, Standardwert = TRUE, gibt an, ob uebergebene NAs als
#'     'n.a.' (TRUE) oder als '' (FALSE) dargestellt werden sollen
#' @return String, in europaeisches Format umgeformter Input
#' @export
#'
num_format <- function(x,digits=0,g_check=T){
    if (g_check){
        ifelse(is.na(x),"n.a.",
               formatC(x,format="f",big.mark=".",decimal.mark=",",digits=digits))
    } else {
        formatC(x,format="f",big.mark=".",decimal.mark=",",digits=digits)
    }
    
}