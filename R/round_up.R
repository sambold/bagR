#' round_up: rundet Zahl zur naechsten Obergrenze hin auf
#' 
#' rundet eine Zahl oder einen Vektor zur naechsten Obergrenze hin auf. Als 
#' Obergrenzenschritt kann eine beliebige Ganzzahl definiert werden. Wird eine
#' Schrittlaenge von 10 angegeben, wird zum naechsten Zehner aufgerundet. Wird
#' eine Schrittlaenge von 5 angegeben, wird zum naechsten Fuenfer aufgerundet etc.
#'
#' @param x Numeric, aufzurundender Zahlenwert/-vektor
#' @param step Numeric, Standard 10; Schrittweite innerhalb der aufgerundet werde soll
#' @return aufgerundete Zahl bzw. aufgerundeter Vektor
#' @examples
#' round_up(1)
#' round_up(c(1,5,12))
#' round_up(3,step=5)
#' round_up(105,step=1000)
#' @export
#'
round_up <- function(x, step=10) {
    step*ceiling(x/step)
}
