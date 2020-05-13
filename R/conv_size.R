#' conv_size: konvertiert Fontsize von Theme in size-Einheit
#'
#' oder so aehnlich ... was genau wie und wo ist mir noch nicht ganz klar. Aber
#' damit sollte die Schriftgroesse definiert durch theme und die Schriftgroesse
#' definiert ueber z.b. geom_text gleich/vergleichbar sein
#'
#' @param x Fontsize (analog to Theme), die in size-Einheit konvertiert werden soll
#' @import ggplot2
#' @return konvertierte Fontsize
#' @examples
#' conv_size(14)
#' @export
#'
conv_size <- function(x){
    x/ggplot2:::.pt
}
