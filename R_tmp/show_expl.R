#' show_expl: erzeugt Beispieldatensaetze fuer die einzelnen Plot-Vorlagen
#'
#' @param typ String, Standard="pyramide"; gibt an, fuer welche Plot-Vorlage
#'     ein Beispieldatensatz ausgegeben werden soll. Moegliche Werte sind:
#'     \itemize{
#'         \item pyramide
#'         \item circular
#'     }
#' @return data.frame
#' @examples
#' show_expl("pyramide")
#' @export
#'
show_expl <- function(typ="pyramide"){
    switch(typ,
           "pyramide" = res <- get(load("data/pop_ges_long.RData")),
           "circular" = res <- get(load("data/bildung.RData")))
    return(res)
}

