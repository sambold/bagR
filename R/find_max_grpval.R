#' find_max_grpval: gibt je Gruppe den Wert mit dem haufigsten Vorkommen zurueck
#' 
#' Bei gleich haufigem Vorkommen wird der Wert mit der niedrigsten Sortierreihenfolge
#' genommen.
#'
#' @param x Inputdaten
#' @param .key Variable, Gruppe innerhalb der ein Wert gefunden werden soll
#' @param .val Variable, Auspraegungen fuer die ein Max-Wert gesucht wird
#' @import dplyr
#' @import rlang
#' @return tibble, in dem jedem .key genau ein Wert (mit dem haufigsten Auftreten)
#'     zugeordnet ist
#' @export
#'
find_max_grpval <- function(x,.key,.val){
    .key <- rlang::enquo(.key)
    .val <- rlang::enquo(.val)
    res <- x %>%
        dplyr::count(!!.key, !!.val) %>%
        dplyr::group_by(!!.key) %>%
        dplyr::arrange(dplyr::desc(n),!!.key) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n)
    # Output
    return(res)
}
