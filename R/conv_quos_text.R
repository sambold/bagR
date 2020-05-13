#' conv_quos_text: Umwandlung von Quotations
#' 
#' wandelt unterschiedliche Arten von Quotations in einen String um. Umgewandelt
#' werden sowohl quos, ellipsis, normale quo und quo, denen ein Listelement
#' uebergeben wird.
#'
#' @param qs mit ... uebergebene quos bzw. rlang::quos
#' @return 
#' @examples
#' conv_quos_text(rlang::quos(x,y,z))
#' conv_quos_text(rlang::quo(x))
#' conv_quos_text(rlang::quo(list(x,y,z)))
#' # Achtung: Operatoren werden nicht in String uebernommen
#' conv_quos_text(rlang::quo(x <- 1))
#' @import rlang
#' @import dplyr
#' @export
#'
conv_quos_text <- function(qs) {
    quo_len <- length(rlang::get_expr(qs))
    quo_start <- dplyr::case_when(class(qs)[1]=="quosures" ~ 1,
                                  class(qs)[1]=="quosure" ~ 2,
                                  TRUE ~ 0)
    
    if (quo_start==0){
        warning("Achtung, kein Quosure als Input")
        res <- NULL
    } else {
        if (quo_len==1){
            res <- rlang::as_label(qs)
        } else {
            res <- unlist(lapply(quo_start:quo_len, function(i) {
                rlang::as_label(rlang::get_expr(qs)[[i]])
            }))
        }
    }
    return(res)
}

