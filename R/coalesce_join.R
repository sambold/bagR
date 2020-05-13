#' coalesce_join: join, der Inahlte gemeinsamer Variablen miteinander vereint
#' 
#' Beim Join koennen verschiedene Join-Varianten des dplyr-packages verwendet
#' werden. Variabeln, die in beiden zu joinenden Quellen vorkommen, werden 
#' miteinander verschmolzen. Dabei wird grundsaetzlich der Wert aus x verwendet,
#' außer dieser ist missing, dann wird das missing mit dem Wert aus y verwendet.
#'
#' @param x data.frame (siehe dplyr)
#' @param y data.frame (siehe dplyr)
#' @param by character/named list, Variablen ueber die gejoint wird (siehe dplyr)
#' @param suffix charakter, Suffix fuer Variablennamen, die in beiden Datensaetzen
#'     vorkommen (siehe dplyr)
#' @param join Funktionsaufruf fuer den Join nach dem Schema
#'     dplyr::left_join, dplyr::full_join, etc.
#' @param keep Boolean, Standard FALSE; gibt an, ob die urspruenglichen Variablen,
#'     die in beiden Datensaetzen vorkommen behalten werden sollen oder ob nur
#'     die vereinten Variablen behalten werden sollen
#' @param ... weitere, zu uebergebende Argumente (siehe dplyr)
#' @import dplyr
#' @import purrr
#' @import magrittr
#' @return gejointen Datensatz
#' @export
#'
coalesce_join <- function(x,
                          y,
                          by=NULL,
                          suffix=c(".x",".y"),
                          join=dplyr::left_join,
                          keep=F,
                          ...){
    # Join
    res <- join(x,y,by=by,suffix=suffix,...)
    # Coalesce
    cols_union <- dplyr::union(colnames(x),colnames(y))
    cols_dup <- colnames(x)[!colnames(x) %in% colnames(res)]
    coalesced <- purrr::map_dfc(cols_dup, 
                                ~dplyr::coalesce(res[[paste0(.x, suffix[1])]],
                                                 res[[paste0(.x, suffix[2])]]))
    coalesced <- magrittr::set_colnames(coalesced,cols_dup)
    # Join bereinigen
    res <- dplyr::bind_cols(res,coalesced)
    if (!keep) res <- dplyr::select(res,cols_union)
    # Output
    return(res)
}
