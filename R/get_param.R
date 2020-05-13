#' get_param: erstellt aus den uebergebenen cronR-Argumenten eine Parametertabelle
#'#'
#' @param filter_cols String, Standard c("FREQ_CRONJOB","FNAME_CRONJOB");
#'     Variablennamen als String, die nicht in der Parametertabelle vorkommen
#'     (aber als col_label vorhanden sind). Mit dem Wert NULL werden keine Filter
#'     gesetzt
#' @param delim String, Standard @@; Delimiter durch den die Spaltennamen 
#'     getrennt sidn
#' @return Parametertabelle
#' @export
#' @import magrittr
#'
get_param <- function(filter_cols=c("FREQ_CRONJOB","FNAME_CRONJOB"),
                      delim="@@"){
    library(magrittr)
    args <- commandArgs()
    if (length(which(args=="--args"))>0){
        args <- args[(which(args=="--args")+1):length(args)]
        col_labels <- args[length(args)] %>%
            strsplit(delim) %>%
            unlist() %>%
            .[!. %in% filter_cols]
        param <- args %>% 
            matrix(nrow=1) %>% 
            dplyr::as_tibble() %>% 
            magrittr::set_colnames(col_labels)
        return(param)
    } else {
        warning("Es wurden keine Parameter uebergeben")
    }
   
}
