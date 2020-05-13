#' show_vinagre:
#' 
#' @description oeffnet Vinagre
#' @param vinagre_port Numeric, gibt Port an auf dem Vinagre-Instanz laeuft
#' @export
#' 
show_vinagre <- function(vinagre_port=5901){
  # show browser (password: secret)
  system(glue::glue("vinagre 127.0.0.1:{vinagre_port}"))
}