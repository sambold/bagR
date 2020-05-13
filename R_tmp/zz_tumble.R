#' .tumble: reiht Stellen eines Inputs zufaellig neu
#'
#' Die Ziffern einer Zahl oder die Elemente eines Strings werden zufaellig neu
#' gereiht. Dabei koennen einzelne Stellen konstant gehalten werden. Das
#' Ergebnis der Neureihung kann auch der Reihenfolge des Inputs entsprechen. Je
#' nachdem, ob als Input eine Zahl/ein String angegeben wird, wird auch eine
#' Zahl/ein String zurueckgegeben.
#'
#' @param x Integer/String, Wert dessen Elemente neu geordnet werden
#' @param keep Integer, Standardwert=NULL; Vektor mit jenen Positionen von x,
#'     die unveraendert bleiben sollen.
#' @return Integer/String
#' @examples
#' .tumble(x=12345)
#' .tumble(x=12345,keep=1:3)
#' @import magrittr
#' @export
#'
.tumble <- function(x,keep=NULL){
  res <- lapply(x, function(f){
    was_char <- is.character(f)
    base <- f %>%
      as.character() %>%
      strsplit(split="") %>%
      unlist()
    if (is.null(keep)){
      tumble <- base %>%
        sample(size=length(.))
    } else {
      tumble_idx <- seq(1,length(base))[-keep]
      tumble <- base[tumble_idx] %>%
        sample(size=length(.))
      dmmy <- base
      dmmy[tumble_idx] <- tumble
      tumble <- dmmy
    }
    if (was_char){
      tumble <- tumble %>%
        paste(collapse="")
    } else {
      tumble <- tumble %>%
        paste(collapse="") %>%
        as.numeric()
    }
    return(tumble)
  }) %>%
    unlist()
  return(res)
}
