#' .gen_svnr:
#'
#' erzeugt zu einem Geburtsdatum bzw. einer Liste von Geburtsdaten eine gültige
#' SVNR, indem die Laufnummer schrittweise so lange erhöht wird, bis die
#' Berechnung zu einer korrekten Prüfziffer führt.
#'
#' @param gebdat_list String bzw. Character Vector, der Geburtsdaten im Format
#'   "ddmmyy" enthält.
#' @param verbose Boolean, gibt an, ob zusätzliche Informationen zum
#'   Generierungsprozess ausgegeben werden sollen.
#' @return SVNR-String bzw. Character Vector mit formal gültigen SVNR. Wenn
#'   verbose=TRUE, wird das Ergebnis der SVNR-Prüfung je generierter SVNR sowie
#'   die Anzahl benötigter Iterationen je Geburtsdatum als message ausgegeben.
#' @import magrittr
#' @export
#'
.gen_svnr <- function(gebdat_list,
                     verbose=F){
  out_list <- character()
  for (geb in gebdat_list){
    # Init
    gilt <- F
    a <- 0
    b <- 0
    c <- 0
    i <- 0
    # gewichtete Ziffernsumme des Gebdats
    sub_sum <- strsplit(geb,"") %>%
      unlist() %>%
      as.numeric() %>%
      magrittr::multiply_by(c(5,8,4,2,1,6)) %>%
      sum()
    # passende Laufnummer finden
    while (!gilt){
      # Schleifenzähler
      i <- i + 1
      if (verbose) message("## Iteration",i,"\n")
      # der Reihe nach 1,2,3 Ziffer erhöhen und dann wieder bei 1. Ziffer beginnen
      dmmy <- i %% 3
      if (dmmy == 1){
        a <- a+1
      } else {
        if (dmmy == 2){
          b <- b+1
        } else {
          c <- c+1
        }
      }
      # gewichtete Ziffernsumme mit Laufnummer
      sum_neu <- sub_sum + a*3 + b*7 + c*9
      res <- sum_neu %% 11
      # Prüfen, ob es sich um eine gültige SVNR handelt
      if (!res==10){
        out <- paste0(a,b,c,res,geb)
        if (verbose) apfl::.check_svnr(out)
        gilt <- T
      }
    }
    out_list <- c(out_list,out)
  }
  return(out_list)
}
