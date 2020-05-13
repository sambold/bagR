#' .check_svnr: prueft SVNR auf formale Korrektheit
#'
#' prueft, ob es sich bei dem uebergebenen Wert um
#' \itemize{
#'   \item einen 10stelligen
#'   \item Zahlenwert handelt, der
#'   \item nicht mit einer 0 beginnt,
#'   \item dessen Angaben zum Geburtsdatum innerhalb zulaessiger Datumsgrenzen liegt und
#'   \item dessen 4. Stelle mit der berechneten Pruefziffer uebereinstimmt
#' }
#' Neben dem Ergebnis der Pruefung kann auch eine Beschreibung des Pruefergebnisses
#' als 'message' zurueckgegeben werden.
#'
#' @param svnr String-/Integerwert, der die SVNR enthaelt
#' @return tibble mit den Spalten:
#'     - SVNR: urspruengliche SVNR
#'     - ERR_CODE: String, eschreibung des Prueffehlers bzw. NA fuer korrekte SVNR
#'     - CHECK: Boolean, TRUE (korrekte SVNR)/FALSE (fehlerhafte SVNR)
#' @examples
#' .check_svnr(1234010100)
#' .check_svnr("1234010100")
#' .check_svnr(c(123,"sdfkj","1234010100"))
#' .check_svnr("test")
#' @references https://www.sozialversicherung.at/cdscontent/?portal=esvportal&contentid=10007.686020&viewmode=content
#' @import magrittr
#' @export
#'
.check_svnr <- function(svnr){
    # Hilfsfunktionen
    check_date <- function(x){
        if (length(x)==10){
            d <- x[5]*10+x[6]
            m <- x[7]*10+x[8]
            # SVNR nicht innerhalb zulaessiger Datumsgrenzen
            res <- (d<1 | d>31 | m<1 | m>15)
            return(!res)
        } else {
            return(FALSE)
        }

    }
    calc_pziffer <- function(x){
        if (length(x)==10){
            svnr.w <- c(3,7,9,0,5,8,4,2,1,6)
            pziffer <- sum(x*svnr.w) %% 11
            res <- pziffer
            return(res)
        } else {
            return(10)
        }
    }

    # Standardwert fuer Rueckgabe
    res <- as.list(svnr) %>%
        lapply(function(f){
            # SVNR aufbereiten
            svnr.split <- f %>%
                as.character() %>%
                strsplit("") %>%
                unlist()
            svnr.split <- suppressWarnings(as.numeric(svnr.split))
            out <- f %>%
                dplyr::tibble(SVNR=.) %>%
                dplyr::mutate(
                    ERR_CODE=dplyr::case_when(
                        sum(is.na(svnr.split))!=0 ~ "Fehler: SVNR nicht numerisch",
                        length(svnr.split)!=10 ~ "Fehler: SVNR hat nicht 10 Stellen",
                        svnr.split[1]==0 ~ "Fehler: SVNR beginnt mit 0",
                        check_date(svnr.split)==FALSE ~ "Fehler: Geburtsdatum nicht innerhalb zulaessiger Datumsgrenzen",
                        calc_pziffer(svnr.split)==10 ~ "Fehler: Pruefziffer ergibt '10', kein zulaessiger Wert",
                        (calc_pziffer(svnr.split)!=svnr.split[4]) ~ "Fehler: Pruefziffer stimmt nicht mit berechneter Pruefziffer ueberein")) %>%
                dplyr::mutate(CHECK=is.na(ERR_CODE))
            return(out)
        }) %>%
        do.call(dplyr::bind_rows,.)
    return(res)
}
