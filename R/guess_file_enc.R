#' guess_file_enc
#' 
#' Durch Angabe eines Pfades (inkl. Dateiname) wird ein Teil eines File als 
#' Rohdaten mit 'scan' eingelesen, um zu versuchen, das verwendete Encoding 
#' zu bestimmen.
#' 
#' @param fpath String, Pfad (inkl. Dateiname)
#' @param what String, Typ dessen, was eingelesen werden soll (siehe scan)
#' @param sep String, Trennzeichen in Daten (siehe scan)
#' @param quote String, Set an Quoting-Symbolen (siehe scan)
#' @param skipNul Boolean, Nuls auslassen, wenn von Character eingelesen wird (siehe scan)
#' @param nline Integer, maximale Anzahl an Zeilen, die eingelesen werden (siehe scan)
#' @param quiet Boolean, Info zum Einleseprozess (siehe scan)
#' @param ... zusaetzliche Parameter für die Funktion scan
#' @return data.frame mit Liste möglicher Encodings und deren Wahrscheinlichkeit
#' @import rvest
#' @export
guess_file_enc <- function(fpath,
                          what="character",
                          sep=";",
                          quote="\"",
                          skipNul=T,
                          nline=10,
                          quiet=T,
                          ...){
    con <- file(description=fpath,open="r")
    f <- scan(file=con,what="character",sep=sep,quote=quote,skipNul=T,
              nline=nline,quiet=quiet,...)
    close(con)
    rvest::guess_encoding(f)
}
