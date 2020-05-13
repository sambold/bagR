#' calc_age: berechnet Alter
#'
#' berechnet Alter bzw. verstrichene Zeit in Sekunden, Minuten, Stunden, Wochen 
#' oder in Monaten zwischen zwei Datumswerten. 
#'
#' @param von String, der in POSIXlt umgewandelt werden kann bzw. Date
#' @param bis String, der in POSIXlt umgewandelt werden kann bzw. Date,
#'     Standardwert = Sys.Date(); 
#' @param unit String, Standardwert = age; gibt an, in welcher Einheit die
#'     Differenz angegeben werden soll. 
#'     Zur Auswahl stehen secs, mins, hours, weeks, months, age
#' @return Alter
#' @examples
#' calc_age(von="2004-05-12",bis=Sys.time())
#' @export
#'
calc_age <- function(von,
                     bis=Sys.Date(),
                     unit="age"){
  von <- as.POSIXlt(von)
  bis <- as.POSIXlt(bis)
  unit_list <- c("age","secs","mins","hours","weeks","months")
  if (unit=="age"){
    if ((bis$mon < von$mon)|((bis$mon==von$mon) & bis$mday < von$mday)){
      res <- bis$year-von$year-1
    } else {
      res <- bis$year-von$year
    }
  } else if (unit %in% c("secs","mins","hours","weeks")){
    res <- difftime(bis,von,units=unit)
  } else if (unit=="months"){
    von_mon <- von$year*12+von$mon
    bis_mon <- bis$year*12+bis$mon
    res <- bis_mon-von_mon
  } else {
    warning(paste0("Gewaehlte 'unit' muss einen der folgenden Werte enthalten:\n",
                   paste0("    -",unit_list,collapse="\n")))
  }
  return(res)
}
