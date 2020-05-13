#' clear_dev: schliesst geoeffnete Ausgabe-Devices fuer Grafiken
#'
#' @export
#'
clear_dev <- function(){
    # Falls zu viele Ausgabe-Devices offen sind, werden alle Devices geschlossen
    l <- length(dev.list())
    if (l>0){
        for (i in 1:l){
            dev.off()
        }
    }
}