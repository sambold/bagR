#' check_ftp: Daten von FTP-Server abrufen
#'
#' @description Ruft ausgewählte Dateien von einem FTP-Server ab und speichert sie in einem
#' Zielordner. Die Auswahl der zu downloadenden Dateien kann dabei 
#' \itemize {
#'   \item anhand eines Abgleichs mit einem Referenzverzeichnis erfolgen. Hier 
#'     werden nur jene Dateien abgezogen, die nicht schon im Referenzverzeichnis
#'     vorhanden sind (der Abgleich erfolgt über den Dateinamen)
#'   \item anhand des Zeitpunkts definiert werden. Hier werden alle Dateien 
#'     abgezogen, die nach einem bestimmten Zeitpunkt am FTP-Server erstellt wurden
#'   \item anhand eines Regex-Filters definiert werden. Hier werden alle Dateien
#'     abgezogen, deren Dateiname dem festgelegten Regex-Muster entsprechen.
#'  }
#'  Aus der Liste zu downloadender Dateien, die nach Angabe einer der oben 
#'  beschriebenen Filteroptionen identifiziert wurde, können optional auch manuell
#'  jene Dateien ausgewählt werden, die abgezogen werden sollen. Ebenso können 
#'  Dateien durch einen Regex-Filter vom Download ausgeschlossen werden.
#'  
#'  Sofern bei einem FTP-Aufruf keine neuen Dateien gefunden wurden, besteht die
#'  Möglichkeit den Aufruf in bestimmten Abständen für eine bestimmte Anzahl zu 
#'  wiederholen.
#'  
#' @param server String, erforderlich, FTP-Server (z.B. 193.58.211.75)
#' @param fpath.ftp String, zu durchsuchender Ordner auf dem FTP-Server ohne
#'   Begrenzung durch "/" (z.B. daapfl)
#' @param out.dir String, Ausgabepfad für Downloads (Standard: paste0(getwd(),"/"))
#' @param typ String, Angabe wie die Auswahl der Download-Files erfolgen soll: 
#'   über ein Referenzverzeichnis (dir), anhand eines Zeitpunktes (intime) oder 
#'   durch einen Regex-Filter (regex). Mögliche Ausprägungen: dir, intime, regex
#' @param select Boolean, gibt an, ob Download-Files aus der Liste potentieller 
#'   Downlaods (eingeschränkt durch die Auswahl über typ) zusätzlich manuell 
#'   selektiert werden können sollen (Standard: FALSE)
#' @param cmp.dir String, nur relevant, wenn  typ=dir, Angabe eines 
#'   Referenzverzeichnisses das zur Auswhal der Download-Files dient
#' @param cmp.filter String, nur relevant, wenn  typ=dir,
#'   Regex-Filter mit dem Liste der Files im Referenzverzeichnis
#'   eingeschränkt werden kann (Standard: ".*\\.csv" -> der Regex-Filter wird so
#'   angewandt, dass immer nur Dateibezeichnungen aber nicht evtl. Pfade zu 
#'   Unterordnern berücksichtigt werden. Dh. der oben angeführte Filter wird in 
#'   der Funktion so ergänzt, dass 'home/data/test.csv' auf 'test.csv' reduziert 
#'   wird. Beim Abgleich mit dem FTP-Server wird dann die Datei 'test.csv' vom 
#'   Download ausgeschlossen. Unabhängig davon in welchem Ordner sie sich dort
#'   befindet.)
#' @param regex String, nur relevant, wenn typ=regex, Regex-Filter mit dem die 
#'   zu downloadenden Files am FTP-Server eingeschränkt werden können
#' @param lbound Integer, nur relevant, wenn typ=intime, Anzahl der Sekunden, die 
#'   höchstens vergangen sein dürfen, seit ein File am FTP-Server erstellt wurde.
#'   Dh. für einen Download muss das Erstelldatum der Datei am FTP-Server 
#'   > (Sys.time()-lbound) sein. (Standard: 86400; Erstelldatum max. vor 24h)
#' @param blacklist String, Regex-Filter, Dateinamen am FTP-Server, die diesem 
#'   Muster entsprechen, werden vom Download ausgeschlossen (Standard: NA -> kein blacklist-Filter)
#' @param retry Boolean, gibt an, ob automatisch mehrere Verbindungen nacheinander
#'   zum FTP-Server aufgebaut werden sollen, bis eine zu downloadende Datei gefunden
#'   wird bzw. bis die Anzahl maximaler Wiederholungen erreicht wurde (Standard: FALSE)
#' @param sleep Integer, nur relevant, wenn retry=TRUE, gibt an wie viele Sekunden
#'   zwischen den Verbindungsversuchen gewartet werden soll (Standard: 3600)
#' @param max.retry Integer, nur relevant, wenn retry=TRUE, gibt an wie oft 
#'   ein erneuter Verbindungsversuch zum FTP-Server durchgeführt werden soll
#'   (Standard: 24)
#' @param verbose Boolean, gibt an, ob ausführlichere Informationen zum 
#'   Verbindungsaufbau ausgegeben werden sollen (Standard: FALSE)
#' @import RCurl
#' @import stringr
#' @import magrittr
#' @return kein Rückgabewert
#' @export
#' 
check_ftp <- function(server="",
                      fpath.ftp="",
                      out.dir=paste0(getwd(),"/"),
                      typ="dir",
                      select=F,
                      cmp.dir=getwd(),
                      cmp.filter=".*\\.csv",
                      regex="",
                      lbound=86400,
                      blacklist=NA,
                      retry=F,
                      sleep=3600,
                      max.retry=24,
                      verbose=F){
    lbound <- Sys.time()-lbound
    # Falls die Datei in einem Unterverzeichnis liegt, wird die Ordnerstruktur 
    # abgeschnitten und nur der Dateiname (z.b. .*\\.csv) bleibt erhalten.
    # Für den Prozessfluss ist es notwendig, dass es immer zumindest eine Capturing-
    # und eine Non-Capturing-Group gibt
    cmp.filter <- paste0("(?:^.*/)?(",cmp.filter,")")
    i <- 1
    if (!retry) sleep=0
    while ((retry|i==1)&(i<max.retry)){
        if (retry) cat("## [",i,"/",max.retry,"] Durchsuche Server ... nächster Versuch in ",sleep," Sekunden ... \n")
        # Downloadlink erzeugen
        url <- paste0("ftp://", server,"/",fpath.ftp,"/")
        # Prüfen, ob bereits die notwendigen Credentials für die FTP-Verbindung
        # im Tresor hinterlegt sind. Falls nicht, um Eingabe fragen
        tryCatch(ifelse(length(apfl::getSecret(secretname=server))>0,NA,NA),
                 error=function(e){
                     message("## Im Passwort-Tresor sind noch keine Daten zu diesem FTP-Server vorhanden. Es wird nun ein Datensatz angelegt ...\n",
                             " # Server: ",server,"\n",
                             " # Bitte Username und Passwort eingeben ...\n")
                     apfl::addSecret(secretname=server)},
                 finally={NA})
        # Liste der FTP-Dateien
        curl<-RCurl::getCurlHandle()
        RCurl::curlSetOpt(.opts=list(forbid.reuse=1),curl=curl)
        ftp.files <- RCurl::getURL(url=url,
                                   userpwd=paste(apfl::getSecret(secretname=server)$user,
                                                 apfl::getSecret(secretname=server)$password,
                                                 sep=":"),
                                   dirlistonly=T,
                                   ftp.use.epsv=F,
                                   verbose=verbose,
                                   curl=curl) %>%
            stringr::str_split("\n|\r") %>%
            unlist() %>%
            .[.!=""]
        rm(curl)
        gc()
        # in blacklist angeführte Files/Regex ausschließen
        if (!is.na(blacklist)){
            ftp.files <- ftp.files[!grepl(blacklist,ftp.files,perl=T)]
        }
        # Abgleich dir/intime
        if (typ=="dir"){
            # Filter: Referenzverzeichnis
            # Liste der Dateien im Referenzverzeichnis
            cmp.files <- list.files(cmp.dir,recursive=T) 
            if (length(cmp.files)>0){
                cmp.files <- cmp.files %>% 
                    stringr::str_match(pattern=cmp.filter) %>%
                    .[,2]
            }
            filter <- !ftp.files %in% cmp.files
        } else {
            if (typ=="regex"){
                filter <- grepl(regex,ftp.files,perl=T)
            } else {
                # Filter: Intime
                curl<-RCurl::getCurlHandle()
                RCurl::curlSetOpt(.opts=list(forbid.reuse=1),curl=curl)
                regex.filter <- paste0("(\\w* *\\d* \\d*:\\d*)(?: *",ftp.files,")")
                filter <- RCurl::getURL(url=url,
                                        userpwd=paste(apfl::getSecret(secretname=server)$user,
                                                      apfl::getSecret(secretname=server)$password,
                                                      sep=":"),
                                        dirlistonly=F,
                                        ftp.use.epsv=T,
                                        verbose=verbose,
                                        curl=curl) %>%
                    stringr::str_split("\n") %>%
                    unlist() %>%
                    .[.!=""] %>%
                    stringr::str_match(regex.filter) %>%
                    .[,2] %>%
                    gsub("  "," ",.) %>%
                    gsub("Jan","Jän",.) %>%
                    gsub("Mar","Mär",.) %>%
                    gsub("May","Mai",.) %>%
                    gsub("Oct","Okt",.) %>%
                    gsub("Dec","Dez",.) %>%
                    strptime("%b %d %H:%M") %>%
                    magrittr::is_greater_than(lbound)
                rm(curl)
                gc()
            }
        }
        # Download Files
        dl.list <- ftp.files[filter]
        if (length(dl.list)>0){
            if (select){
                # Auswahl der Download-Files, beistrichgetrennt
                cat("## Neue Files \n")
                print(paste(1:length(dl.list),dl.list,sep=" - "))
                ui <- readline("## Files auswählen (z.B.: 1,2) \n")
                ui <- strsplit(ui,",") %>%
                    unlist() %>%
                    as.numeric()
                dl.list <- dl.list[ui]
            } 
            for (file in dl.list){
                cat("## Starte Download:",file,"...\n")
                furl <- paste0(url,file)
                fout <- paste0(out.dir,file)
                curl<-RCurl::getCurlHandle()
                RCurl::curlSetOpt(.opts=list(forbid.reuse=1),curl=curl)
                dat.bin <- RCurl::getBinaryURL(furl,
                                               userpwd=paste(apfl::getSecret(secretname=server)$user,
                                                             apfl::getSecret(secretname=server)$password,
                                                             sep=":"),
                                               ftp.use.epsv=F,
                                               curl=curl)
                rm(curl)
                gc()
                writeBin(dat.bin, fout)
                retry <- F
            }
            i <- i + 1
        } else {
            i <- i + 1
        }
        #closeAllConnections()
        Sys.sleep(sleep)
    }
}






