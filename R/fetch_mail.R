#' fetch_mail: ruft eMail ab
#'
#' ruft eMail ueber rJython ab
#'
#' @param host Character, Standard = imap.gmail.com; Angabe zum verwendeten Host
#' @param port Numeric, Standard = 993; Angabe zum verwendeten Port
#' @param user Character, Standard = samboldsky@gmail.com; User-eMail
#' @param pswd Character, Standard bagR::get_secret("google"); User-Passwort
#' @param search.from Character, Standard NULL; wenn NULL, werden alle Absender
#'     abgerufen - ansonsten auf bestimmte Absender einschraenkbar
#' @param search.subj Character, Standard NULL; wenn NULL, werden alle Betreffe
#'     abgerufen - ansonsten auf bestimmten Betreff einschraenkbar
#' @param search.body Character, Standard NULL; wenn NULL, werden alle Inhalte
#'     abgerufen - ansonsten auf bestimmte Inhalte einschraenkbar
#' @param wait Numeric, Standard = 1, aktuell nicht verwendet (vermutlich wars 
#'     fuer mehrfaches Abrufen gedacht bzw. als Pause dazwischen)
#' @param max.msg Numeric, Standard = NULL, wenn NULL werden alle Nachrichten
#'     abgerufen. Ansonsten einschraenkbar auf die n aktuellsten Nachrichten.
#' @param java.path Character, Pfad zu Java (falls nicht automatisch erkannt wird)
#' @import rJython
#' @import dplyr
#' @return ...
#' @export
#'
fetch_mail <- function(host = "imap.gmail.com",
                       port = 993,
                       user = "samboldsky@gmail.com",
                       pswd = bagR::get_secret("google"),
                       search.from = NULL,
                       search.subj = NULL,
                       search.body = NULL,
                       wait=1,
                       max.msg=NULL,
                       java.path="/usr/lib/jvm/java-8-openjdk-amd64/"){
    
    # auf JAVA_HOME pruefen
    if (Sys.getenv("JAVA_HOME")=="") Sys.setenv(JAVA_HOME=java.path)
    #library(rJython)
    rJython <- rJython::rJython()
    
    # offen: max Anzahl von dl-Mails bestimmen, body/anhang downloaden
    code <- sprintf(paste(
        "import time",
        "from itertools import chain",
        "import email",
        "import imaplib",
        "imap_ssl_host = '%s'",
        "imap_ssl_port = %i",
        "username = '%s'",
        "password = '%s'",
        "uid_max=0",
        sep="@E@"),
        host,
        port,
        user,
        pswd)
    code <- unlist(strsplit(code,split="@E@"))
    rJython::jython.exec(rJython,code)
    
    # Restrict mail search. Be very specific.
    if (!is.null(c(search.from,search.subj,search.body))){
        code <- "criteria = {"
        if (!is.null(search.from)) code <- paste0(code,"'FROM': '",search.from,"',")
        if (!is.null(search.subj)) code <- paste0(code,"'SUBJECT': '",search.subj,"',")
        if (!is.null(search.body)) code <- paste0(code,"'BODY': '",search.body,"',")
        code <- paste0(substr(code,1,nchar(code)-1),"}")
        rJython::jython.exec(rJython,code)
        code <- paste(
            "c = list(map(lambda t: (t[0], '\"'+str(t[1])+'\"'),criteria.items())) + [('UID', '%d:*' % (uid_max+1))]",
            "search_string='(%s)' % ' '.join(chain(*c))",
            sep="@E@")
        code <- unlist(strsplit(code,split="@E@"))
        rJython::jython.exec(rJython,code)
    } else {
        rJython::jython.exec(rJython,"search_string = '(UID 1:*)'")
    }
    
    code <- paste(
        # "server = imaplib.IMAP4_SSL(imap_ssl_host, imap_ssl_port)",
        "server = imaplib.IMAP4_SSL('imap.gmail.com', 993)",
        "server.login(username, password)",
        "server.select('INBOX')",
        "result, data = server.uid('search', None, search_string)",
        "uids = [int(s) for s in data[0].split()]",
        "server.logout()",
        sep="@E@")
    code <- unlist(strsplit(code,split="@E@"))
    rJython::jython.exec(rJython,code)
    
    # Fuer jede Nachricht ein-/ausloggen
    uids <- jython.get(rJython,"uids")
    if (!is.null(max.msg)) uids <- uids[order(uids, decreasing = TRUE)][1:max.msg]
    uid_max <- jython.get(rJython,"uid_max")
    if (length(uids)>0){
        mail.df <- dplyr::data_frame()
        for (uid in uids){
            if (uid > uid_max){
                cat("## UID",uid,"einlesen ... \n")
                code <- sprintf(paste(
                    "from email.utils import parseaddr",
                    "server = imaplib.IMAP4_SSL(imap_ssl_host, imap_ssl_port)",
                    "server.login(username, password)",
                    "server.select('INBOX')",
                    "result, data = server.uid('search', None, search_string)",
                    "uids = [int(s) for s in data[0].split()]",
                    "uid=%d",
                    "result, data = server.uid('fetch', uid, '(RFC822)')",
                    "msg = email.message_from_string(data[0][1])",
                    "from_label, fromadr=parseaddr(msg['from'])",
                    "to_label,toadr = parseaddr(msg['to'])",
                    "subj = msg['subject']",
                    "date = msg['date']",
                    "uid_max = uid",
                    "body=''",
                    "server.logout()",
                    sep="@E@"),
                    uid)
                code <- unlist(strsplit(code,split="@E@"))
                rJython::jython.exec(rJython,code)

                # Test
                if (rJython::jython.get(rJython,"msg.is_multipart()")){
                    code <- sprintf(paste(
                        "for part in msg.walk(): ctype=part.get_content_type()",
                        "cdispo = str(part.get('Content-Disposition'))",
                        sep="@E@"))
                    code <- unlist(strsplit(code,split="@E@"))
                    rJython::jython.exec(rJython,code)
                    ctype <-  rJython::jython.get(rJython,"ctype")
                    cdispo <-  rJython::jython.get(rJython,"cdispo")
                    rJython::jython.exec(rJython,"charset = part.get_content_charset()")
                    charset <- rJython::jython.get(rJython,"charset")
                    if (ctype %in% c("text/plain","text/html")){
                        if (charset == "None"){
                            rJython::jython.exec(rJython,"body = part.get_payload(decode=True)")
                            #body <- jython.get(rJython,"body")
                        } else {
                            if (ctype=="text/plain"){
                                rJython::jython.exec(rJython,"body = unicode(part.get_payload(decode=True), str(charset), 'ignore').encode('utf8','replace')")
                                #body <- jython.get(rJython,"body")
                            }
                            if (ctype=="text/html"){
                                rJython::jython.exec(rJython,"body = unicode(part.get_payload(decode=True), str(charset), 'ignore').encode('utf8','replace')")
                                #body <- jython.get(rJython,"body")
                            }
                        }
                        # if (ctype=="text/html" & !"attachment" %in% cdispo){
                        #     jython.exec(rJython,"body = part.get_payload(decode=True)")
                        #     body <- jython.get(rJython,"body")
                        # } else {
                        #     jython.exec(rJython,"body = msg.get_payload(decode=True)")
                        #     body <- jython.get(rJython,"body")
                        # }
                    }
                } else {
                    rJython::jython.exec(rJython,"unicode(msg.get_payload(decode=True), msg.get_content_charset(), 'ignore').encode('utf8', 'replace')") 
                }
                rJython::jython.exec(rJython,"body=body.strip()")
                body <- rJython::jython.get(rJython, "body")
                
                mail.df <- mail.df %>%
                    dplyr::bind_rows(dplyr::data_frame(uid=uid,
                                                       subject=rJython::jython.get(rJython,"subj"),
                                                       date=rJython::jython.get(rJython,"date"),
                                                       from=rJython::jython.get(rJython,"fromadr"),
                                                       from_label=rJython::jython.get(rJython,"from_label"),
                                                       to=rJython::jython.get(rJython,"toadr"),
                                                       to_label=rJython::jython.get(rJython,"to_label"),
                                                       body=body))
     
            }
        }
    }
    return(mail.df)
}

