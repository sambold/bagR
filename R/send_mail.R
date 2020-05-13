#' send_mail
#' 
#' Versendet ein eMail mit/ohne Anhang an eine\*einen oder mehrere Empfänger\*innen.
#' 
#' @param from String, Sender\*in-eMail
#' @param to String-Vektor, eine oder mehrere Empfänger\*innen-Adressen. Wenn
#'   NULL, wird die from-Adresse übernommen.
#' @param body String, Text des eMails
#' @param subject String, Betreff des eMails
#' @param files String-Vektor, Pfad für die zu versendenden Anhänge
#' @param user String, Username für den SMTP-Login. Wenn NULL, wird die 
#'   from-Adresse übernommen.
#' @param pswd String, Passwort für den SMTP-Login
#' @param host String, Hostadresse für die SMTP-Verbindung
#' @param port Integer, Port für die SMTP-Verbindung
#' @return versendet ein eMail
#' @import stringr
#' @import rJython
#' @export
#' 
send_mail <- function(from="samboldsky@gmail.com",
                      to=NULL,
                      body="",
                      subject="",
                      files=NULL,
                      user=NULL,
                      pswd=bagR::get_secret("sambold_google"),
                      host="smtp.gmail.com",
                      port=587){
    
    # auf gueltige eMails pruefen
    regex.filter <- "^[[:alnum:].-_]+@[[:alnum:].-]+$"
    valid.from <- sum(is.na(stringr::str_match(from,regex.filter)))==0
    stopifnot(valid.from)
    if (is.null(to)) to <- from
    valid.to <- sum(is.na(stringr::str_match(to,regex.filter)))==0
    stopifnot(valid.to)
    if (is.null(user)) user <- from
    # Empfaenger in richtiges Format bringen
    if (length(to)==1){
        to <- paste0(to,";")
    } else {
        to <- paste(to,collapse=";")
    }
    # Mail mit/ohne Anhang
    if (!is.null(files)){
        files <- paste0("'",files,"'",collapse=",")
        mail<-sprintf(paste(
            # Email-Settings
            "fromadr = '%s'",
            "toadr='%s'",
            "txt='%s'",
            "subject='%s'",
            #"body=MIMEMultipart(txt)", mit txt in Klammer wurde Text als Blank gesendet, wenn er ein Komma enthielt
            "body=MIMEMultipart()",
            "body['From']=email.utils.formataddr(('', fromadr))", 
            "body['To']=email.utils.formataddr(('', toadr))", 
            "body['Subject']=subject",
            # SMTP Server 
            "username='%s'",
            "password='%s'",
            # Attach File
            "files = [%s]",
            "body.attach(MIMEText(txt,'html'))",
            "for f in files:",
            "    head,tail = os.path.split(f)",
            "    part = MIMEBase('application', 'octet-stream')",
            "    part.set_payload( open(f, 'rb').read() )",
            "    Encoders.encode_base64(part)",
            "    part.add_header('Content-Disposition', 'attachment', filename=tail)",
            "    body.attach(part)",
            #Set SMTP server and send email, e.g., google mail SMTP server 
            "server = smtplib.SMTP('%s:%d')", 
            "server.ehlo()", 
            "server.starttls()", 
            "server.ehlo()", 
            "server.login(username,password)", 
            "server.sendmail(fromadr, toadr.split(';'), body.as_string())", 
            "server.quit()",
            sep="@E@"
        ),
        from,
        to,
        body,
        subject,
        user,
        pswd,
        files,
        host,
        port) 
    } else {
        mail<-sprintf(paste(
            # Email-Settings
            "fromadr = '%s'",
            "toadr='%s'",
            "txt='%s'",
            "subject='%s'",
            #"body=MIMEMultipart(txt)", mit txt in Klammer wurde Text als Blank gesendet, wenn er ein Komma enthielt
            "body=MIMEMultipart()",
            "body['From']=email.utils.formataddr(('', fromadr))", 
            "body['To']=email.utils.formataddr(('', toadr))", 
            "body['Subject']=subject",
            "body.attach(MIMEText(txt,'html'))",
            # SMTP Server 
            "username='%s'",
            "password='%s'",
            #Set SMTP server and send email, e.g., google mail SMTP server 
            "server = smtplib.SMTP('%s:%d')", 
            "server.ehlo()", 
            "server.starttls()", 
            "server.ehlo()", 
            "server.login(username,password)", 
            "server.sendmail(fromadr, toadr.split(';'), body.as_string())",
            "server.quit()",
            sep="@E@"
        ),
        from,
        to,
        body,
        subject,
        user,
        pswd,
        host,
        port)
    }
    # Jython-Code für Weiterverarbeitung splitten
    mail <- unlist(strsplit(mail,split="@E@"))
    # Jython ausführen/Email versenden
    rJython <- rJython::rJython()
    rJython$exec("import smtplib" ) 
    rJython$exec("from email.MIMEMultipart import MIMEMultipart")
    rJython$exec("from email.MIMEBase import MIMEBase")
    rJython$exec("from email.MIMEText import MIMEText")
    rJython$exec("import email.utils") 
    rJython$exec("import smtplib")
    rJython$exec("import os")
    rJython$exec("from email.Utils import COMMASPACE, formatdate")
    rJython$exec("from email import Encoders")
    rJython::jython.exec(rJython,mail)
}