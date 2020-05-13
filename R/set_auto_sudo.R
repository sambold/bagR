#' set_auto_sudo:
#' 
#' @description legt fest, ob Sudo-Eingabe automatisch erfolgen soll und 
#'     hinterlegt ggf. ein Passwort
#' @param auto_sudo Boolean, Standardwert TRUE; gibt an, ob Sudo-Eingabe
#'     automatisch erfolgen soll ("-kS") oder manuell ("").
#' @param sudo_pswd String, wenn Sudo-Eingabe automatisch erfolgen soll, wird
#'     nach einem Passwort gefragt, das dann hinterlegt wird. 
#' @return Liste mit Sudo-Zusatz fuer Terminal und Passwort
#' @export
#' 
set_auto_sudo <- function(auto_sudo=TRUE,
                          use_vault=TRUE,
                          secret="sambold_linux"){
  if (auto_sudo){
    opt <- "-kS"
    if (use_vault){
      if (is.null(secret)){
        secret <- rstudioapi::askForPassword(prompt="Bitte Secret-Name fuer Passwort eingeben ...")
      }
    } else {
      if (is.null(secret)){
        secret <- rstudioapi::askForPassword(prompt="Bitte Passwort eingeben ...")
      }
    }
  } else {
    opt <- ""
  }
  res <- list(opt=opt,
              use_vault=use_vault,
              secret=secret)
  return(res)
}