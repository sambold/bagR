#' add_secret:
#' @description Wrapper fuer secret::add_secret Speichert ein Secret im Vault
#' 
#' @param name String, Secret-Name
#' @param value String Secret-Value
#' @param users String, User fuer den/die Secret zugaenglich
#' @param vault String, Pfad zum Vaul
#' @export
#'
add_secret <- function(name,
                       value=rstudioapi::askForPassword(),
                       users="sambold",
                       vault="~/.vault"){
    secret::add_secret(name=name,value=value,users=users,vault=vault)
}