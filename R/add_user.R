#' add_user:
#' @description Wrapper fuer secret::add_user. Legt einen User im Vault an
#' 
#' @param user String, User-Name
#' @param pub_key String, Pfad zum Public Key
#' @param vault String, Pfad zum Vaul
#' @export
#'
add_user <- function(user="sambold",
                     pub_key="~/.ssh/id_rsa.pub",
                     vault="~/.vault"){
    secret::add_user(email=user, public_key=pub_key,vault=vault)
}