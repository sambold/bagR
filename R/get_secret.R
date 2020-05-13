#' get_secret:
#' @description Wrapper fuer secret::get_secret. Holt ein Secret aus dem Vault
#' 
#' @param name String, Secret-Name
#' @param key String, Pfad zum RSA-Key
#' @param vault String, Pfad zum Vault
#' @return secret
#' @export
#'
get_secret <- function(name,
                       key="~/.ssh/id_rsa",
                       vault="~/.vault"){
    secret::get_secret(name=name,key=key,vault=vault)
}