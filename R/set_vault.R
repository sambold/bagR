#' set_vault
#' 
#' Wrapper für secret::create_vault, um vault-Pfad im Environment abzulegen
#' @param path String, Pfad zum Vault
#' @return Pfad zum Vault (nur bei Ausgabe in Variable)
#' @import secret
#' @export

set_vault <- function(path="~/.vault"){
    # für neuen User:
    # priv_key <- openssl::rsa_keygen(bits=2048)
    # pub_key <- as.list(priv_key)$pubkey
    # openssl::write_pem(priv_key,"id_rsa","~/.ssh")
    # secret::add_user("sambold",pub_key,vault=path)
    Sys.setenv(R_SECRET_VAULT=secret::create_vault(path=path))
}


