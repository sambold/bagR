#' crt_vault
#' @description Wrapper fuer secret::create_vault. Legt einen Vault an
#' 
#' @param vault_path String, Pfad zum Vault
#' @export
crt_vault <- function(vault_path="~/.vault"){
    if (!file.exists(vault_path)) {
        res <- dir.create(vault_path)
        if (!res) warning("Vault Pfad konnte nicht angelegt werden")
    }
    secret::create_vault(vault_path)
}