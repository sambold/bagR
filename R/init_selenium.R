#' init_selenium:
#' 
#' @description Startet neuen Selenium Browser und schafft Vorbedingungen.
#' @param image String, Pfad zum Image mit dem der Browser gestartet werden soll
#' @param profile_path String, Pfad zu einem bestehenden Browserprofil
#' @param selenium_port Numeric, Standardwert 4445, Port der fuer Selenium 
#'     verwendet werden soll.
#' @param vinagre_port Numeric, Standardwert NULL, Port der fuer Vinagre 
#'     verwendet werden soll. Wenn NULL wird vinagre nicht verwendet.
#' @param opts List, Standardwert NULL; wenn NULL wird opts ueber die Funktion
#'     set_auto_sudo() befuellt. Dabei werden zwei Listelemente zugewiesen
#'     \itemize{
#'         \item opt: gibt an, ob Passwort automatisch ("-Ks") oder manuell ("") 
#'             im Terminal eingegeben werden muss
#'         \item sudo_pswd: Passwort zur Bestaetigung, wenn opt="-Ks"
#'     }
#' @return RSelenium remoteDriver
#' @export
#' 
init_selenium <- function(image="selenium/standalone-firefox-debug:latest",
                          profile_path="/home/samuel/.mozilla/firefox/testR",
                          selenium_port=4445,
                          vinagre_port=NULL,
                          opts=NULL){
  if (is.null(opts)){
    opts <- set_auto_sudo()
  }
  if (!check_for_image(opts=opts,
                       image=image)){
    # pull image ----
    system(glue::glue("sudo {opts$opt} docker pull {image}"),
           input=ifelse(opts$use_vault, bagR::get_secret(opts$secret),opts$secret))
  }
  
  # ports festlegen
  selenium_port_string <- glue::glue("-p {selenium_port}:4444")
  if (is.null(vinagre_port)){
    vinagre_port_string <- ""
  } else {
    vinagre_port_string <- glue::glue("-p {vinagre_port}:5900")
  }
  # mit/ohne bestehendem Profil
  if (is.null(profile_path)){
    system(glue::glue("sudo {opts$opt} docker run -d \\
                      {selenium_port_string} \\
                      {vinagre_port_string} \\
                      --shm-size 2g \\
                      {image}"),
           input=ifelse(opts$use_vault, bagR::get_secret(opts$secret),opts$secret))
  } else {
    system(glue::glue("sudo {opts$opt} docker run --rm \\
                      {selenium_port_string} \\
                      {vinagre_port_string} \\
                      -v {profile_path} \\
                      -e JAVA_OPTS \\
                      {image}"),
           input=ifelse(opts$use_vault, bagR::get_secret(opts$secret),opts$secret))
  }
  # Browser starten
  remDr <- RSelenium::remoteDriver(port=selenium_port)
}