#' .calc_overlap: Berechnet zeitliche Ueberschneidung in Datensatz
#'
#' Berechnet in einem Datensatz zeitliche Ueberschneidungen. Dabei kann entweder
#' rein ein Flag erstellt werden oder es wird zusaetzlich die Dauer der
#' Ueberschneidung ausgegeben. Zudem kann der Datensatz zur Identifikation von
#' Ueberschneidungen weiter in Gruppen unterteilt werden, innerhalb derer die
#' Berechnung dann stattfindet.
#'
#' @param df data.frame; Datensatz, der zumindest ein Spalte mit Beginn- und
#'     Endzeiten enthaelt.
#' @param grp Spaltenname, Standardwert NULL; Spalte des Datensatzes, anhand der
#'     die Daten gruppiert werden, bevor es zur Berechnung der Ueberschneidungen
#'     kommt. Beim Wert NULL wird der gesamte Datensatz als eine Gruppe behandelt.
#' @param id Spaltenname, Standardwert NULL; Spalte, die als ID/Laufnummer verwendet
#'     werden soll. Die ID dient dazu im Output die sich ueberschneidenden Datenzeilen
#'     zu identifizieren. Beim Wert NULL wird automatisch eine Laufnummer vergeben.
#' @param von Spaltenname; Spalte, die den Beginnzeitpunkt des Zeitraums enthaelt.
#' @param bis Spaltenname; Spalte, die den Endzeitpunkt des Zeitraums enthaelt.
#' @param calc_interval Boolean, Standardwert TRUE; gibt an, ob
#'     Ueberschneidungszeitraeume nur geflagt werden sollen oder ob auch die
#'     Dauer der Ueberschneidung berechnet werden soll. Positive Werte in der
#'     Spalte OVERLAP_DUR zeigen die Anzahl der Ueberschneidungstage an, 0 zeigt
#'     einen nahtlosen Uebergang an, negative Werte geben an wieviele Tage zwei
#'     Zeitraume auseinander liegen. Enthalten die Spalten OVERLAP_FLAG, ID_H oder
#'     OVERLAP_DUR den Wert NA besteht die jeweilige Gruppe nur aus einer Beobachtung
#'     (d.h. es gibt keinen zweiten Zeitraum)
#' @return tibble; der urspruengliche Datensatz ergaenzt um die Spalten:
#'     \itemize{
#'         \item OVERLAP_ID: Bezugszeile zum originalen Datensatz, die anzeigt
#'             welche Zeilen sich zeitlich ueberschneiden. Wird keine ID als
#'             Parameter uebergeben (id=NULL), wird automatisch eine ID
#'             (die Zeilennummer) generiert. Die ID des originalen Datensatzes
#'             wird in diesem Fall durch OVERLAP_ID_ORI bezeichnet, die ID der
#'             dazu gematchten Ueberschneidung mit OVERLAP_ID. Wird ein Wert fuer
#'             id als Parameter uebergeben, ist die ID im originalen Datensatz
#'             durch diese Variable definiert. Bei den gematchten Ueberschneidungen
#'             wieder durch die Bezeichnung OVERLAP_ID. Kommt die Datenzeile im
#'             Datensatz nicht mehrfach vor, ist die OVERLAP_ID = NA.
#'         \item OVERLAP_FLAG: Datenzeilen, die im Datensatz mehrfach vorkommen,
#'             werden auf Ueberschneidung geprueft. Ueberschneiden sich die
#'             Zeitraeume, wird OVERLAP_FLAG auf TRUE gesetzt. Gibt es keine
#'             Ueberschneidung, auf FALSE. Kommt die Datenzeile im Datensatz nicht
#'             mehrfach vor, ist OVERLAP_FLAG = NA.
#'         \item OVERLAP_DUR: sofern ueber den Parameter calc_interval aktiviert,
#'             wird fuer mehrfach vorkommende Datenzeilen die Dauer potentieller
#'             Ueberschneidungen bzw. der zeitliche Abstand zwischen zwei
#'             Zeitraeumen berechnet:
#'             \itemize{
#'                 \item negative Werte: keine Ueberschneidung. Die Zeitraeume
#'                     sind n Tage voneinander getrennt.
#'                 \item 0: keine Ueberschneidung. Die Zeitraeume schließen direkt
#'                     aneinander an.
#'                 \item positive Werte: Ueberschneidung. Die Zeitraeume
#'                     ueberschneiden sich um n Tage.
#'             }
#'     }
#' @import dplyr
#' @import magrittr
#' @import rlang
#' @export
#'
.calc_overlap <- function(df,
                          grp=NULL,
                          id=NULL,
                          von,
                          bis,
                          calc_interval=T){
  # User*in-Input fuer NSE vorbereiten
  grp <- rlang::enquo(grp)
  id <- rlang::enquo(id)
  von <- rlang::enquo(von)
  bis <- rlang::enquo(bis)
  # Daten auf relevante Spalten reduzieren
  df_sub <- df %>%
    dplyr::select(!!grp,!!id,!!von,!!bis)
  # NULLS in Argumenten handhaben
  if (rlang::quo_is_null(grp)){
    # Einheitsgruppe fuer alle definieren
    df_sub <- df_sub %>%
      dplyr::mutate(OVERLAP_GRP=1) %>%
      dplyr::select(OVERLAP_GRP,!!id,!!von,!!bis)
    grp <- rlang::quo(OVERLAP_GRP)
  }
  if (rlang::quo_is_null(id)){
    # LFNR als ID generieren und fuer abschließenden Join auch im Ursprungs-df
    df <- df %>%
      dplyr::mutate(OVERLAP_ID_ORI=dplyr::row_number())
    df_sub <- df_sub %>%
      dplyr::mutate(OVERLAP_ID_ORI=dplyr::row_number()) %>%
      dplyr::select(!!grp,OVERLAP_ID_ORI,!!von,!!bis)
    id <- rlang::quo(OVERLAP_ID_ORI)
  }
  if (calc_interval){
    # Overlap flaggen + Dauer berechnen
    df_sub <- df_sub %>%
      dplyr::left_join(df_sub %>%
                         magrittr::set_colnames(c("GRP_H","ID_H","VON_H","BIS_H")),
                       setNames("GRP_H",rlang::as_name(grp))) %>%
      dplyr::filter(!!id!=ID_H)  %>%
      dplyr::mutate(MAX_BIS=dplyr::if_else(!!bis>=BIS_H,!!bis,BIS_H),
                    MIN_VON=dplyr::if_else(!!von<=VON_H,!!von,VON_H)) %>%
      dplyr::mutate(INT1=as.numeric(difftime(!!bis+1,!!von,units="days")),
                    INT2=as.numeric(difftime(BIS_H+1,VON_H,units="days")),
                    INT_GES=as.numeric(difftime(MAX_BIS+1,MIN_VON,units="days")),
                    OVERLAP_DUR=INT1+INT2-INT_GES,
                    OVERLAP_FLAG=ifelse(OVERLAP_DUR>0,T,F),
                    OVERLAP_ID=ID_H) %>%
      dplyr::select(!!id,OVERLAP_ID,OVERLAP_FLAG,OVERLAP_DUR)
  } else {
    # Overlap flaggen
    df_sub <- df_sub %>%
      dplyr::left_join(df_sub %>%
                         magrittr::set_colnames(c("GRP_H","ID_H","VON_H","BIS_H")),
                       setNames("GRP_H",rlang::as_name(grp))) %>%
      dplyr::filter(!!id!=ID_H) %>%
      dplyr::mutate(OVERLAP_FLAG=VON_H<=!!bis & BIS_H >= !!von,
                    OVERLAP_ID=ID_H) %>%
      dplyr::select(!!id,OVERLAP_ID,OVERLAP_FLAG)
  }
  # Overlap-Berechnung wieder zu Ursprungs-df joinen
  df <- df %>%
    dplyr::left_join(df_sub,setNames(rlang::as_name(id),rlang::as_name(id)))
  # Output
  return(df)
}
