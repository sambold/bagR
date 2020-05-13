################################################################################
#   DATEN EINLESEN
################################################################################
library(magrittr)

# Daten fuer marginal dist plot
# Endstand d AKtienkurse
data("EuStockMarkets")
stocks <- EuStockMarkets %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(DAY=dplyr::row_number()) %>%
    tidyr::pivot_longer(cols=c(DAX,SMI,CAC,FTSE),
                        names_to="STOCK",
                        values_to="VALUE") %>%
    dplyr::mutate(STOCK=factor(STOCK))
rm(EuStockMarkets)



# Treemap
url <- paste0("http://www.statistik.at/web_de/statistiken/",
              "menschen_und_gesellschaft/soziales/verbrauchsausgaben/",
              "konsumerhebung_2014_2015/110328.html")
col_label <- c("TYP","HAUS_EURO","HAUS_PCT","AQUI_EURO","AQUI_PCT")
main_grp <- c("Ernährung, Alkoholfreie Getränke",
              "Alkoholische Getränke, Tabakwaren",
              "Bekleidung, Schuhe",
              "Wohnen, Energie",
              "Wohnungsausstattung",
              "Gesundheit",
              "Verkehr",
              "Kommunikation",
              "Freizeit, Sport, Hobby",
              "Bildung",
              "Café, Restaurant",
              "Sonstige Ausgaben",
              "Nicht für den privaten Konsum3)")


# Daten laden
konsum <- xml2::read_html(url) %>%
    rvest::html_table(fill=T) %>%
    .[[1]] %>%
    magrittr::set_colnames(col_label) %>%
    dplyr::as_tibble() %>%
    tail(-5) %>%
    head(-1) %>%
    dplyr::mutate_at(.vars=c("HAUS_EURO","HAUS_PCT","AQUI_EURO","AQUI_PCT"),
                     ~plotR::.conv_number(.)) %>%
    dplyr::mutate(GRP_TYP=dplyr::if_else(TYP %in% main_grp,"main","sub")) 

grp_n <-  which(konsum$GRP_TYP=="main")[-1]-which(konsum$GRP_TYP=="main")[-length(main_grp)]
grp_n <- c(grp_n,nrow(konsum)-sum(grp_n))
grp_id <- rep.int(1:length(main_grp),times=grp_n)

konsum <- konsum %>%
    dplyr::mutate(GRP_ID=grp_id)

konsum <- konsum %>%
    dplyr::filter(GRP_TYP=="sub") %>%
    dplyr::select(GRP_ID,TYP,HAUS_EURO) %>%
    magrittr::set_names("GRP","SUB_GRP","VAL")


################################################################################
#   MARGINAL PLOT ----
################################################################################

# Achtung: DAY kann kein factor sein
p <- stocks %>%
    ggplot2::ggplot(ggplot2::aes(x=DAY,y=VALUE,color=STOCK)) +
    ggplot2::geom_point(size=.1) +
    # Achsen
    ggplot2::scale_x_continuous(breaks=seq(0,nrow(stocks),100),
                                labels=seq(0,nrow(stocks),100) %>%
                                    formatC(big.mark=".",decimal.mark=",",
                                            format="f",digits=0)) +
    ggplot2::scale_y_continuous(breaks=seq(0,max(stocks$VALUE),length.out=20),
                                labels=seq(0,max(stocks$VALUE),length.out=20) %>%
                                    formatC(big.mark=".",decimal.mark=",",
                                            format="f",digits=0)) +
    # Titel/Label/Guides
    ggplot2::ggtitle("Europäische Aktienindizes",
                     "Kursentwicklung") +
    ggplot2::xlab("Anzahl der Tage") +
    ggplot2::ylab("Aktienindex") +
    ggplot2::guides(color=ggplot2::guide_legend(title.theme=ggplot2::element_blank(),
                                                override.aes=list(size=2))) +
    # Theme
    ggsci::scale_color_jama(alpha=0.5) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title=ggplot2::element_text(color="#939393"),
                   plot.subtitle=ggplot2::element_text(color="#939393",
                                                       margin=ggplot2::margin(b=2)),
                   axis.title.x=ggplot2::element_text(color="#939393",
                                                      margin=ggplot2::margin(t=10)),
                   axis.title.y=ggplot2::element_text(color="#939393",
                                                      margin=ggplot2::margin(r=10)),
                   axis.text=ggplot2::element_text(color="#939393"),
                   panel.grid.minor=ggplot2::element_blank(),
                   panel.grid.major.y=ggplot2::element_line(color=ggplot2::alpha("#939393",alpha=.3),
                                                            linetype="dashed",
                                                            size=0.5),
                   # panel.grid.major.y=ggplot2::element_line(color=ggplot2::alpha("#939393",alpha=.3),
                   #                                          linetype="dashed",
                   #                                          size=0.5),
                   panel.grid.major.x=ggplot2::element_blank(),
                   #legend.position=c(0.1,0.8),
                   legend.position=c(0.5,0.87),
                   legend.direction="horizontal")
p %>%
    ggExtra::ggMarginal(typ="histogram",
                        margins="y",
                        groupFill=T,
                        alpha=0.5,
                        size=8,
                        yparams=list(bins=100))


################################################################################
#   TREEMAP
################################################################################

# Create data
group <- c("group-1","group-2","group-3")
value <- c(13,5,22)
data <- data.frame(group,value)

df <- dplyr::tibble(grp=c("A","A","A","B","C","C","A"),
                    subgrp=c(1,2,3,1,1,2,4),
                    val=c(5,4,8,20,5,5,3))

# treemap
treemap::treemap(df,
        index=c("grp","subgrp"),
        vSize="val",
        type="index")


# library
library(treemap)

# Build Dataset

# treemap
treemap::treemap(konsum,
        index=c("GRP_ID","TYP"),
        vSize="HAUS_EURO",
        type="index"
) 



