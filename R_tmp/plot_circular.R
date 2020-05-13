#' plot_circular: Erstellt Circular Bar Plot
#' 
#' @param dat asdf
#' @param .x sdaf
#' @param .y sdaf
#' @param .x_label asdf
#' @param .grp asdf
#' @param label_space sadf
#' @param grp_space sdaf
#' @param grp_info_space asdf
#' @param fill_pal sf
#' @param show_grid sadf
#' @param show_grid_label sdlfj
#' @param show_grp_info sdlfj
#' @param show_bar_label asdklfj
#' @param title asdf
#' @param subtitle sdaf
#' @param typ asdf 
#' @param return_plot asdf 
#' @return plot-Objekt
#' @examples
#' plot_circular(dat=show_expl("circular"))
#' @import dplyr
#' @import rlang
#' @import ggsci
#' @import ggplot2
#' @importFrom plyr round_any
#' @export
#'
plot_circular <- function(dat,
                          .x=POLBEZ,
                          .y=PFLICHT,
                          .x_label=POLBEZ,
                          .grp=BDL,
                          label_space=NULL,
                          grp_space=4,
                          grp_info_space=NULL,
                          fill_pal=NULL,
                          show_grid=F,
                          show_grid_label=F,
                          show_grp_info=F,
                          show_bar_label=T,
                          title="",
                          subtitle="",
                          typ=1){
    # Tidyverse
    .x <- rlang::enquo(.x)
    .y <- rlang::enquo(.y)
    .grp <- rlang::enquo(.grp)
    .x_label=rlang::enquo(.x_label)
    
    if (is.null(label_space)) label_space <- mean(dat[[rlang::as_label(.y)]])*0.5
    if (is.null(grp_info_space)) grp_info_space <- label_space*2
    if (is.null(fill_pal)) fill_pal <- ggsci::pal_d3(alpha=0.5)(10)
    
    # auf Spalten reduzieren, um Konflikte zu vermeiden
    dat <- dat %>%
        dplyr::select(!!.x,!!.y,!!.x_label,!!.grp) %>%
        dplyr::mutate(ID=dplyr::row_number())
    
    # Hilfsvariablen
    max_y <- max(dat[[rlang::as_label(.y)]],na.rm=T)
    if (show_grid){
        grid_space <- (max_y/4) %>%
            floor() %>%
            plyr::round_any(10**(nchar(.)-1),ceiling)
    } 
    
    # Plot
    if (typ==1){
        # Inputdaten weiter aufbereiten
        dat <- dat %>% 
            # Winkel fuer Label: 0.5 zentriert im Balken
            dplyr::mutate(ANGLE=90-360*(ID-0.5)/nrow(.),
                          HJUST=dplyr::if_else(ANGLE < -90,1,0),
                          LABEL_POS=!!.y+label_space) %>%
            # ANGLE anpassen -> sonst stimmt der Abstand zu den Balken nicht
            dplyr::mutate(ANGLE=dplyr::if_else(ANGLE < -90,ANGLE+180,ANGLE))
        # Plot
        res <- dat %>%
            ggplot2::ggplot(ggplot2::aes(x=!!.x,
                                         y=!!.y,
                                         fill=!!.grp)) +
            ggplot2::geom_bar(stat="identity") +
            # Labels fuer Bars hinzufuegen
            ggplot2::geom_text(data=dat,
                               ggplot2::aes(x=!!.x,
                                            y=LABEL_POS,
                                            label=!!.x_label,
                                            hjust=HJUST),
                               color="#939393",
                               #alpha=0.8,
                               size=2,
                               angle=dat$ANGLE,
                               inherit.aes=FALSE) +
            # Kreis
            ggplot2::coord_polar(start = 0) +
            # Kreis-Limits: 1. Wert negativ -> Innenkreis, 2. Wert NA -> alle Datenpunkte
            ggplot2::ylim(-1*max(dat[[rlang::as_label(.y)]]),NA) +
            # Titel/Label/Guides
            ggplot2::ggtitle(title,subtitle) +
            ggplot2::guides(fill=ggplot2::guide_legend(title.theme=ggplot2::element_blank(),
                                                       nrow=1)) +
            # Theme
            ggplot2::scale_fill_manual(values=fill_pal) +
            ggplot2::theme_minimal() +
            ggplot2::theme(plot.title=ggplot2::element_text(color="#939393",hjust=0.5),
                           plot.subtitle=ggplot2::element_text(color="#939393",
                                                               margin=ggplot2::margin(b=2),
                                                               hjust=0.5),
                           axis.text=ggplot2::element_blank(),
                           axis.title=ggplot2::element_blank(),
                           panel.grid=ggplot2::element_blank(),
                           plot.margin=ggplot2::margin(t=10,
                                                       b=0,
                                                       l=0,
                                                       r=0),
                           legend.position=c(0.5,0.02),
                           legend.direction="horizontal",
                           legend.key.size=ggplot2::unit(0.5,"line"))
    } else if (typ==2){
        # Abstand zwischen Gruppen einfuegen
        dat <- dat %>%
            # Abstand zwischen Gruppen einfuegen
            dplyr::bind_rows(dplyr::tibble(!!.grp:=rep(levels(dat[[rlang::as_label(.grp)]]),
                                                       each=grp_space)) %>%
                                 dplyr::mutate(!!.grp:=factor(!!.grp)))  %>%
            dplyr::arrange(BDL) %>%
            dplyr::mutate(ID=dplyr::row_number()) %>%
            # Winkel fuer Label: 0.5 zentriert im Balken
            dplyr::mutate(ANGLE=90-360*(ID-0.5)/nrow(.),
                          HJUST=dplyr::if_else(ANGLE < -90,1,0),
                          LABEL_POS=!!.y+label_space) %>%
            # ANGLE anpassen -> sonst stimmt der Abstand zu den Balken nicht
            dplyr::mutate(ANGLE=dplyr::if_else(ANGLE < -90,ANGLE+180,ANGLE)) 
      
        # Hilfstabelle fuer Baseline und Grid
        dat_supp <- dat %>%
            dplyr::group_by(!!.grp) %>%
            dplyr::summarise(START_BASE=min(ID),
                             END_BASE=max(ID)-grp_space,
                             LABEL_POS=mean(c(START_BASE,END_BASE))) %>%
            dplyr::mutate(START_GRID=START_BASE-1,
                          ENDE_GRID=dplyr::lag(END_BASE,1)+1) 
        res <- dat %>%
            ggplot2::ggplot(ggplot2::aes(x=as.factor(ID),y=!!.y,fill=!!.grp)) +
            ggplot2::geom_bar(stat="identity") +
            # Kreis
            ggplot2::coord_polar(start = 0) +
            # Kreis-Limits: 1. Wert negativ -> Innenkreis, 2. Wert NA -> alle Datenpunkte
            ggplot2::ylim(-1*max(dat[[rlang::as_label(.y)]],na.rm=T),NA) +
            # Titel/Label/Guides
            ggplot2::ggtitle(title,subtitle) +
            ggplot2::guides(fill=ggplot2::guide_legend(title.theme=ggplot2::element_blank(),
                                                       nrow=1)) +
            # Theme
            ggplot2::scale_fill_manual(values=fill_pal) +
            ggplot2::theme_minimal() +
            ggplot2::theme(plot.title=ggplot2::element_text(color="#939393",hjust=0.5),
                           plot.subtitle=ggplot2::element_text(color="#939393",
                                                               margin=ggplot2::margin(b=2),
                                                               hjust=0.5),
                           axis.text=ggplot2::element_blank(),
                           axis.title=ggplot2::element_blank(),
                           panel.grid=ggplot2::element_blank(),
                           plot.margin=ggplot2::margin(t=10,
                                              b=0,
                                              l=0,
                                              r=0),
                           legend.position=c(0.5,0.02),
                           legend.direction="horizontal",
                           legend.key.size=ggplot2::unit(0.5,"line"))
        
        if (show_bar_label){
            res <- res +
                # Labels fuer Bars hinzufuegen
                ggplot2::geom_text(data=dat,
                                   ggplot2::aes(x=as.factor(ID),
                                                y=LABEL_POS,
                                                label=!!.x_label,
                                                hjust=HJUST),
                                   color="#939393",
                                   size=2,
                                   angle=dat$ANGLE,
                                   inherit.aes=FALSE)
        }
        
        # Grid hinzufuegen
        if (show_grid){
            y_list <- seq(0,max_y,grid_space)
            for (y_pos in y_list){
                res <- res +
                    ggplot2::geom_segment(data=dat_supp %>% 
                                              dplyr::filter(!is.na(ENDE_GRID)),
                                          ggplot2::aes(x=ENDE_GRID,
                                                       xend=START_GRID),
                                          y=y_pos,
                                          yend=y_pos,
                                          colour="#939393",
                                          alpha=1,
                                          size=0.3,
                                          inherit.aes=FALSE)
            }
            if (show_grid_label){
                # Grid-Text hinzufuegen
                res <- res +
                    ggplot2::annotate("text",
                                      x=rep(max(dat$ID),length(y_list)),
                                      y=y_list,
                                      label=y_list,
                                      color="#939393",
                                      size=.conv_size(6),
                                      angle=0,
                                      hjust=1,
                                      vjust=-1)
            }
            
        }
        
        # Infos zu Gruppen hinzufuegen
        if (show_grp_info){
            res <- res +
                ggplot2::geom_segment(data=dat_supp,
                                      ggplot2::aes(x=START_BASE,
                                                   y=0,
                                                   xend=END_BASE,
                                                   yend=0),
                                      colour="#000000",
                                      alpha=0.5,
                                      size=0.6,
                                      inherit.aes=FALSE) +
                ggplot2::geom_text(data=dat_supp,
                                   ggplot2::aes(x=LABEL_POS,
                                                y=-grp_info_space,
                                                label=!!.grp),
                                   hjust=0.5,
                                   colour="#000000",
                                   alpha=0.5,
                                   size=3,
                                   fontface="bold",
                                   inherit.aes=FALSE)
        }
    } else if (typ==3){
      # Abstand zwischen Gruppen einfuegen
      dat <- dat %>%
        # Abstand zwischen Gruppen einfuegen
        dplyr::bind_rows(dplyr::tibble(!!.grp:=rep(levels(dat[[rlang::as_label(.grp)]]),
                                                   each=grp_space)) %>%
                           dplyr::mutate(!!.grp:=factor(!!.grp)))  %>%
        dplyr::arrange(BDL) %>%
        dplyr::mutate(ID=dplyr::row_number()) %>%
        # Winkel fuer Label: 0.5 zentriert im Balken
        dplyr::mutate(ANGLE=90-360*(ID-0.5)/nrow(.),
                      HJUST=dplyr::if_else(ANGLE < -90,1,0),
                      LABEL_POS=!!.y+label_space) %>%
        # ANGLE anpassen -> sonst stimmt der Abstand zu den Balken nicht
        dplyr::mutate(ANGLE=dplyr::if_else(ANGLE < -90,ANGLE+180,ANGLE)) 
      
      # Hilfstabelle fuer Baseline und Grid
      dat_supp <- dat %>%
        dplyr::group_by(!!.grp) %>%
        dplyr::summarise(START_BASE=min(ID),
                         END_BASE=max(ID)-grp_space,
                         LABEL_POS=mean(c(START_BASE,END_BASE))) %>%
        dplyr::mutate(START_GRID=START_BASE-1,
                      ENDE_GRID=dplyr::lag(END_BASE,1)+1) 
      res <- dat %>%
        ggplot2::ggplot(ggplot2::aes(x=as.factor(ID),y=0,ymin=0,ymax=!!.y,color=!!.grp)) +
        ggplot2::geom_linerange(size=2,alpha=0.8) +
        # Kreis
        ggplot2::coord_polar(start = 0) +
        # Kreis-Limits: 1. Wert negativ -> Innenkreis, 2. Wert NA -> alle Datenpunkte
        ggplot2::ylim(-1*max(dat[[rlang::as_label(.y)]],na.rm=T),NA) +
        # Titel/Label/Guides
        ggplot2::ggtitle(title,subtitle) +
        ggplot2::guides(color=ggplot2::guide_legend(title.theme=ggplot2::element_blank(),
                                                   nrow=1)) +
        # Theme
        ggplot2::scale_color_manual(values=fill_pal) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title=ggplot2::element_text(color="#939393",hjust=0.5),
                       plot.subtitle=ggplot2::element_text(color="#939393",
                                                           margin=ggplot2::margin(b=2),
                                                           hjust=0.5),
                       axis.text=ggplot2::element_blank(),
                       axis.title=ggplot2::element_blank(),
                       panel.grid=ggplot2::element_blank(),
                       plot.margin=ggplot2::margin(t=10,
                                                   b=0,
                                                   l=0,
                                                   r=0),
                       legend.position=c(0.5,0.02),
                       legend.direction="horizontal",
                       legend.key.size=ggplot2::unit(0.5,"line"))
      
      if (show_bar_label){
        res <- res +
          # Labels fuer Bars hinzufuegen
          ggplot2::geom_text(data=dat,
                             ggplot2::aes(x=as.factor(ID),
                                          y=LABEL_POS,
                                          label=!!.x_label,
                                          hjust=HJUST),
                             color="#939393",
                             size=2,
                             angle=dat$ANGLE,
                             inherit.aes=FALSE)
      }
      
      # Grid hinzufuegen
      if (show_grid){
        y_list <- seq(0,max_y,grid_space)
        for (y_pos in y_list){
          res <- res +
            ggplot2::geom_segment(data=dat_supp %>% 
                                    dplyr::filter(!is.na(ENDE_GRID)),
                                  ggplot2::aes(x=ENDE_GRID,
                                               xend=START_GRID),
                                  y=y_pos,
                                  yend=y_pos,
                                  colour="#939393",
                                  alpha=1,
                                  size=0.3,
                                  inherit.aes=FALSE)
        }
        if (show_grid_label){
          # Grid-Text hinzufuegen
          res <- res +
            ggplot2::annotate("text",
                              x=rep(max(dat$ID),length(y_list)),
                              y=y_list,
                              label=y_list,
                              color="#939393",
                              size=.conv_size(6),
                              angle=0,
                              hjust=1,
                              vjust=-1)
        }
        
      }
      
      # Infos zu Gruppen hinzufuegen
      if (show_grp_info){
        res <- res +
          ggplot2::geom_segment(data=dat_supp,
                                ggplot2::aes(x=START_BASE,
                                             y=0,
                                             xend=END_BASE,
                                             yend=0),
                                colour="#000000",
                                alpha=0.5,
                                size=0.6,
                                inherit.aes=FALSE) +
          ggplot2::geom_text(data=dat_supp,
                             ggplot2::aes(x=LABEL_POS,
                                          y=-grp_info_space,
                                          label=!!.grp),
                             hjust=0.5,
                             colour="#000000",
                             alpha=0.5,
                             size=3,
                             fontface="bold",
                             inherit.aes=FALSE)
      }
    }
    return(res)
}
