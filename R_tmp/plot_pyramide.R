#' plot_pyramide: Erstellt aus einem Input-Datensatz ein Pyramidenplot
#' 
#' @param dat asdf
#' @param .grp sdaf
#' @param .x sdaf
#' @param .n sdaf
#' @param .x_label asdf
#' @param grp_space sadf
#' @param tick_space_n sdaf
#' @param tick_space_x sf
#' @param fill_pal asdf
#' @param title asdf
#' @param subtitle sdaf
#' @param typ asdf 
#' @param return_plot asdf 
#' @return plot-Objekt
#' @examples
#' plot_pyramide(dat=show_expl("pyramide"))
#' @export
#' @import dplyr
#' @import rlang
#' @import ggplot2
#' @importFrom plyr round_any
#' @importFrom gridExtra grid.arrange
#'
plot_pyramide <- function(dat,
                          .grp=GESCHL,
                          .x=ALTER,
                          .n=N,
                          .x_label=ALTER_GRP5,
                          grp_space=0.05,
                          tick_space_n=NULL,
                          tick_space_x=NULL,
                          fill_pal=NULL,
                          title="",
                          subtitle="",
                          typ=1,
                          return_plot=T){
    library(magrittr)
    # Tidyeval quosure
    .grp <- rlang::enquo(.grp)
    .x <- rlang::enquo(.x)
    .n <- rlang::enquo(.n)
    .x_label <- rlang::enquo(.x_label)
    # Hilfsvar ableiten
    grp_lvl <- dat[[rlang::as_label(.grp)]] %>% 
        unique()
    dat <- dat %>%
        dplyr::mutate(!!.n:=dplyr::case_when(!!.grp==grp_lvl[1] ~ !!.n*-1,
                                             TRUE ~ !!.n))
    n_max <- dat[[rlang::as_label(.n)]] %>%
        abs() %>%
        max() %>%
        floor() %>%
        plyr::round_any(10**(nchar(abs(.))-1),ceiling)
    x_max <- dat[[rlang::as_label(.x)]] %>%
        max() %>%
        floor() %>%
        plyr::round_any(10**(nchar(abs(.))-1),ceiling)
    x_min <- dat[[rlang::as_label(.x)]] %>%
        min() %>%
        floor() %>%
        plyr::round_any(10**(nchar(abs(.))-1),floor)
    if (typ==1) grp_space <- n_max*grp_space
    if (is.null(tick_space_n)) {
        tick_space_n <- (n_max/5) %>%
            floor() %>%
            plyr::round_any(10**(nchar(abs(.))-1),ceiling)
    }
    if (is.null(tick_space_x)){
        tick_space_x <- (x_max/20) %>%
            floor() %>%
            plyr::round_any(10**(nchar(abs(.))-1),ceiling)
    }
    if (is.null(fill_pal)) fill_pal <- c("#374E55FF","#DF8F44FF")
    # Checks
    if (length(grp_lvl) != 2) warnings("Achtung: benötige 2 Gruppen für ein Pyramiden-Plot")
    # Plot
    if (typ==1){
        res <- dat %>%
            dplyr::mutate(YMAX=dplyr::if_else(!!.grp==grp_lvl[1],
                                              -grp_space+!!.n,
                                              grp_space+!!.n),
                          YMIN=dplyr::if_else(!!.grp==grp_lvl[1],
                                              -grp_space,
                                              grp_space)) %>%
            ggplot2::ggplot(ggplot2::aes(x=!!.x,
                                         color=!!.grp,
                                         y=0,
                                         ymin=YMIN,
                                         ymax=YMAX,
                                         label=!!.x_label)) +
            ggplot2::geom_linerange(size=1,alpha=0.8) +
            ggplot2::geom_label(size=.conv_size(8),
                                label.padding=ggplot2::unit(0.0,"lines"),
                                label.size=0,
                                label.r=ggplot2::unit(0.0,"lines"),
                                alpha=0.9,
                                color="#939393") +
            ggplot2::scale_y_continuous(breaks=c(seq(-n_max,0,tick_space_n)-grp_space,
                                                 seq(0,n_max,tick_space_n)+grp_space),
                                        labels=c(seq(n_max,0,-tick_space_n),
                                                 seq(0,n_max,tick_space_n)) %>%
                                            formatC(big.mark=".",decimal.mark=",",
                                                    format="f",digits=0)) +
            ggplot2::scale_color_manual(values=fill_pal) +
            ggplot2::guides(color=ggplot2::guide_legend(title.theme=ggplot2::element_blank())) +
            ggplot2::ggtitle(title,subtitle) +
            ggplot2::coord_flip() +
            ggplot2::theme_minimal() +
            ggplot2::theme(text=ggplot2::element_text(colour="#939393"),
                           axis.title=ggplot2::element_blank(),
                           axis.ticks.y=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           panel.grid.major=ggplot2::element_blank(),
                           panel.grid.minor=ggplot2::element_blank(),
                           panel.grid.minor.x=ggplot2::element_line(color=ggplot2::alpha("#939393",alpha=.3),
                                                                    linetype="dashed"),
                           panel.grid.major.x=ggplot2::element_line(color=ggplot2::alpha("#939393",alpha=.3),
                                                                    linetype="solid"),
                           legend.position=c(0.85,0.9),
                           legend.direction="horizontal")
        plot(res)
    } else {
        p1 <- dat %>%
            ggplot2::ggplot(ggplot2::aes(x=!!.x,
                                         group=!!.grp,
                                         fill=!!.grp)) +
            ggplot2::geom_bar(ggplot2::aes(y=!!.n),
                              stat="identity",
                              subset(dat,dat[[rlang::as_label(.grp)]]==grp_lvl[1]),
                              width=0.75,
                              position = ggplot2::position_dodge(width = 0.92)) +
            ggplot2::scale_y_continuous(breaks=seq(0,-n_max,-tick_space_n),
                                        labels=abs(seq(0,-n_max,-tick_space_n)),
                                        expand=c(0,0)) +
            ggplot2::scale_x_continuous(breaks=seq(x_min,x_max,tick_space_x),
                                        labels=seq(x_min,x_max,tick_space_x),
                                        sec.axis=ggplot2::dup_axis()) +
            ggplot2::scale_fill_manual(values=fill_pal[1]) +
            ggplot2::coord_flip() +
            ggplot2::guides(fill=F) +
            ggplot2::ggtitle(title,subtitle) +
            ggplot2::theme_minimal() +
            ggplot2::theme(text=ggplot2::element_text(colour="#939393"),
                           axis.title=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           axis.ticks.y.right=ggplot2::element_line(color="#939393"),
                           axis.line.y.right=ggplot2::element_line(color="#939393"),
                           panel.grid.major=ggplot2::element_blank(),
                           panel.grid.minor=ggplot2::element_blank(),
                           panel.grid.minor.x=ggplot2::element_line(color=ggplot2::alpha("#939393",alpha=.3),
                                                                    linetype="dashed"),
                           panel.grid.major.x=ggplot2::element_line(color=ggplot2::alpha("#939393",alpha=.3),
                                                                    linetype="solid"))
        
        p2 <- dat %>%
            ggplot2::ggplot(ggplot2::aes(x=!!.x,
                                         group=!!.grp,
                                         fill=!!.grp)) +
            ggplot2::geom_bar(ggplot2::aes(y=0),
                              stat="identity") +
            ggplot2::geom_text(ggplot2::aes(y=0,label=!!.x_label),
                               color="#939393",
                               size=.conv_size(8)) +
            ggplot2::scale_y_continuous(breaks=0,labels="") +
            ggplot2::scale_x_continuous(breaks=seq(x_min,x_max,tick_space_x),
                                        labels=seq(x_min,x_max,tick_space_x)) +
            ggplot2::coord_flip() +
            ggplot2::guides(fill=F) +
            ggplot2::ggtitle("","") +
            ggplot2::theme_minimal() +
            ggplot2::theme(text=ggplot2::element_text(colour="#939393"),
                           axis.title=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           axis.line=ggplot2::element_blank(),
                           axis.ticks=ggplot2::element_blank(),
                           panel.grid=ggplot2::element_blank(),
                           legend.direction="horizontal")
        
        p3 <- dat %>%
            ggplot2::ggplot(ggplot2::aes(x=!!.x,
                                         group=!!.grp,
                                         fill=!!.grp)) +
            ggplot2::geom_bar(ggplot2::aes(y=!!.n),
                              stat="identity",
                              subset(dat,dat[[rlang::as_label(.grp)]]==grp_lvl[2]),
                              width=0.75,
                              position = ggplot2::position_dodge(width = 0.92)) +
            ggplot2::scale_y_continuous(breaks=seq(0,n_max,tick_space_n),
                                        labels=seq(0,n_max,tick_space_n),
                                        expand=c(0,0)) +
            ggplot2::scale_x_continuous(breaks=seq(x_min,x_max,tick_space_x),
                                        labels=seq(x_min,x_max,tick_space_x)) +
            ggplot2::scale_fill_manual(values=fill_pal[2]) +
            ggplot2::coord_flip() +
            ggplot2::guides(fill=F) +
            ggplot2::ggtitle("","") +
            ggplot2::theme_minimal() +
            ggplot2::theme(text=ggplot2::element_text(colour="#939393"),
                           axis.title=ggplot2::element_blank(),
                           axis.text.y=ggplot2::element_blank(),
                           axis.ticks.y=ggplot2::element_line(color="#939393"),
                           axis.line.y=ggplot2::element_line(color="#939393"),
                           panel.grid.major=ggplot2::element_blank(),
                           panel.grid.minor=ggplot2::element_blank(),
                           panel.grid.minor.x=ggplot2::element_line(color=ggplot2::alpha("#939393",alpha=.3),
                                                                    linetype="dashed"),
                           panel.grid.major.x=ggplot2::element_line(color=ggplot2::alpha("#939393",alpha=.3),
                                                                    linetype="solid"))
        res <- gridExtra::grid.arrange(p1,
                                p2,
                                p3,
                                widths=c(0.48,0.04,0.48),
                                ncol=3)
    }
    if (return_plot) return(res)
}
