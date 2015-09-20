#' Theme lazappi
#' 
#' My ggplot2 theme
#' 
#' @param base.size   Base font size
#' @param base.family Base font family
#' 
#' @return ggplot2 theme object
#' 
#' @export
theme_lazappi <- function(base.size = 15, base.family = "") {
    
    ggplot2::theme(
        ## Elements in this first block aren't used directly, but are inherited
        ## by others
        line                 = ggplot2::element_line(colour     = "black",
                                                     size       = 0.5,
                                                     linetype   = 1,
                                                     lineend    = "butt"),
        rect                 = ggplot2::element_rect(fill       = "white",
                                                     colour     = "black",
                                                     size       = 0.5,
                                                     linetype   = 1),
        text                 = ggplot2::element_text(family     = base.family,
                                                     face       = "plain",
                                                     colour     = "black",
                                                     size       = base.size,
                                                     hjust      = 0.5,
                                                     vjust      = 0.5,
                                                     angle      = 0,
                                                     lineheight = 0.9),
        axis.title           = ggplot2::element_text(size       = rel(1.2)),
        axis.text            = ggplot2::element_text(size       = rel(0.9),
                                                     colour     = "grey40"),
        strip.text           = ggplot2::element_text(size       = rel(0.8)),
 
        #--------------- Axis ---------------#
        axis.line            = ggplot2::element_blank(),
        #axis.line.y          = ggplot2::element_blank(),
        axis.text.x          = ggplot2::element_text(vjust      = 1),
        axis.text.y          = ggplot2::element_text(hjust      = 1),
        axis.ticks           = ggplot2::element_line(colour     = "grey40",
                                                     size       = 1),
        axis.ticks.length    = grid::unit(0.2, "cm"), 
        axis.title.x         = ggplot2::element_text(vjust      = -2),
        axis.title.y         = ggplot2::element_text(angle      = 90,
                                                     vjust      = 2),
        axis.ticks.length    = grid::unit(0.15, "cm"),
        axis.ticks.margin    = grid::unit(0.1, "cm"),
 
        #--------------- Legend ---------------#
        legend.background    = ggplot2::element_blank(), #ggplot2::element_rect(colour     = NA),
        legend.margin        = grid::unit(0.5, "cm"),
        legend.key           = ggplot2::element_blank(), #ggplot2::element_rect(colour     = "grey80"),
        legend.key.size      = grid::unit(1.2, "lines"),
        legend.key.height    = NULL,
        legend.key.width     = NULL,
        legend.text          = ggplot2::element_text(size       = rel(0.9)),
        legend.text.align    = NULL,
        legend.title         = ggplot2::element_text(size       = rel(0.9),
                                                     face       = "bold",
                                                     hjust      = 0),
        legend.title.align   = 0.5,
        legend.position      = "right",
        legend.direction     = NULL,
        legend.justification = "center",
        legend.box           = NULL,
 
        #--------------- Panel ---------------#
        panel.background     = ggplot2::element_blank(), #ggplot2::element_rect(fill       = "grey98",
                                                     #colour     = NA),
        panel.border         = ggplot2::element_blank(), #ggplot2::element_rect(fill       = NA,
                                                     #colour     = "grey50"),
        panel.grid.major     = ggplot2::element_blank(), #ggplot2::element_line(colour     = "grey90",
                                                     #size       = 0.2),
        panel.grid.minor     = ggplot2::element_blank(), #ggplot2::element_line(colour     = "grey98",
                                                     #size       = 0.5),
        panel.margin         = grid::unit(0.25, "lines"),
        panel.margin.x       = NULL,
        panel.margin.y       = NULL,
 
        #--------------- Strip ---------------#
        strip.background     = ggplot2::element_rect(fill       = "grey80",
                                                     colour     = "grey50",
                                                     size       = 0.2),
        strip.text.x         = ggplot2::element_text(),
        strip.text.y         = ggplot2::element_text(angle      = -90),
 
        #--------------- Plot ---------------#
        plot.background      = ggplot2::element_rect(colour     = "white"),
        plot.title           = ggplot2::element_text(size       = rel(1.8),
                                                     colour     = "black",
                                                     vjust      = 3),
                                          # T  R  B    L
        plot.margin          = grid::unit(c(2, 1, 1.5, 1.5), "lines"),
 
        complete             = TRUE
   )
 }