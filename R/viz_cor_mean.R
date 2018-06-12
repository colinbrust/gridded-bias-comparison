

viz_cor_mean <- function() {

  list(

    scale_color_manual(values = c("white","black"), guide = F),

    theme_minimal(),

    theme(
      axis.text.x = element_text(angle = 45, vjust = 1,
                                 hjust = 1)),

    coord_fixed(),

    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(family = "sans", colour="gray26",
                               size = 9, face = "bold"),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(0, 0),
      legend.position = c(0.15, -0.3),
      legend.direction = "horizontal",
      legend.title = element_text(family = "sans", colour="black",
                                  size = 10, face = "bold"),
      legend.text = element_text(family = "sans", colour="black",
                                 size = 8, face = "bold"),
      legend.text.align = 0.5,
      strip.text = element_text(family = "sans", size = 9.5, face = "bold",
                                hjust = 0.5, vjust = 1),
      plot.background = ggplot2::element_blank(),
      plot.title = element_text(family = "sans", size = 13, hjust = -0.5,
                                colour = "gray15", face = "bold"),
      plot.subtitle = element_text(family = "sans", size = 10, hjust = 0.5,
                                   colour = "gray20", face = "bold"))


  )
}
