#----------------------------------------

#   # custom GG theme
#   # Starts with theme_bw() and change some things
#   # Updates a number of geom defaults
#
#   # Requires Droid Sans Mono
#   # (https://fonts.google.com/specimen/Droid+Sans+Mono)
#   # How to do this:
#   # Droid Mono must be brought into R with pkg `extrafont`
#   library("extrafont") # loads type to graphics device
#   font_import()        # run this 1st time you use extrafont
#   loadfonts()          # brings font names into R

#----------------------------------------

theme_dm <- function(base_size = 11, base_family = "") {

  update_geom_defaults("ribbon", list(fill = "gray80",
                                      color = NULL,
                                      alpha = 0.5))

  update_geom_defaults("text", list(colour = "gray50",
                                    size = 3))


  #----------------------------------------
  #   start with theme_bw()
  #----------------------------------------

  theme_bw(base_size = base_size, base_family = base_family ) %+replace%
    theme(
      #----------------------------------------
      #   panels
      #----------------------------------------
      panel.background = element_rect(fill="#ffffff", colour=NA),
      # panel.background = element_blank(),
      panel.border = element_rect(size = 0.5, fill = NA, colour = "gray50"),
      # panel.border = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      # panel.spacing = unit(1, "lines"),
      panel.grid.major = element_line(size=0.25, colour="gray80"),
      panel.grid.minor = element_blank(),


      #----------------------------------------
      #   strips
      #----------------------------------------
      strip.text = element_text(size=rel(0.8),
                                # family="Source Sans Pro Semibold",
                                color = "gray50"),
      strip.background = element_rect(fill="gray90", colour="gray50"),
      # strip.background = element_blank(),


      #----------------------------------------
      #   axes
      #----------------------------------------
      title = element_text(size = rel(1),
                           vjust = 1.2,
                           # family = "Source Sans Pro Semibold",
                           color = "gray40"),
      axis.title = element_text(size = rel(0.9),
                                # family = "Source Sans Pro Semibold",
                                color = "gray50"),
      axis.title.x = element_text(margin = margin(t = 10, b = 10)),
      # axis.title.y = element_text(margin = margin(r = 10)),

      axis.text = element_text(size = rel(0.7),
                               colour = "gray50"),
      #family = "Droid Sans Mono"),

      axis.ticks = element_line(size=0.25, colour="gray80"),
      # axis.ticks = element_line(),
      # axis.ticks.x = element_blank(),
      # axis.ticks.y = element_blank(),
      # axis.ticks.length = unit(1, "lines")


      #----------------------------------------
      #   legends
      #----------------------------------------
      legend.background = element_rect(color = "white"),
      legend.title = element_text(size = rel(0.8), color = "gray50"),
      legend.key = element_blank(),
      legend.text = element_text(size = rel(0.8), color = "gray50")

    )

}


ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(fill = Species)) +
  theme_dm()
