#'This function formats visuals for using general rules
#'@param viz the visualization object, created with ggplot
#'
#'@return a visualization
#'
#'@import ggplot2
#'@import extrafont
#'
#'@export
graphFormat <- function(viz) {

  viz <- viz +
    theme(plot.title.position = "plot", # Moves title/subtitle to top left
          axis.text.y = element_text(angle = 0), # Rotate Tick Labels
          axis.title.y = element_text(angle = 0, # Rotate Axis Label
                                      vjust = .5),  # Move Label to mid
          panel.grid.major = element_blank(), # Remove grid
          panel.grid.minor = element_blank(), # Remove grid
          plot.title = element_text(face = "bold",
                                    family = "roboto"), #family = "serif"), # Change text properties
          plot.subtitle = element_text(face = "bold.italic",
                                       color = "darkblue"), # Change text properties
          panel.background = element_rect(fill = "gray95"), # Change background of just the graph
          axis.line = element_line(color = "black"),
          legend.position = "none")
  return(viz)

}

#'@param Title a string for the title of the viz
#'@param Subtitle a string for the subtitle of the viz
#'@param X a string for the title of the x axis of the viz
#'@param Y a string for the title of the y axis of the viz
