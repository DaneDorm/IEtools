#' Generate a Control Chart for data
#' @param dat a dataframe
#' @param value the column in the dataframe where the values are to graph
#'
#' @return a basic Control Chart visualization
#'
#' @import ggplot2
#'
#'@export
ControlChart <- function(dat, value) {

  Xbar <- dat %>%
    pull({{value}}) %>%
    mean()

  stdev <- dat %>%
    pull({{value}}) %>%
    sd()

  CL <- data.frame(c("+3sigma", "+2sigma", "+1sigma", "Mean",
                      "-1sigma", "-2sigma", "-3sigma")
                    )
  colnames(CL) <- c("CL")

  CL$Value <- c(Xbar + stdev * 3,
                 Xbar + stdev * 2,
                 Xbar + stdev,
                 Xbar,
                 Xbar - stdev,
                 Xbar - stdev * 2,
                 Xbar - stdev * 3)
  CL$Value <- round(CL$Value, digits = 2)


  cls <- dat %>%
    mutate(Obs = seq_along(1:nrow(dat))) %>%
    ggplot() +
    ylim(Xbar - stdev * 4, Xbar + stdev * 4) +
    geom_point(mapping = aes(x = Obs, y = {{value}})) +
    geom_hline(yintercept = CL$Value[1], color = "red", size = 2) +
    geom_text(aes(0, CL$Value[1], label = "UCL", vjust = - 1), color = "red") +
    geom_hline(yintercept = CL$Value[2], color = "yellow", size = 2) +
    geom_hline(yintercept = CL$Value[3], color = "green", size = 2) +
    geom_text(aes(0, CL$Value[4], label = "Mean", vjust = - 1), color = "black") +
    geom_hline(yintercept = CL$Value[4], color = "grey", size = 2) +
    geom_hline(yintercept = CL$Value[5], color = "green", size = 2) +
    geom_hline(yintercept = CL$Value[6], color = "yellow", size = 2) +
    geom_hline(yintercept = CL$Value[7], color = "red", size = 2) +
    geom_text(aes(0, CL$Value[7], label = "LCL", vjust = - 1), color = "red") +
    geom_point(mapping = aes(x = Obs, y = {{value}}))

  cls <- graphFormat(cls)




  return(cls)



}
