#' This function compares different production strategies to meet forcasted demand
#' @param forecast a vector of forecasted demand for a single product
#' @param method a specific production strategy, either Level, Chase, or Flexibility
#' @param inv0 starting inventory
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#'
#' @return a visual showing how inventory and production changes over time
#'
#' @export
produce_compare <- function(forecast, method = "Level", inv0 = 0) {
  if (method == "Level"){
    dat <- data.frame("Forecast" = forecast)

    level = mean(forecast)
    dat$Produce <- round(level, digits = 0)

    dat$Period <- seq_along(1:length(forecast))
    dat$Inventory[1] <- inv0 + dat$Produce[1] - dat$Forecast[1]

    for (i in seq(from = 2, to = nrow(dat))) {
      dat$Inventory[i] <- dat$Inventory[i - 1] + dat$Produce[i] - dat$Forecast[i]
    }

    viz <- dat %>%
      select(Period, Forecast, Produce, Inventory) %>%
      pivot_longer(cols = Forecast:Inventory,
                   names_to = "Metric",
                   values_to = "Value") %>%
      ggplot() +
      geom_col(aes(x = Period, y = Value, fill = Metric),
               position = "dodge"
               )

    viz <- graphFormat(viz)
    return(viz)

  } else if (method == "Chase") {
    dat <- data.frame("Forecast" = forecast)

    dat$Produce <- dat$Forecast

    dat$Period <- seq_along(1:length(forecast))
    dat$Change[1] <- 0

    for (i in seq(from = 2, to = nrow(dat))) {
      dat$Change[i] <- abs(dat$Produce[i - 1] - dat$Produce[i])
    }

    viz <- dat %>%
      select(Period, Forecast, Produce, Change) %>%
      pivot_longer(cols = Forecast:Change,
                   names_to = "Metric",
                   values_to = "Value") %>%
      ggplot() +
      geom_col(aes(x = Period, y = Value, fill = Metric),
               position = "dodge")

    viz <- graphFormat(viz)
    return(viz)


  } else if (method == "Flex") {
    dat <- data.frame("Forecast" = forecast)

    dat$BaseProduce <- min(forecast)


    dat$Period <- seq_along(1:length(forecast))

    for (i in seq(from = 1, to = nrow(dat))) {
      dat$AddProduce[i] <- dat$Forecast[i] - dat$BaseProduce[i]
    }

    viz <- dat %>%
      mutate(AddProduce = ifelse(AddProduce < 0, 0, AddProduce)) %>%
      select(Period, Forecast, BaseProduce, AddProduce) %>%
      pivot_longer(cols = Forecast:AddProduce,
                   names_to = "Metric",
                   values_to = "Value") %>%
      ggplot() +
      geom_col(aes(x = Period, y = Value, fill = Metric),
               position = "dodge"
      )

    viz <- graphFormat(viz)
    return(viz)

  } else {
    print("Please choose one of the methods supported by this function.")
  }

}
