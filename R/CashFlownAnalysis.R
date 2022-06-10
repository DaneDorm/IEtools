#' This function generates end of period cash flows,
#' for receiving returns or paying off debts
#' @param prin A number, the initial amount or principal
#' @param
#' @param i A decimal, the interest rate per period
#' @param n An integer, the number of periods to be studied
#'
#'
#'
#' @export
cash_flow <- function(prin, i = 0.05, n = 12) {
  Period <- seq(from = 0, to = n, by = 1)
  dat <- data.frame(Period)

  dat$CashFlow[1] <- prin
  i <- 2
  while (i <= nrow(dat)) {
    dat$CashFlow[i] <- dat$CashFlow[i - 1] * (1 + i)
    i <- i + 1
  }

  dat$CashFlow <- as.numeric(round(dat$CashFlow, digits = 2))

  return(dat)
}
#' This function generates a cash flow visualization
#' @param flow a vector of the cash flow data
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return a cash flow viz
#'
#' @export
cash_viz <- function(flow) {
  dat <- data.frame("Flow" = flow, "Period" = seq(1, length(flow))) %>%
    mutate(Account = ifelse(Flow >= 0, "Recieved", "Paid")) %>%
    ggplot() +
    geom_col(aes(x = Period, y = Flow, fill = Account))

  dat <- graphFormat(dat)

  return(dat)

}
