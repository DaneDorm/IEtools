#' This Function takes data from a Time Study and finds the Standard Time
#' @param dat a dataframe, the observed times for each element of the job, with the accompanying PRs
#' @param irreg a dataframe, the irregular job elements, times and frequencies
#' @param allow a decimal, the Allowance for this work environment
#'
#'@import dplyr
#'
#' @export
stnd_time <- function(dat, irreg = NULL, allow = 0.1) {

  n_obs <- length(dat) / 2
  n_elements <- nrow(dat)
  dts <- data.frame(matrix(0, nrow = n_elements, ncol = n_obs))

  CycleTimes <- irreg[1] * irreg[2]
  IrregCycleTime <- sum(CycleTimes)

  i <- 1
  obs <- 1
  while (i < length(dat)) {
    dts[obs] <- dat[i] * dat[i + 1]

    i <- i + 2
    obs <- obs + 1
  }
  NT <- dts %>%
    summarize(across(.fns = mean))# %>% # Now have time for each cycle/observation
  NT <- NT + IrregCycleTime
  AvgNT <- mean(t(NT)) # Average Cycle Time, including irregular cycle elements
  ST <- AvgNT * (1 + allow)
  ST <- data.frame(ST, row.names = c("Standard Time"))
  colnames(NT) <- paste("Cycle", seq_along(1:n_obs), sep = " ")
  rownames(NT) <- c("Normal Time")
  return(list(ST, NT))
}
