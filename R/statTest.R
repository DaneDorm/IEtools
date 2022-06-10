#'Create a Two Sided Confidence Interval for a dataset
#'@param dat a list of numerical data
#'@param per what percent interval for CI, given as a decimal value
#'
#'@importFrom purrr map
#'
#'@export
stat_ci <- function(dat, per = .95) {

  if (is.numeric(dat)) {

    xbar <- mean(dat)
    alpha <- (1 - per) / 2
    len <- length(dat)
    sigma <- sd(dat)

    z <- c(alpha, 1 - alpha)
    result <- xbar + qnorm(z) * sigma / sqrt(len)
    return(result)

  } else {
    print("Please use a numeric dataset")
  }
}
#' Performs a t test on a dataset to see if there is significant difference between means
#' @param dat a dataframe, each treatment must be labeled, each row is an observation
#' @param trt the name of the column that labels each treatment
#' @param value the name of the column that holds the values
#'
#'
#'
#' @export
stat_ttest <- function(dat, trt, value) {

  n_obs <- nrow(dat)

  n_trt <- dat %>%
    select({{trt}}) %>%
    distinct() %>%
    nrow()



  sample_means <- dat %>%
    group_by({{trt}}) %>%
    summarise(across(.cols = {{value}},
                     .fns = list(mean, n, sd),
                     .names = "{.fn}"))

  return(sample_means)


}

#' Generates an ANOVA table for a given dataset with the associated P-values.
#' @param dat a dataframe, each treatment must be labeled, each row is an observation
#' @param trtA the name of the first column that labels each treatment
#' @param trtB the name of the first column that labels each treatment, not needed for one way anova
#' @param value the name of the column that holds the values
#' @param twoway logical, is this a two way ANOVA or one way
#'
#' @import dplyr
#' @importFrom stringr str_c
#'
#' @export
stat_anova <- function(dat, trtA, trtB = NULL, value, twoway = FALSE) {

  if (twoway) {  # if twoway is TRUE, follow this

    anova_table <- data.frame(matrix(0, nrow = 5, ncol = 5), row.names = c("TreatmentA",
                                                                           "TreatmentB",
                                                                           "Interaction",
                                                                           "Error",
                                                                           "Total"))
    colnames(anova_table) <- c("SS", "DoF", "MS", "Fvalue", "Pvalue")



    anova_dat <- dat %>%
      rename(TrtA = {{trtA}}) %>%   # Renaming columns allows me to call them as strings in the merge functions
      rename(TrtB = {{trtB}}) %>%   # Going with this for now, will come back and revist at a later date
      mutate(TrtAB = str_c(TrtA, TrtB, sep = "-"))

    TotalMean <- anova_dat %>%
      pull({{value}}) %>%
      mean()

    n_obs <- nrow(anova_dat)

    n_trtA <- length(unique(anova_dat$TrtA))
    trtA_size <- n_obs / n_trtA


    n_trtB <- length(unique(anova_dat$TrtB))
    trtB_size <- n_obs / n_trtB

    n_trtAB <- n_trtA * n_trtB
    trtAB_size <- n_obs / n_trtAB

    A_means <- anova_dat %>%
      group_by(TrtA) %>%
      summarise(Amean = mean({{value}})) %>%
      mutate(ADiff = (Amean - TotalMean) ** 2 * trtA_size)
    SStrtA <- sum(A_means$ADiff)



    B_means <- anova_dat %>%
      group_by(TrtB) %>%
      summarise(Bmean = mean({{value}})) %>%
      mutate(BDiff = ((Bmean - TotalMean) ** 2) * trtB_size)
    SStrtB <- sum(B_means$BDiff)



    AB_means <- anova_dat %>%
      group_by(TrtAB) %>%
      summarise(ABmean = mean({{value}}))



    anova_dat <- anova_dat %>%
      merge(A_means, by = "TrtA") %>%
      merge(B_means, by = "TrtB") %>%
      merge(AB_means, by = "TrtAB") %>%
      mutate(TotalMean = mean({{value}})) %>%
      mutate(ErrorDiff = ({{value}} - ABmean) ** 2) %>%
      mutate(TotalDiff = ({{value}} - TotalMean) ** 2)
    SSerror <- sum(anova_dat$ErrorDiff)
    SStotal <- sum(anova_dat$TotalDiff)
    SStrtAB <- SStotal - SSerror - SStrtA - SStrtB

    anova_table$SS <- c(SStrtA, SStrtB, SStrtAB, SSerror, SStotal)
    anova_table$DoF <- c(n_trtA - 1,
                         n_trtB - 1,
                         (n_trtA - 1) * (n_trtB - 1),
                         n_trtA * n_trtB * (trtAB_size - 1),
                         n_obs - 1)
    anova_table$MS <- anova_table$SS / anova_table$DoF
    anova_table$Fvalue <- anova_table$MS / anova_table$MS[4]
    anova_table$MS[5] <- NA
    anova_table$Pvalue <- pf(anova_table$Fvalue,
                                anova_table$DoF,
                                anova_table$DoF[4],
                                lower.tail = FALSE)
    anova_table$Fvalue[4:5] <- NA
    anova_table$Pvalue[4:5] <- NA

    return(anova_table)

  } else if (!twoway) { # If twoway is false, follow this

    anova_table <- data.frame(matrix(0, nrow = 3, ncol = 5), row.names = c("Treatment", "Error", "Total"))
    colnames(anova_table) <- c("SS", "DoF", "MS", "Fvalue", "Pvalue")

    n_obs <- nrow(dat)

    n_trt <- dat %>%
      select({{trtA}}) %>%
      distinct() %>%
      nrow()

    n_per_trt = n_obs / n_trt

    anova_table$DoF <- c(n_trt - 1, n_obs - n_trt, n_obs - 1)

    sample_means <- dat %>%
      rename(TreatmentA = {{trtA}}) %>%
      group_by(TreatmentA) %>%
      summarise(TrtMean = mean({{value}}))

    anova_dat <- dat %>%
      rename(TreatmentA = {{trtA}}) %>%
      merge(sample_means, by = "TreatmentA") %>%
      mutate(TotalMean = mean({{value}})) %>%
      mutate(SStrt = ((TrtMean - TotalMean) ** 2) * n_per_trt) %>%
      mutate(SStotal = ({{value}} - TotalMean) ** 2) %>%
      mutate(SSE = ({{value}} - TrtMean) ** 2)

    SStrt <- sum(unique(anova_dat$SStrt))
    SSE <- sum(anova_dat$SSE)
    SStotal <- sum(anova_dat$SStotal)

    anova_table$SS <- c(SStrt, SSE, SStotal)

    anova_table$MS <- anova_table$SS / anova_table$DoF

    anova_table$MS[3] <- NA

    anova_table$Fvalue <- c(anova_table$MS[1] / anova_table$MS[2], NA, NA)

    anova_table$Pvalue <- c(pf(anova_table$Fvalue[1],
                               anova_table$DoF[1],
                               anova_table$DoF[2],
                               lower.tail = FALSE),
                            NA,
                            NA)

    return(anova_table)

  } else {
    print("Please Provide a TRUE or FALSE value for the 'twoway' arguement.")
  }

}
