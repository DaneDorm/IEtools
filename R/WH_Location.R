#' This function solves a Set Covering Model
#' @param dat A matrix of which Warehouse covers which retailer,
#' retailers are rows and Warehouses are columns,
#' 1 means the WH can cover the retailer, 0 otherwise
#'
#' @return a matrix of the best result, which WH covers which retailers
#'
#' @export
wh_cover <- function(dat) {

  n_retail <- nrow(dat)
  n_wh <- ncol(dat)
  best_results <- c()
  best_OF <- n_wh + 1  # Starting values
  i <- 1
  while (i <= 500) {
    results <- matrix(sample(0:1, size = n_retail * n_wh, replace = TRUE),  # For now, random starting values
                      nrow = n_retail,                                      # Smarter algorithm coming soon
                      ncol = n_wh)
    # Retail Constraints
    retail_constraint <- c()
    cur_r <- 1
    r <- 1
    while (cur_r <= n_retail) {
      wh_max <- r + n_wh - 1

      retail_constraint[cur_r] <- sum(dat[r:wh_max] * results[r:wh_max])

      cur_r <- cur_r + 1
      r <- r + n_wh - 1
    }
    retail_constraint <- ifelse(retail_constraint >= 1, TRUE, FALSE)
    build_wh <- colSums(results) #
    build_wh <- ifelse(build_wh >= 1, TRUE, FALSE) # If wh is used to supply retail, will have col sum of more than 1

    # Now to find the value of the objective function
    if (sum(retail_constraint) >= n_retail) { # If meets contraints
      #all_results <- append(all_results, results)

      build_wh <- colSums(results) #
      build_wh <- ifelse(build_wh >= 1, TRUE, FALSE) # If wh is used to supply retail, will have col sum of more than 1
      OF <- sum(build_wh) # OF tried to min the number of WH built

      if (OF < best_OF) { # New Best Result
        best_results <- results
        best_OF <- OF
        best_results <- data.frame(best_results, row.names = paste0("Retailer #", seq_along(1:n_retail)))
        colnames(best_results) <- paste0("WH #", seq_along(1:n_wh))
        best_results <- rbind(best_results, ifelse(colSums(best_results) >= 1, TRUE, FALSE))
        rownames(best_results)[n_retail + 1] <- "Build WH?"
      }

    }



    i <- i + 1
  }

  return(best_results)
}

#' This function solves a Set Partitioning Model
#' @param dat A matrix of which Warehouse covers which retailer,
#' retailers are rows and Warehouses are columns,
#' 1 means the WH can cover the retailer, 0 otherwise
#'
#' @return a matrix of the best result, which WH covers which retailers
#'
#' @export
wh_partition <- function(dat) {

  n_retail <- nrow(dat)
  n_wh <- ncol(dat)
  best_results <- c("No Result Found")
  best_OF <- n_wh + 1  # Starting values
  i <- 1
  while (i <= 2000) {
    results <- matrix(sample(0:1, size = n_retail * n_wh, replace = TRUE),  # For now, random starting values
                      nrow = n_retail,                                      # Smarter algorithm coming soon
                      ncol = n_wh)
    # Retail Constraints
    retail_constraint <- c()
    cur_r <- 1
    r <- 1
    while (cur_r <= n_retail) {
      wh_max <- r + n_wh - 1

      retail_constraint[cur_r] <- sum(dat[r:wh_max] * results[r:wh_max])

      cur_r <- cur_r + 1
      r <- r + n_wh - 1
    }
    retail_constraint <- ifelse(retail_constraint == 1, TRUE, FALSE)  # This is the only difference, only one WH per retail
    build_wh <- colSums(results) #
    build_wh <- ifelse(build_wh >= 1, TRUE, FALSE) # If wh is used to supply retail, will have col sum of more than 1

    # Now to find the value of the objective function
    if (sum(retail_constraint) >= n_retail) { # If meets contraints
      #all_results <- append(all_results, results)

      build_wh <- colSums(results) #
      build_wh <- ifelse(build_wh >= 1, TRUE, FALSE) # If wh is used to supply retail, will have col sum of more than 1
      OF <- sum(build_wh) # OF tried to min the number of WH built

      if (OF < best_OF) { # New Best Result
        best_results <- results
        best_OF <- OF
        best_results <- data.frame(best_results, row.names = paste0("Retailer #", seq_along(1:n_retail)))
        colnames(best_results) <- paste0("WH #", seq_along(1:n_wh))
        best_results <- rbind(best_results, ifelse(colSums(best_results) >= 1, TRUE, FALSE))
        rownames(best_results)[n_retail + 1] <- "Build WH?"

      }

    }



    i <- i + 1
  }

  return(best_results)
}
