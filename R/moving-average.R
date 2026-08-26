moving_average <- function(site) {
  result <- tibble(
    window_start = seq(ymd("1988-01-01"), ymd("1995-01-01"), by = "9 weeks"),
    K = NA,
    Mg = NA,
    Ca = NA,
    "NO3-N" = NA,
    "NH4-N" = NA
  )

  for (i in 1:nrow(result)) {
    w1 <- result$window_start[i]
    w2 <- result$window_start[i] + weeks(9)

    in_window <- site$Sample_Date >= w1 & site$Sample_Date < w2 # Which samples are inside the window

    # Pull out the ion concentrations that fall inside the window
    k_window <- site$K[in_window]
    mg_window <- site$Mg[in_window]
    ca_window <- site$Ca[in_window]
    no3n_window <- site$"NO3-N"[in_window]
    nh4n_window <- site$"NH4-N"[in_window]

    # Calculate the mean of each ion concentration and fill in the result
    result$K[i] <- mean(k_window, na.rm = TRUE)
    result$Mg[i] <- mean(mg_window, na.rm = TRUE)
    result$Ca[i] <- mean(ca_window, na.rm = TRUE)
    result$"NO3-N"[i] <- mean(no3n_window, na.rm = TRUE)
    result$"NH4-N"[i] <- mean(nh4n_window, na.rm = TRUE)
  }
  return(result)
}
