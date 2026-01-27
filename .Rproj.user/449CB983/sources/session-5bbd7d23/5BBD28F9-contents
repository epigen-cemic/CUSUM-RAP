## -----------------------------------------------------------
## 01_functions_cusum.R
## Core CUSUM functions for WS5.WP1.2
## Applied to weekly rumor counts by spatial analysis unit
## -----------------------------------------------------------

library(dplyr)
library(rlang)

## cusum_core:
##  - Implements a one-sided (upper) CUSUM for count data.
##  - y:  vector of observed counts (e.g., weekly rumor counts).
##  - mu: vector of expected (baseline) counts, same length as y.
##  - k:  reference value (roughly half the shift, in std. units).
##  - h:  decision threshold (when S_t >= h, we raise an alarm).
##  - trans: transformation used to approximate normality.
##  - reset: if TRUE, reset CUSUM to 0 after an alarm.
cusum_core <- function(y, mu,
                       k = 1.04,
                       h = 2.26,
                       trans = c("standard", "anscombe"),
                       reset = FALSE) {
  
  trans <- match.arg(trans)
  
  if (length(y) != length(mu)) {
    stop("y and mu must have the same length.")
  }
  
  n <- length(y)
  S <- numeric(n)   # CUSUM process S_t
  alarm <- logical(n)
  z <- numeric(n)   # standardized scores
  
  for (t in seq_len(n)) {
    
    ## 1) Standardized residual or transformed score
    if (trans == "standard") {
      # Simple standardization for Poisson-like counts:
      #   z_t = (Y_t - mu_t) / sqrt(mu_t)
      z[t] <- (y[t] - mu[t]) / sqrt(mu[t])
      
    } else if (trans == "anscombe") {
      # Anscombe transformation for Poisson-like counts:
      #   A(Y) = 2 * sqrt(Y + 3/8)
      z[t] <- 2 * sqrt(y[t] + 3/8) - 2 * sqrt(mu[t] + 3/8)
    }
    
    ## 2) CUSUM increment:
    ##    x_t = z_t - k
    incr <- z[t] - k
    
    ## 3) One-sided CUSUM for increases:
    ##    S_t = max(0, S_{t-1} + x_t)
    if (t == 1) {
      S[t] <- max(0, incr)
    } else {
      S[t] <- max(0, S[t - 1] + incr)
    }
    
    ## 4) Alarm rule:
    ##    alarm_t = TRUE if S_t >= h
    if (S[t] >= h) {
      alarm[t] <- TRUE
      if (reset) {
        S[t] <- 0
      }
    }
  }
  
  list(
    cusum = S,
    alarm = alarm,
    z = z
  )
}



## run_cusum_for_unit:
##  - Runs the CUSUM pipeline for a *single* analysis unit.
##  - Assumes df_unit is already filtered to one analysis_unit_id.
##  - Assumes data are sorted chronologically (by year/week or time_index).
##
## Required columns in df_unit:
##  - time_index: numeric time index within the unit
##  - n_cases:    weekly rumor counts
##
## Returns:
##  - df_unit with additional columns:
##      mu_hat, cusum, alarm, z
run_cusum_for_unit <- function(df_unit,
                               baseline_filter,
                               detect_filter,
                               k      = 1.04,
                               h      = 2.26,
                               trans  = "standard",
                               reset  = FALSE) {
  
  required_cols <- c("time_index", "n_cases")
  missing_cols <- setdiff(required_cols, names(df_unit))
  if (length(missing_cols) > 0) {
    stop("df_unit is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }
  
  # Extract variables
  y <- df_unit$n_cases
  t <- df_unit$time_index
  
  # Identify baseline and detection periods
  idx_base <- baseline_filter(df_unit)
  idx_det  <- detect_filter(df_unit)
  
  if (!any(idx_base)) stop("baseline_filter selects no rows for this unit.")
  if (!any(idx_det))  stop("detect_filter selects no rows for this unit.")
  
  ## 1) Baseline model for expected counts (mu_t)
  ##    Simple Poisson GLM with linear trend in time.
  ##    Can be extended (seasonality, covariates, etc.) if needed.
  fit <- glm(
    y ~ t,
    family = poisson(),
    subset = idx_base
  )
  
  mu_hat <- as.numeric(predict(fit, type = "response"))
  
  ## 2) Apply CUSUM to the detection period for this unit
  res <- cusum_core(
    y   = y[idx_det],
    mu  = mu_hat[idx_det],
    k   = k,
    h   = h,
    trans = trans,
    reset = reset
  )
  
  ## 3) Attach results back to df_unit
  df_unit$mu_hat <- mu_hat
  df_unit$cusum  <- NA_real_
  df_unit$alarm  <- FALSE
  df_unit$z      <- NA_real_
  
  df_unit$cusum[idx_det] <- res$cusum
  df_unit$alarm[idx_det] <- res$alarm
  df_unit$z[idx_det]     <- res$z
  
  df_unit
}



## run_cusum_all_units:
##  - Applies CUSUM to all analysis units in the dataset.
##  - unit_var: column name that identifies the analysis unit
##    (e.g. "analysis_unit_id", which may be country/province/etc.).
##
## Required columns in df:
##  - unit_var (e.g. analysis_unit_id)
##  - time_index
##  - n_cases
run_cusum_all_units <- function(df,
                                unit_var        = "analysis_unit_id",
                                baseline_filter,
                                detect_filter,
                                k      = 1.04,
                                h      = 2.26,
                                trans  = "standard",
                                reset  = FALSE) {
  
  unit_sym <- rlang::sym(unit_var)
  
  df %>%
    dplyr::group_by(!!unit_sym) %>%
    dplyr::group_modify(
      ~ run_cusum_for_unit(
        df_unit         = .x,
        baseline_filter = baseline_filter,
        detect_filter   = detect_filter,
        k      = k,
        h      = h,
        trans  = trans,
        reset  = reset
      )
    ) %>%
    dplyr::ungroup()
}
