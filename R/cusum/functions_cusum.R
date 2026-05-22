#' @title Core CUSUM Calculation
#'
#' @description
#' Implements a one-sided (upper) CUSUM algorithm for count data. It compares
#' observed counts (\code{y}) against expected baseline counts (\code{mu}) to detect
#' anomalous increases.
#'
#' @details
#' The function calculates a standardized score (\code{z}) for each time point using
#' either a standard Poisson approximation or an Anscombe transformation. It then
#' computes the cumulative sum (\code{S}) of deviations above a reference value (\code{k}).
#' An alarm is raised if \code{S} exceeds the decision threshold (\code{h}).
#'
#' @param y Numeric vector. Observed counts (e.g., weekly rumor counts).
#' @param mu Numeric vector. Expected (baseline) counts. Must have the same length as \code{y}.
#' @param k Numeric. Reference value, representing roughly half the shift size in standard units.
#'          Defaults to 1.04.
#' @param h Numeric. Decision threshold. When the CUSUM process \code{S_t >= h}, an alarm is raised.
#'          Defaults to 2.26.
#' @param trans Character string. Transformation used to approximate normality.
#'              Options are \code{"standard"} (default) or \code{"anscombe"}.
#' @param reset Logical. If \code{TRUE}, the CUSUM statistic \code{S_t} is reset to 0
#'              immediately after an alarm is triggered. Defaults to \code{FALSE}.
#'
#' @return A list containing three vectors of length \code{n}:
#' \item{cusum}{The CUSUM process values (\code{S_t}).}
#' \item{alarm}{Logical vector indicating if an alarm was raised at time \code{t}.}
#' \item{z}{The standardized scores (residuals) at time \code{t}.}
#'
#' @export
cusum_core <- function(y, mu,
                       k,       # No default: User MUST provide this
                       h,       # No default: User MUST provide this
                       trans = c("standard", "anscombe"),
                       reset = FALSE) {
  
  trans <- match.arg(trans)
  
  if (length(y) != length(mu)) {
    stop("y and mu must have the same length.")
  }
  
  n <- length(y)
  S <- numeric(n)
  alarm <- logical(n)
  z <- numeric(n)
  
  current_S <- 0  # temporary tracker
  
  for (t in seq_len(n)) {
    
    ## 1) Standardized residual
    if (trans == "standard") {
      denom <- sqrt(mu[t])
      if (denom < 1e-9) denom <- 1e-9 
      z[t] <- (y[t] - mu[t]) / denom
    } else if (trans == "anscombe") {
      z[t] <- 2 * sqrt(y[t] + 3/8) - 2 * sqrt(mu[t] + 3/8)
    }
    
    ## 2) CUSUM increment
    incr <- z[t] - k
    
    ## 3) One-sided CUSUM
    current_S <- max(0, current_S + incr)
    S[t] <- current_S
    
    ## 4) Alarm rule:
    if (current_S >= h) {     # Check current_S instead of S[t]
      alarm[t] <- TRUE
      if (reset) {
        current_S <- 0        # Reset the tracker, leaving S[t] as the peak value!
      }
    }
  }
  
  list(cusum = S, alarm = alarm, z = z)
}


#' @title Run CUSUM for a Single Analysis Unit
#'
#' @description
#' Runs the complete CUSUM pipeline for a single analysis unit (e.g., one province).
#' This involves:
#' \enumerate{
#'   \item Identifying baseline vs. detection periods using the provided filter functions.
#'   \item Fitting a Poisson GLM (linear trend) to the baseline data to estimate expected counts (\code{mu}).
#'   \item Applying the \code{cusum_core} function to the detection period.
#' }
#'
#' @param df_unit Data frame. Contains data for a single unit. Must be sorted chronologically.
#'                Required columns: \code{time_index}, \code{n_cases}.
#' @param baseline_filter Function. A function that takes \code{df_unit} as input and returns a
#'                        logical vector indicating which rows belong to the baseline period.
#' @param detect_filter Function. A function that takes \code{df_unit} as input and returns a
#'                      logical vector indicating which rows belong to the detection period.
#' @param k Numeric. Reference value for CUSUM. Defaults to 1.04.
#' @param h Numeric. Decision threshold for CUSUM. Defaults to 2.26.
#' @param trans Character string. Transformation method (\code{"standard"} or \code{"anscombe"}).
#' @param reset Logical. Whether to reset CUSUM after an alarm. Defaults to \code{FALSE}.
#' @param fixed_mu Numeric (Optional). If provided, this fixed value is used as the expected count 
#'                 for all time points, bypassing the GLM baseline calculation.
#'
#' @return The input \code{df_unit} with additional columns:
#' \item{mu_hat}{Estimated expected counts.}
#' \item{cusum}{CUSUM process values.}
#' \item{alarm}{Logical indicating if threshold was exceeded.}
#' \item{z}{Standardized scores.}
#'
#' @importFrom stats glm poisson predict
#' @export
run_cusum_for_unit <- function(df_unit,
                               baseline_filter,
                               detect_filter,
                               k,       # Mandatory
                               h,       # Mandatory
                               trans  = "standard",
                               reset  = FALSE,
                               fixed_mu = NULL) {
  
  required_cols <- c("time_index", "n_cases")
  missing_cols <- setdiff(required_cols, names(df_unit))
  if (length(missing_cols) > 0) {
    stop("df_unit is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  y <- df_unit$n_cases
  t <- df_unit$time_index
  
  idx_base <- baseline_filter(df_unit)
  idx_det  <- detect_filter(df_unit)
  
  # Manual vs Automatic Baseline Logic
  if (!is.null(fixed_mu)) {
    mu_hat <- rep(as.numeric(fixed_mu), length(y))
  } else {
    if (!any(idx_base)) stop("baseline_filter selects no rows for this unit (and no fixed_mu provided).")
    fit <- glm(y ~ t, family = poisson(), subset = idx_base)
    mu_hat <- as.numeric(predict(fit, type = "response"))
  }
  
  if (!any(idx_det))  stop("detect_filter selects no rows for this unit.")
  
  # Pass k and h explicitly
  res <- cusum_core(
    y   = y[idx_det],
    mu  = mu_hat[idx_det],
    k   = k,
    h   = h,
    trans = trans,
    reset = reset
  )
  
  df_unit$mu_hat <- mu_hat
  df_unit$cusum  <- NA_real_
  df_unit$alarm  <- FALSE
  df_unit$z      <- NA_real_
  
  df_unit$cusum[idx_det] <- res$cusum
  df_unit$alarm[idx_det] <- res$alarm
  df_unit$z[idx_det]     <- res$z
  
  df_unit
}



#' @title Run CUSUM on All Units
#'
#' @description
#' A wrapper function that applies the CUSUM pipeline to all analysis units in the dataset.
#' It groups the data by \code{unit_var} and applies \code{run_cusum_for_unit} to each group.
#'
#' @param df Data frame. The full dataset containing multiple analysis units.
#'           Required columns: \code{unit_var}, \code{time_index}, \code{n_cases}.
#' @param unit_var Character string. The column name identifying the analysis unit
#'                 (e.g., \code{"analysis_unit_id"}).
#' @param baseline_filter Function. Passed to \code{run_cusum_for_unit}. Defines baseline rows.
#' @param detect_filter Function. Passed to \code{run_cusum_for_unit}. Defines detection rows.
#' @param k Numeric. Reference value. Defaults to 1.04.
#' @param h Numeric. Decision threshold. Defaults to 2.26.
#' @param trans Character string. Transformation method.
#' @param reset Logical. Whether to reset CUSUM after an alarm.
#' @param fixed_mu Numeric (Optional). If provided, forces a manual baseline.
#'
#' @return A tibble containing the original data with appended CUSUM results,
#'         ungrouped.
#'
#' @import dplyr
#' @importFrom rlang sym
#' @export
run_cusum_all_units <- function(df,
                                unit_var        = "analysis_unit_id",
                                baseline_filter,
                                detect_filter,
                                k,       # Mandatory
                                h,       # Mandatory
                                trans  = "standard",
                                reset  = FALSE,
                                fixed_mu = NULL) {
  
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
        reset  = reset,
        fixed_mu = fixed_mu
      )
    ) %>%
    dplyr::ungroup()
}