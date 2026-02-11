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