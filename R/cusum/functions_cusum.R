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


#' Resolve a scalar or named unit-level parameter for one CUSUM unit.
#'
#' @param value Numeric scalar or named numeric vector.
#' @param df_unit Data frame for one analysis unit.
#' @param unit_var Character. Analysis-unit identifier column.
#'
#' @return Numeric scalar.
#' @keywords internal
#' Resolve a scalar or named unit-level parameter for one CUSUM unit.
#'
#' @param value Numeric scalar or named numeric vector.
#' @param unit_id Analysis-unit identifier value.
#'
#' @return Numeric scalar.
#' @keywords internal
resolve_unit_parameter <- function(value, unit_id = NULL) {
  if (is.null(value)) {
    return(NULL)
  }
  
  value_names <- names(value)
  value <- as.numeric(value) |> stats::setNames(value_names)
  
  if (length(value) == 1 || is.null(names(value)) || all(names(value) == "")) {
    return(as.numeric(value[1]))
  }
  
  unit_id <- as.character(unit_id)
  
  if (!unit_id %in% names(value)) {
    stop("No unit-level parameter value was found for analysis unit: ", unit_id)
  }
  
  as.numeric(value[[unit_id]])
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
    unit_id <- if ("analysis_unit_id" %in% names(df_unit)) {
      as.character(df_unit$analysis_unit_id[[1]])
    } else {
      NULL
    }
    
    unit_mu <- resolve_unit_parameter(fixed_mu, unit_id = unit_id)
    mu_hat <- rep(unit_mu, length(y))
  } else {
    if (!any(idx_base)) stop("baseline_filter selects no rows for this unit (and no fixed_mu provided).")
    fit <- glm(y ~ t, family = poisson(), subset = idx_base)
    mu_hat <- as.numeric(predict(fit, type = "response"))
  }
  
  if (!any(idx_det))  stop("detect_filter selects no rows for this unit.")
  
  # Pass k and h explicitly
  unit_id <- if ("analysis_unit_id" %in% names(df_unit)) {
    as.character(df_unit$analysis_unit_id[[1]])
  } else {
    NULL
  }
  
  unit_k <- resolve_unit_parameter(k, unit_id = unit_id)

  res <- cusum_core(
    y   = y[idx_det],
    mu  = mu_hat[idx_det],
    k   = unit_k,
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
                                unit_var = "analysis_unit_id",
                                baseline_filter,
                                detect_filter,
                                k,
                                h,
                                trans = "standard",
                                reset = FALSE,
                                fixed_mu = NULL) {
  
  df %>%
    dplyr::group_by(.data[[unit_var]]) %>%
    dplyr::group_modify(function(.x, .y) {
      unit_id <- as.character(.y[[unit_var]][[1]])
      
      .x[[unit_var]] <- unit_id
      
      result <- run_cusum_for_unit(
        df_unit = .x,
        baseline_filter = baseline_filter,
        detect_filter = detect_filter,
        k = resolve_unit_parameter(k, unit_id = unit_id),
        h = h,
        trans = trans,
        reset = reset,
        fixed_mu = if (!is.null(fixed_mu)) {
          resolve_unit_parameter(fixed_mu, unit_id = unit_id)
        } else {
          NULL
        }
      )
      
      dplyr::select(result, -dplyr::any_of(unit_var))
    }) %>%
    dplyr::ungroup()
}
#' @title Format CUSUM Weekly Results for User-Facing Tables
#'
#' @description
#' Converts internal CUSUM result columns into clearer output labels. The
#' internal `mu_hat` value is shown as expected weekly cases and, when
#' population is available, as expected rate per 100,000 people.
#'
#' @param df Data frame returned by `run_cusum_all_units()`.
#'
#' @return Data frame with user-facing columns.
#' @export
format_cusum_weekly_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  out <- df

  if ("epi_date" %in% names(out)) {
    out$epi_date <- format(as.Date(out$epi_date), "%G-W%V")
  }

  if (all(c("n_cases", "population") %in% names(out))) {
    out$observed_rate_per_100k <- ifelse(
      !is.na(out$population) & out$population > 0,
      (out$n_cases / out$population) * 100000,
      NA_real_
    )
  }

  if (all(c("mu_hat", "population") %in% names(out))) {
    out$expected_rate_per_100k <- ifelse(
      !is.na(out$population) & out$population > 0,
      (out$mu_hat / out$population) * 100000,
      NA_real_
    )
  }

  cols <- c(
    intersect(c("country", "level1", "level2", "level3", "level4"), names(out)),
    intersect(c("analysis_unit_id", "year", "week", "epi_date"), names(out)),
    intersect(c("n_cases", "mu_hat", "population", "observed_rate_per_100k", "expected_rate_per_100k", "cusum", "alarm", "z", "k_value"), names(out))
  )

  out <- out[, unique(cols), drop = FALSE]

  names(out) <- dplyr::recode(
    names(out),
    analysis_unit_id = "Analysis unit",
    year = "Year",
    week = "Week",
    epi_date = "Epi week",
    n_cases = "Observed cases",
    mu_hat = "Expected weekly cases (mu)",
    population = "Population",
    observed_rate_per_100k = "Observed Rate",
    expected_rate_per_100k = "Expected Rate",
    cusum = "CUSUM score",
    alarm = "Alarm",
    z = "Standardized residual",
    k_value = "reference value k"
  )

  out
}


#' @title Format Unit-Level CUSUM Reference Table
#'
#' @description
#' Creates one reference row per analysis unit, avoiding repeated baseline
#' values in the main interpretation table.
#'
#' @param df Data frame returned by `run_cusum_all_units()`.
#'
#' @return Data frame with one row per analysis unit.
#' @export
format_cusum_reference_table <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"analysis_unit_id" %in% names(df)) {
    return(data.frame())
  }

  geo_cols <- intersect(c("country", "level1", "level2", "level3", "level4"), names(df))

  ref <- df %>%
    dplyr::group_by(.data$analysis_unit_id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(geo_cols), ~ dplyr::first(.x)),
      expected_weekly_cases = dplyr::first(.data$mu_hat),
      population = if ("population" %in% names(df)) mean(.data$population, na.rm = TRUE) else NA_real_,
      expected_rate_per_100k = if ("population" %in% names(df)) {
        pop <- mean(.data$population, na.rm = TRUE)
        ifelse(!is.na(pop) && pop > 0, (dplyr::first(.data$mu_hat) / pop) * 100000, NA_real_)
      } else {
        NA_real_
      },
      k_reference_value = if ("k_value" %in% names(df)) dplyr::first(.data$k_value) else NA_real_,
      alarm_weeks = sum(.data$alarm, na.rm = TRUE),
      .groups = "drop"
    )

  names(ref) <- dplyr::recode(
    names(ref),
    analysis_unit_id = "Analysis unit",
    expected_weekly_cases = "Expected weekly cases",
    population = "Population",
    expected_rate_per_100k = "Expected Rate",
    k_reference_value = "reference value k",
    alarm_weeks = "Alarm weeks"
  )

  ref
}
