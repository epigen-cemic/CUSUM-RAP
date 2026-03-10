## -----------------------------------------------------------
## functions_parameters.R
## Calculators for CUSUM parameters (ARL0, h, k)
## -----------------------------------------------------------

#' @title Recommend Decision Threshold (h)
#' 
#' @description 
#' Calculates the recommended decision threshold (h) given a target Average 
#' Run Length (ARL0) and a reference value (k). This is an internal helper 
#' function used to generate UI suggestions.
#'
#' @details 
#' The function solves for 'h' using the standard continuous approximation 
#' formula for the ARL of a CUSUM chart:
#' ARL0 = (exp(2 * k * h) - 1 - 2 * k * h) / (2 * k^2)
#' 
#' It uses `uniroot` to find the root of the equation within a practical 
#' search interval [0.01, 30]. If the inputs are invalid or the root cannot 
#' be found, it safely returns `NA`.
#'
#' @param arl0 Numeric. The target Average Run Length (in-control). Must be > 1.
#' @param k Numeric. The reference value (slack parameter). Must be > 0.
#'
#' @return A numeric value representing the recommended threshold 'h', 
#'         rounded to 3 decimal places. Returns \code{NA} on failure or 
#'         invalid input.
#'         
#' @keywords internal
#' @noRd
recommend_h <- function(arl0, k) {
  if (is.na(arl0) || is.na(k) || arl0 <= 1 || k <= 0) return(NA)
  
  f <- function(h) {
    (exp(2 * k * h) - 1 - 2 * k * h) / (2 * k^2) - arl0
  }
  
  # Search for h between 0.01 and 30
  res <- try(uniroot(f, lower = 0.001, upper = 30, extendInt = "yes"), silent = TRUE)
  if (inherits(res, "try-error")) return(NA)
  
  return(round(res$root, 3))
}


#' @title Recommend Reference Value (k)
#' 
#' @description 
#' Calculates the recommended reference value (k) given a target Average 
#' Run Length (ARL0) and a decision threshold (h). This is an internal helper 
#' function used to generate UI suggestions.
#'
#' @details 
#' The function solves for 'k' using the standard continuous approximation 
#' formula for the ARL of a CUSUM chart:
#' ARL0 = (exp(2 * k * h) - 1 - 2 * k * h) / (2 * k^2)
#' 
#' It uses `uniroot` to find the root of the equation within a practical 
#' search interval [0.01, 10]. If the inputs are invalid or the root cannot 
#' be found, it safely returns `NA`.
#'
#' @param arl0 Numeric. The target Average Run Length (in-control). Must be > 1.
#' @param h Numeric. The decision threshold (alarm limit). Must be > 0.
#'
#' @return A numeric value representing the recommended reference value 'k', 
#'         rounded to 3 decimal places. Returns \code{NA} on failure or 
#'         invalid input.
#'         
#' @keywords internal
#' @noRd
recommend_k <- function(arl0, h) {
  if (is.na(arl0) || is.na(h) || arl0 <= 1 || h <= 0) return(NA)
  
  f <- function(k) {
    (exp(2 * k * h) - 1 - 2 * k * h) / (2 * k^2) - arl0
  }
  
  # Search for k between 0.01 and 10
  res <- try(uniroot(f, lower = 0.001, upper = 10, extendInt = "yes"), silent = TRUE)
  if (inherits(res, "try-error")) return(NA)
  
  return(round(res$root, 3))
}



#' @title Calculate Phase 1 Training Baseline
#' 
#' @description 
#' Isolates the first 25% of the requested analysis window to establish a clean, 
#' pre-outbreak expected baseline. This acts as a "Phase I" statistical control 
#' period. It attempts a Poisson GLM first, but safely falls back to a 
#' mathematical average if the data lacks sufficient variance.
#'
#' @param df Data frame. The dataset containing the timeline and case counts. 
#'        Must include \code{time_index} and \code{n_cases} columns.
#' @param window_size Numeric. The total number of weeks the user selected for 
#'        the full analysis period.
#'
#' @return A single numeric value representing the safely calculated expected 
#'         baseline count (\code{mu}).
#'         
#' @keywords internal
#' @noRd
get_phase1_baseline <- function(df, window_size) {
  
  # 1. Figure out the timeline
  max_week <- max(df$time_index, na.rm = TRUE)
  start_week <- max(0, max_week - window_size)
  
  # Calculate the 25% cutoff mark
  baseline_cutoff <- start_week + (0.25 * window_size)
  
  # 2. Isolate Phase 1 (The first 25% of the data)
  phase1_data <- df %>% 
    dplyr::filter(time_index > start_week & time_index <= baseline_cutoff)
  
  # 3. Calculate the Expected Baseline safely
  calculated_mu <- tryCatch({
    # Attempt a basic Poisson GLM using the n_cases column
    model <- glm(n_cases ~ 1, family = poisson(link = "log"), data = phase1_data)
    exp(coef(model)[[1]]) 
  }, error = function(e) {
    # If the GLM panics (e.g., zero variance), fall back to the mathematical mean
    mean(phase1_data$n_cases, na.rm = TRUE)
  })
  
  # 4. Final Safety Net: If the result is still NA or NaN, use the whole dataset average
  if (is.na(calculated_mu) || is.nan(calculated_mu)) {
    calculated_mu <- mean(df$n_cases, na.rm = TRUE)
  }
  
  return(calculated_mu)
}