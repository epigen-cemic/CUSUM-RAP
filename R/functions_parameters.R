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
  res <- try(uniroot(f, lower = 0.01, upper = 30), silent = TRUE)
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
  res <- try(uniroot(f, lower = 0.01, upper = 10), silent = TRUE)
  if (inherits(res, "try-error")) return(NA)
  
  return(round(res$root, 3))
}