library(dplyr)
library(tidyr)

#' @title Epidemiological Data Simulator for CUSUM Testing
#' @description Generates a multi-year dataset with realistic baseline rates, 
#' random gaps, and injected outbreaks to test detection sensitivity.
generate_cusum_test_data <- function() {
  
  # 1. Geographic Setup
  provinces <- c("Catamarca", "Tucumán")
  departments <- list(
    "Catamarca" = c("Capital", "Ambato", "Belén", "Capayán", "Santa María"),
    "Tucumán"   = c("Capital", "Yerba Buena", "Tafí del Valle", "Lules", "Monteros")
  )
  
  years <- 2023:2026
  weeks <- 1:52
  
  df_list <- list()
  
  # 2. Hierarchy and Case Generation Loop
  for (prov in provinces) {
    for (dept in departments[[prov]]) {
      
      # Assign a random population to the Department
      dept_pop <- sample(50000:200000, 1)
      subdivisions <- sprintf("%03d", sample(1:100, 3)) # 3 subdivisions per dept
      
      # Split population across subdivisions
      pop_weights <- runif(3); pop_weights <- pop_weights / sum(pop_weights)
      sub_pops <- round(dept_pop * pop_weights)
      
      for (i in 1:3) {
        # 1. Create the timeline
        sub_df <- expand.grid(
          country = "Argentina",
          level1 = prov,
          level2 = dept,
          level3 = subdivisions[i],
          year = years,
          week = weeks,
          stringsAsFactors = FALSE
        )
        
        # If the year is 2026, only keep weeks 1 through 8
        sub_df <- sub_df %>%
          dplyr::filter(!(year == 2026 & week > 8))
        
        sub_df$pop_est <- sub_pops[i]
        
        # --- BASELINE: 1 per 10,000 rate ---
        current_lambda <- sub_pops[i] / 10000
        sub_df$n_cases <- rpois(nrow(sub_df), lambda = current_lambda)
        
        # --- OUTBREAK INJECTION: Sustained Shift ---
        # We inject one outbreak per subdivision in 2025 to test CUSUM accumulation
        start_week <- sample(15:30, 1)
        multipliers <- c(1.5, 2.5, 4.0, 6.0, 8.0) # Growing transmission
        
        for (j in seq_along(multipliers)) {
          target_row <- which(sub_df$year == 2025 & sub_df$week == (start_week + j - 1))
          if(length(target_row) > 0) {
            # Replace baseline with higher-rate Poisson value
            sub_df$n_cases[target_row] <- rpois(1, lambda = current_lambda * multipliers[j])
          }
        }
        
        df_list[[length(df_list) + 1]] <- sub_df
      }
    }
  }
  
  # 3. Consolidate and Create Gaps
  full_df <- dplyr::bind_rows(df_list)
  
  # Randomly delete 35% of the rows to create "Missing Weeks" for the gap-filler
  full_df <- full_df %>% 
    dplyr::slice_sample(prop = 0.65) %>%
    dplyr::arrange(level1, level2, level3, year, week)
  
  return(full_df)
}

# 4. Generate and Save the CSV
set.seed(42) # For reproducibility
final_test_data <- generate_cusum_test_data()
write.csv(final_test_data, "CUSUM_Stress_Test_2026.csv", row.names = FALSE)

cat("Success: Generated", nrow(final_test_data), "rows for testing.\n")