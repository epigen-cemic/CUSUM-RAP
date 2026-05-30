#' Register CUSUM module logic: downloads.R
#'
#' @param state Internal module environment created by `cusumServer()`.
#' @return Invisibly returns `state`.
cusum_server_downloads <- function(state) {
  evalq({
        output$download_series_plot <- downloadHandler(
          filename = function() {
            clean_name <- sub(".*\\|", "", selected_unit_id())
            paste0("CUSUM_Bar_plot_", clean_name, "_", Sys.Date(), ".png")
          },
          content = function(file) {
            ggplot2::ggsave(file, plot = series_plot_obj(), 
                            device = "png", width = download_plot_width_in(), height = download_plot_height_in(), dpi = 300)
          }
        )

        output$download_process_plot <- downloadHandler(
          filename = function() {
            clean_name <- sub(".*\\|", "", selected_unit_id())
            paste0("CUSUM_Trends_", clean_name, "_", Sys.Date(), ".png")
          },
          content = function(file) {
            ggplot2::ggsave(file, plot = process_plot_obj(), 
                            device = "png", width = download_plot_width_in(), height = download_plot_height_in(), dpi = 300)
          }
        )

        output$download_data <- downloadHandler(
          filename = function() {
            paste0("CUSUM_Results_", Sys.Date(), ".csv")
          },
          content = function(file) {
            df <- analyzed_data()

            if ("epi_date" %in% names(df)) {
              df$epi_date <- format(as.Date(df$epi_date), "%G-W%V")
            }

            write.csv(df, file, row.names = FALSE)
          }
        )

        output$download_prepared_data <- downloadHandler(
          filename = function() {
            paste0("CUSUM_Prepared_Data_", Sys.Date(), ".csv")
          },
          content = function(file) {
            df <- prepared_target_data()

            if ("epi_date" %in% names(df)) {
              df$epi_date <- format(as.Date(df$epi_date), "%G-W%V")
            }

            write.csv(df, file, row.names = FALSE)
          }
        )
  }, envir = state)
  invisible(state)
}
