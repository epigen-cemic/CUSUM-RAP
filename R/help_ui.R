# =============================================================================
# CUSUM Help UI
# =============================================================================

#' CUSUM help tab UI
#'
#' @description
#' Builds the Help tab for the standalone CUSUM RAP tool. It describes the tool purpose, required input columns, accepted aliases, documentation links, and prepared-data log.
#'
#' @return A Shiny `tagList` containing Help tab content.
#' @keywords internal
cusum_help_tab <- function(ns = identity) {
  tagList(
    div(
      class = "help-card",
      h3("CUSUM RAP Help"),
      p(
        "The CUSUM RAP detects unusual increases in case counts over time. ",
        "Population is optional for the standard count-based CUSUM workflow; ",
        "it is only needed if a future population-adjusted expected-frequency model is used."
      ),
      tags$ul(
        tags$li(tags$a(href = "docs/User_Manual_EN.pdf", target = "_blank", "Open user manual")),
        tags$li(tags$a(href = "docs/User_Manual_ES.pdf", target = "_blank", "Open spanish user manual"))
      )
    ),

    div(
      class = "help-card",
      h3("Display settings"),
      p("Use this setting to increase the interface and plot text size. Larger options also increase plot height so the plotting area does not become compressed."),
      tags$label(class = "control-label", "Display Font Size"),
      selectInput(
        ns("font_size"),
        label = NULL,
        choices = c(
          "Small (current)" = "small",
          "Medium" = "medium",
          "Large" = "large",
          "Extra large" = "xlarge"
        ),
        selected = "small",
        width = "280px"
      ),
      tags$label(class = "control-label", "Decimal places shown in result tables"),
      selectInput(
        ns("display_digits"),
        label = NULL,
        choices = setNames(as.character(0:8), as.character(0:8)),
        selected = "4",
        width = "180px"
      ),
      p(class = "help-note", "This only changes how numbers are displayed in the app. Downloaded CSV files keep the full numeric values.")
    ),

    div(
      class = "help-card",
      h3("Required data structure"),
      p("The uploaded file should contain one row per geographic unit and epidemiological week."),
      tags$table(
        class = "requirements-table",
        tags$thead(
          tags$tr(
            tags$th("Column"),
            tags$th("Required"),
            tags$th("Description"),
            tags$th("Accepted aliases / examples")
          )
        ),
        tags$tbody(
          tags$tr(tags$td("year"), tags$td("Yes"), tags$td("Epidemiological year."), tags$td("year, anio, año, epiyear; e.g. 2026")),
          tags$tr(tags$td("week"), tags$td("Yes"), tags$td("Epidemiological week."), tags$td("week, semana, epiweek, se; e.g. 8")),
          tags$tr(tags$td("country"), tags$td("Recommended"), tags$td("Country or national unit."), tags$td("Argentina")),
          tags$tr(tags$td("level1"), tags$td("Depends on selected level"), tags$td("First administrative level."), tags$td("province, region, Level1")),
          tags$tr(tags$td("level2"), tags$td("Depends on selected level"), tags$td("Second administrative level."), tags$td("department, district, LAD, Level2")),
          tags$tr(tags$td("level3"), tags$td("Depends on selected level"), tags$td("Third administrative level."), tags$td("fraction, MSOA, Level3")),
          tags$tr(tags$td("n_cases"), tags$td("Yes"), tags$td("Observed cases or event count."), tags$td("n_cases, cases, case_count, count, n")),
          tags$tr(tags$td("population"), tags$td("Optional"), tags$td("Not required for standard CUSUM."), tags$td("population, pop, denominator"))
        )
      )
    ),

    div(
      class = "help-card",
      h3("Prepared data and change log"),
      p(
        "The Prepared Data tab shows the cleaned, aggregated, gap-filled dataset used for analysis. ",
        "It also includes a log describing changes made during import and preparation, such as ",
        "column standardisation, type conversion, overlap resolution, filtering, and weekly gap filling."
      ),
      p(
        "Missing week/location combinations are filled with 0 cases only in the prepared dataset; ",
        "the original uploaded CSV is not modified. The tool also reports observed-week coverage ",
        "and stops CUSUM when too much of the time series would need to be inferred as zero-case weeks."
      )
    )
  )
}
