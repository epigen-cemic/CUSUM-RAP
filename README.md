# CUSUM RAP Tool (WS5.WP1.2)

The CUSUM RAP (Reproducible Analytical Pipeline) Tool is a specialized Shiny application representing WS5.WP1.2 Product 1. It implements a CUSUM-based early detection algorithm on weekly case counts per spatial unit.

## Ecosystem Workflow
This tool is the final analytical step in the ecosystem and is designed to run sequentially:
1. **Data Collection:** Individual-level suspected case data is collected via RapidCase Report.
2. **Aggregation:** Data is filtered and aggregated by week and spatial unit using API-POP.
3. **Analysis:** The aggregated CSV output from API-POP is used as the direct input for this CUSUM RAP Tool.

## Deployment & Setup
Because this application relies on aggregated CSV uploads rather than live database connections, it does not require complex environment credentials. There are no `.Renviron` or `config.yaml` files to configure.

To distribute this tool for local use, you can package it directly into a standalone executable. Utilize RInno to compile the installer for this application, providing a straightforward deployment method for end-users. Alternatively, it can be deployed to the web via shinyapps.io or a custom Shiny Server.

## Documentation
For detailed usage, architecture, and configuration instructions, please refer to the developer guides located in the docs folder:

* 🇬🇧 [English Developer & Admin Guide](www/docs/HELP_EN.md)
* 🇪🇸 [Guía de Desarrollo y Administración en Español](www/docs/HELP_ES.md)

*(Note: End-users looking for the analytical interface manual can find the PDF guide inside the running application by clicking the "Help" button).*