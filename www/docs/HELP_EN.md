# CUSUM RAP Tool: Developer & Administrator Guide

This guide is intended for developers, IT administrators, and maintainers of the CUSUM RAP Tool. If you are an end-user looking for instructions on how to use the analytical interface, please refer to the **User Manual** located at `www/docs/Documentation.pdf` or click the "Help" button inside the running application.

## 1. Project Structure
The repository is structured as a standard Shiny application with modularized components.

* **Root Directory (`/`)**: Contains the main application scripts (`app.R`, `global.R`), project files (`CUSUM-RAP.Rproj`), and top-level configurations (`config.json`).
* **`R/` Directory**: Contains the core logic and Shiny modules. This includes `mod_cusum.R` (the main UI and Server module) and the backend scripts (`functions_cusum.R`, `functions_io.R`, `functions_parameters.R`, `functions_plot.R`).
* **`www/` Directory**: Contains static web assets exposed to the browser. This includes `config.css` (UI styling), the `docs/` folder (end-user documentation), and graphical assets (`AnalysisforAction_white.png`, `splash.png`, `default.ico`).

```text
/ (Root Directory)
├── app.R                        # Main application script
├── global.R                     # Configuration and global variables script
├── CUSUM-RAP.Rproj              # RStudio project file
├── config.json                  # Top-level configurations (e.g., spatial hierarchies)
│
├── R/                           # Core logic and Shiny modules
│   ├── mod_cusum.R              # Main module (UI and Server)
│   ├── functions_cusum.R        # Mathematical algorithm functions
│   ├── functions_io.R           # Data reading and saving functions
│   ├── functions_parameters.R   # Variable calculation functions
│   └── functions_plot.R         # Chart and plot generation functions
│
└── www/                         # Static web assets (exposed to the browser)
    ├── config.css               # Stylesheet (UI colors and layout)
    ├── AnalysisforAction_white.png # Graphic asset
    ├── splash.png               # Graphic asset
    ├── default.ico              # Browser tab icon
    │
    └── docs/                    # End-user documentation
        └── Documentation.pdf    # The manual that opens when clicking "Help"
```

## 2. Running the App Locally (Development)
To run or test the application within an R development environment:
1. Open the `CUSUM-RAP.Rproj` file in RStudio.
2. Open `app.R`.
3. Click **Run App** in the RStudio IDE, or run `shiny::runApp()` in the console.

## 3. Web Deployment 
To make the tool accessible via a web browser, you must host the application on a server. 

**Option A: shinyapps.io (Easiest Cloud Hosting)**
1. Create a free or paid account at [shinyapps.io](https://www.shinyapps.io/).
2. Install the deployment package in R by running `install.packages("rsconnect")`.
3. Authenticate your RStudio session using the secure token provided in your shinyapps.io dashboard.
4. Deploy the app by clicking the blue **Publish** button in the top right corner of your RStudio script editor, or by running `rsconnect::deployApp()` in the console.

**Option B: Self-Hosted Infrastructure**
If your epidemiological data requires strict privacy compliance that prevents the use of third-party cloud hosting, you can host the application on your own infrastructure (AWS, DigitalOcean, or an internal secure server). This requires installing the open-source **Shiny Server** on a Linux machine or containerizing the application using **Docker**.

## 4. App Configuration & Customization
Administrators can customize the application's appearance and geographical mapping without altering the core R logic.

* **Modifying the UI Design (`www/config.css`)**: The application's styling is hardcoded into this CSS file. Find and replace specific hex codes to change branding (e.g., `#4a4a4a` for dark gray backgrounds, `#616161` for standard text).
* **Adding Countries & Regions (`config.json`)**: The spatial hierarchy for the analytical output is defined here. Add a new array defining the administrative levels from highest to lowest (e.g., `"Brazil": { "levels": ["country", "state", "municipality"] }`).
