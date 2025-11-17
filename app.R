library(shinydashboard)
library(shiny)
library(shinyjs)
library(rintrojs)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(data.table)
library(dplyr)
library(plotly)


source("config.R")
source("ui/navbar.R")
source("ui/sidebar.R")
source("ui/fluid_design.R")
source("modules/lib_reparto.R")  # ¡IMPORTANTE! Cargar lib_reparto antes
source("server/dynamic_server.R")



ui <- dashboardPage(
  skin = DASHBOARD_CONFIG$skin,
  title = DASHBOARD_CONFIG$title,

  dashboardHeader(
    title = span(img(src = "sankey.svg", height = 35), "Reparto Costes"),
    titleWidth = 300,
    dropdownMenu(
      type = "notifications",
      headerText = strong("Ayuda"),
      icon = icon("question"),
      badgeStatus = NULL,
      notificationItem(
        text = "Esto es una prueba",
        icon = icon("info")
      )
    ),
    tags$li(
      a(
        strong("ABOUT"),
        height = 40,
        href = "https://github.com/pablo-mu",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),

  create_dashboard_sidebar(DASHBOARD_CONFIG),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "radar.css"),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"
      ),
      tags$style(HTML("
        /* Ajustes compactos para KPIs */
        .info-box {
          padding: 0px 0px;
          min-height: 46px;
        }
        .info-box .info-box-icon {
          width: 46px;
          height: 46px;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 18px; /* icon size */
          margin-right: 1px; /* <- reducir separación */
          border-radius: 1px;
        }
        .info-box .info-box-content {
          padding: 4px 6px;
          margin-left: 0; /* asegurar que no haya margen extra */
          display: flex;
          flex-direction: column;
          justify-content: center;
        }
        .info-box .info-box-text {
          font-size: 12px;
          line-height: 1;
          margin: 0 0 2px 0;
        }
        .info-box .info-box-number {
          font-size: 16px;
          font-weight: 600;
          margin: 0;
        }
      ")),
    ),
    useShinyjs(),
    introjsUI(),

    create_bar(PANEL_DEFINITIONS),
    
    # Paneles dinámicos usando fluid_design
    fluid_design("sankey_panel", "sankey_content"),
    fluid_design("matriz_costes_panel", "matriz_costes_content"),
    fluid_design("diagnostics_panel", "diagnostics_content")
  )
)

server <- function(input, output, session){
    observe({
    introjs(session)
  })

  # Create dynamic server components  
  server_components <- create_dynamic_server(input, output, session, PANEL_DEFINITIONS, DEFAULT_PANEL)
  
  # Extract components for use in modules
  active_panel <- server_components$active_panel
  datos_raw <- server_components$datos_raw
  filters <- server_components$filters
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)


