# =============================================================================
# MODULAR RADAR DASHBOARD
# =============================================================================
# Este es el archivo principal que ensambla todos los módulos

# LIBRARIES
library(shinydashboard)
library(shiny)
library(shinyjs)
library(rintrojs)
library(shinyBS)
library(shinyWidgets)
library(DT)

# LOAD CONFIGURATION AND MODULES
source("config.R")
source("ui/header.R")
source("ui/sidebar.R") 
source("ui/main_buttons.R")
source("ui/fluid_design.R")
source("modules/antimicrobials.R")
source("modules/patients.R")
source("modules/diagnostics.R")
source("modules/outcome.R")
source("server/dynamic_server.R")
source("server/plots_data.R")

# =============================================================================
# UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  skin = DASHBOARD_CONFIG$skin,
  title = DASHBOARD_CONFIG$title,
  
  # Header
  create_dashboard_header(DASHBOARD_CONFIG),
  
  # Sidebar  
  create_dashboard_sidebar(DASHBOARD_CONFIG),
  
  # Body
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    useShinyjs(),
    introjsUI(),
    
    # Main buttons
    create_main_buttons(PANEL_DEFINITIONS),
    
    # Dynamic content area
    uiOutput("dynamic_content")
  )
)

# =============================================================================
# SERVER DEFINITION  
# =============================================================================

server <- function(input, output, session) {
  
  # Initialize introjs
  observe({
    introjs(session)
  })
  
  # Create dynamic server components
  active_panel <- create_dynamic_server(input, output, session, PANEL_DEFINITIONS, DEFAULT_PANEL)
  
  # Create plots and data outputs (pasar input para filtros reactivos)
  create_plots_server(output, input)
  create_tables_server(output)
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)