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
    title = introBox(
      span(img(src = "sankey.svg", height = 35), "Reparto Costes"),
      data.step = 1,
      data.intro = "<strong>¡Bienvenido al Dashboard de Reparto de Costes!</strong><br/>Esta aplicación te permite visualizar y analizar el flujo de costes entre centros de actividad (CACs). Usa este tour guiado para conocer todas las funcionalidades disponibles.<br/><br/>• Navega entre diferentes vistas<br/>• Aplica filtros personalizados<br/>• Genera reportes detallados"
    ),
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
        href = "custom_health2.css"),
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
        
        /* Estilos personalizados para intro.js */
        .introjs-skipbutton {
          color: #c8102e !important;
          font-weight: 600 !important;
          border: 2px solid #c8102e !important;
          background-color: white !important;
          border-radius: 4px !important;
          padding: 6px 14px !important;
          transition: all 0.3s ease !important;
          font-size: 13px !important;
        }
        .introjs-skipbutton:hover {
          background-color: #c8102e !important;
          color: white !important;
        }
      ")),
    ),
    useShinyjs(),
    introjsUI(),

    create_bar(PANEL_DEFINITIONS),
    
    # Paneles dinámicos usando fluid_design
    introBox(
      data.step = 6,
      data.intro = "<strong>Área Principal de Visualización</strong><br/>Aquí se mostrarán los gráficos y tablas generadas según tus filtros:<br/>• <strong>Diagramas interactivos</strong>: Haz zoom y explora los datos<br/>• <strong>Tablas detalladas</strong>: Exporta y analiza los datos en detalle<br/>• <strong>Vistas expandidas</strong>: Usa los botones de expansión para ver en pantalla completa<br/><br/><strong>¡Ya estás listo para comenzar!</strong> Carga tus datos y empieza a explorar.",
      fluid_design("sankey_panel", "sankey_content"),
      fluid_design("matriz_costes_panel", "matriz_costes_content"),
      fluid_design("diagnostics_panel", "diagnostics_content"),
      fluid_design("traza_panel", "traza_content")
    )
  )
)

server <- function(input, output, session){
  observe({
    introjs(session, options = list(
      "nextLabel" = "Siguiente",
      "prevLabel" = "Anterior",
      "skipLabel" = "Saltar Tour",
      "doneLabel" = "Finalizar",
      "showProgress" = TRUE,
      "showBullets" = FALSE,
      "showStepNumbers" = TRUE,
      "exitOnOverlayClick" = FALSE,
      "exitOnEsc" = TRUE,
      "hidePrev" = FALSE,
      "hideNext" = FALSE
    ))
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


