library(shinydashboard)
library(shiny)
library(shinyjs)
library(rintrojs)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(DT)


source("config.R")
source("ui/navbar.R")
source("ui/sidebar.R")
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
        href = "custom_health.css"),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap"
      )
    ),
    useShinyjs(),
    introjsUI(),

    create_bar(PANEL_DEFINITIONS)
  )
)

server <- function(input, output, session){
    observe({
    introjs(session)
  })

  # Create dynamic server components
  active_panel <- create_dynamic_server(input, output, session, PANEL_DEFINITIONS, DEFAULT_PANEL)
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)


