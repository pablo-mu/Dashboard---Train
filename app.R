library(shinydashboard)
library(shiny)
library(shinyjs)
library(rintrojs)
library(shinyBS)
library(shinyWidgets)
library(DT)


source("config.R")


ui <- dashboardPage(
  skin = DASHBOARD_CONFIG$skin,
  title = DASHBOARD_CONFIG$title,

  dashboardHeader(
    title = span(img(src = "Logo_GV_Sanidad_negro.png", height = 50), ""),
    titleWidth = 230,
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

  dashboardSidebar( # <-- Sidebar obligatorio
    sidebarMenu(
      width = DASHBOARD_CONFIG$sidebar_width
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "radar_style.css")
    ),
    useShinyjs(),
    introjsUI()
  )
)

server <- function(input, output, session){
    observe({
    introjs(session)
  })
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)


