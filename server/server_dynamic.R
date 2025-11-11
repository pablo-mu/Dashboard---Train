create_dynamic_server <- function(input, output,
  session, panel_definitions, default_panel
){
  library(data.table)
  library(dplyr)
  library(shiny)
  library(shinyBS) # Para bsButton
  library(shinydashboard) # Para box, fluidRow, etc.
  library(shinyjs) # Para funciones adicionales de UI

  source("server/filters_server.R")
  source("modules/lib_reparto.R")

  # Aseguramos un panel por defecto válido
  if (is.null(default_panel) || !default_panel %in% names(panel_definitions)) {
    default_panel <- names(panel_definitions)[1]
  }
  active_panel <- shiny::reactiveVal(default_panel)

  # Render del botón (independiente de los datos)
  output$dynamic_filter_btn <- shiny::renderUI({
    panel <- active_panel()
    panel_config <- panel_definitions[[panel]]

    if (!is.null(panel_config)) {
      bsButton("show_filters",
              panel_config$button_label,
              icon = icon(panel_config$icon),
              style = "danger")
    } else {
      bsButton("show_filters", "FILTROS", icon = icon("filter"), style = "danger")
    }
  })

  
}