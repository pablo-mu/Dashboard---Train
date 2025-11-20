# ============================================================================
# DIAGNOSTICO PANEL UI
# ============================================================================

#' Create DIAGNOSTICO panel content
#' Data diagnostics and analysis view with modern styling
#' 
#' @return A tagList with the panel UI structure
#' 
create_diagnostico_content <- function() {
  shiny::tagList(
    # Vista previa de datos
    fluidRow(
      column(
        width = 12,
        div(
          style = "position: relative",
          tabBox(
            id = "diagnostico_preview_box",
            width = NULL,
            height = 600,
            tabPanel(
              title = "Vista Previa de Datos",
              withSpinner(
                DT::dataTableOutput("diagnostico_data_preview"),
                type = 4,
                color = "#d33724",
                size = 0.7
              )
            ),
            # Botón de expandir (abajo derecha)
            div(
              style = "position: absolute; right: 0.5em; bottom: 0.5em;",
              actionBttn(
                inputId = "expand_diagnostico_preview",
                icon = icon("search-plus", class = "opt"),
                style = "fill",
                size = "xs"
              )
            )
          )
        )
      )
    ),
    
    # Información y análisis en dos columnas
    fluidRow(
      column(
        width = 6,
        div(
          style = "position: relative",
          tabBox(
            id = "diagnostico_columns_box",
            width = NULL,
            height = 500,
            tabPanel(
              title = "Información de Columnas",
              div(
                style = "padding: 15px; overflow-y: auto; height: 400px;",
                verbatimTextOutput("diagnostico_columns_info")
              )
            )
          )
        )
      ),
      column(
        width = 6,
        div(
          style = "position: relative",
          tabBox(
            id = "diagnostico_analysis_box",
            width = NULL,
            height = 500,
            tabPanel(
              title = "Análisis de Datos",
              div(
                style = "padding: 15px; overflow-y: auto; height: 400px;",
                verbatimTextOutput("diagnostico_data_issues")
              )
            )
          )
        )
      )
    )
  )
}

#' Create Diagnostico-specific filters UI
#' This will be shown in the modal when the user clicks the filter button
create_diagnostico_filters <- function() {
  shiny::tagList(
    h4("Información del Diagnóstico"),
    
    fluidRow(
      column(12,
        div(
          style = "background-color: #e8f4f8; padding: 15px; border-left: 4px solid #3498db; border-radius: 4px; margin-bottom: 15px;",
          p(icon("info-circle"), strong(" Diagnóstico Automático"), 
            style = "margin: 0 0 8px 0; font-size: 14px; color: #2c3e50;"),
          p("El diagnóstico de datos se calcula y actualiza automáticamente cuando se cargan los datos. No es necesario presionar ningún botón.",
            style = "margin: 0; font-size: 12px; color: #555; line-height: 1.5;")
        )
      )
    ),
    
    fluidRow(
      column(12,
        h5("Información disponible:"),
        tags$ul(
          style = "font-size: 12px; color: #666; line-height: 1.8;",
          tags$li("Vista previa de datos (primeras 100 filas)"),
          tags$li("Información detallada de columnas y dimensiones"),
          tags$li("Análisis de importes y valores únicos"),
          tags$li("Cobertura temporal (meses y años)"),
          tags$li("Calidad de datos y estadísticas generales")
        )
      )
    ),
    
    hr(),
    
    fluidRow(
      column(12,
        p(strong("Tip:"), " Usa el botón de expansión ", icon("search-plus"), 
          " en la vista previa para ver más filas en pantalla completa.",
          style = "font-size: 11px; color: #666; background-color: #fffbea; padding: 10px; border-radius: 4px; border-left: 3px solid #f39c12;")
      )
    )
  )
}
