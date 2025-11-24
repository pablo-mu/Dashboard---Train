# ============================================================================
# TRAZA PANEL UI
# ============================================================================

#' Create Traza panel content
#' Matriz de movimientos con controles de agregación por fase
#' 
#' @return A tagList with the panel UI structure
create_traza_content <- function() {
  shiny::tagList(
    # Info Boxes
    fluidRow(
      infoBoxOutput("traza_info_registros", width = 4),
      infoBoxOutput("traza_info_fases", width = 4),
      infoBoxOutput("traza_info_importe", width = 4)
    ),
    
    # Tabla de Traza (Matriz de movimientos)
    fluidRow(
      column(
        width = 12,
        div(
          style = "position: relative",
          tabBox(
            id = "traza_table_box",
            width = NULL,
            height = 700,
            tabPanel(
              title = "Matriz de Movimientos",
              withSpinner(
                DT::dataTableOutput("traza_table"),
                type = 4,
                color = "#d33724",
                size = 0.7
              )
            ),
            
            # Botón de expandir tabla
            div(
              style = "position: absolute; right: 0.5em; bottom: 0.5em;",
              actionBttn(
                inputId = "expand_traza_table",
                icon = icon("search-plus", class = "opt"),
                style = "fill",
                size = "xs"
              )
            ),
            
            # Botón de descarga
            div(
              style = "position: absolute; left: 0.5em; bottom: 0.5em;",
              dropdown(
                tags$div(
                  style = "padding: 2px;",
                  downloadButton(
                    outputId = "download_traza_csv",
                    label = "Descargar",
                    style = "background-color: #c8102e; color: white; border: none; font-size: 11px; display: flex; align-items: center; gap: 5px; padding: 6px 12px;"
                  )
                ),
                size = "xs",
                icon = icon("download", class = "opt"),
                style = "fill",
                up = TRUE
              )
            )
          )
        )
      )
    )
  )
}


# ============================================================================
# TRAZA FILTERS
# ============================================================================

#' Create Traza-specific filters UI
#' This will be shown in the modal when the user clicks the filter button
create_traza_filters <- function() {
  shiny::tagList(
    h4("Configuración del Análisis de Costes"),
    
    fluidRow(
      column(12,
        helpText("Esta tabla muestra, para cada CAC de destino final, cuánto importe le llega en cada fase del proceso de reparto.")
      )
    ),
    
    fluidRow(
      column(6,
        selectInput(
          "modal_traza_nivel_agregacion",
          "Nivel de Agregación:",
          choices = c(
            "CAC (3 dígitos)" = "cac",
            "CAC1 (1 dígito)" = "cac1",
            "CAC2 (2 dígitos)" = "cac2"
          ),
          selected = "cac"
        ),
        helpText("Aplica a todas las fases (Fase 0, 1, 2 y 3)")
      ),
      column(6,
        checkboxInput(
          "modal_traza_mostrar_porcentajes",
          "Mostrar columnas de porcentaje (%)",
          value = FALSE
        ),
        div(
          style = "padding: 20px; background-color: #f8f9fa; border-radius: 4px; margin-top: 10px;",
          strong("Columnas de la tabla:"),
          tags$ul(
            tags$li(strong("CD:"), "Coste Directo (Fase 0 → Fase 3 directamente)"),
            tags$li(strong("F1:"), "Importe que llega en Fase 1"),
            tags$li(strong("F2:"), "Importe que llega en Fase 2"),
            tags$li(strong("F3:"), "Importe que llega en Fase 3")
          ),
          style = "font-size: 11px;"
        )
      )
    ),
    
    hr(),
    
    fluidRow(
      column(12,
        p(strong("Nota:"), "El análisis siempre se calcula a nivel CAC (3 dígitos) y luego se agrega según el nivel seleccionado.",
          style = "font-size: 11px; color: #666;"),
        p(strong("Filtros generales:"), "Los filtros de Centro Gestor, Mes, Año, Origen y Destino se aplican desde el panel lateral izquierdo.",
          style = "font-size: 11px; color: #666;")
      )
    )
  )
}
