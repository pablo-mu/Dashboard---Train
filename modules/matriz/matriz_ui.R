# ============================================================================
# MATRIZ PANEL UI
# ============================================================================

#' Create MATRIZ panel content
#' Interactive MATRIZ diagram with modern styling following radar.R pattern
#' 
#' @return A tagList with the panel UI structure
#' 
create_matriz_content <- function() {
  shiny::tagList(
    fluidRow(
      column(
        width = 12,
        div(
          style = "position: relative",
          tabBox(
            id = "matriz_box",
            width = NULL,
            height = 700,
            tabPanel(
              title = "Matriz de Reparto Costes",
              withSpinner(
                plotlyOutput("matriz_diagram", height = 600),
                type = 4,
                color = "#d33724",
                size = 0.7
              )
            ),
            # Botón de expandir (abajo derecha)
            div(
              style = "position: absolute; right: 0.5em; bottom: 0.5em;",
              actionBttn(
                inputId = "expand_matriz",
                icon = icon("search-plus", class = "opt"),
                style = "fill",
                size = "xs"
              )
            ),
            div(
              style = "position: absolute; left: 0.5em; bottom: 0.5em;",
              dropdown(
                tags$div(
                  style = "padding: 2px;",
                  downloadButton(
                    outputId = "download_matriz",
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
    ),
    fluidRow(
      column(
        width = 6,
        div(
          style = "position: relative",
          tabBox(
            id = "matriz_tabla_larga",
            width = NULL,
            height = 500,
            tabPanel(
              title = "Datos de la matriz (formato largo)",
              DT::dataTableOutput("matriz_table_larga")
            ),
            # Botón de expandir
            div(
              style = "position: absolute; right: 0.5em; bottom: 0.5em;",
              actionBttn(
                inputId = "expand_matriz_larga",
                icon = icon("search-plus", class = "opt"),
                style = "fill",
                size = "xs"
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
            id = "matriz_tabla_ancha",
            width = NULL,
            height = 500,
            tabPanel(
              title = "Datos de la matriz (formato ancho)",
              DT::dataTableOutput("matriz_table_ancha")
            ),
            # Botón de expandir
            div(
              style = "position: absolute; right: 0.5em; bottom: 0.5em;",
              actionBttn(
                inputId = "expand_matriz_ancha",
                icon = icon("search-plus", class = "opt"),
                style = "fill",
                size = "xs"
              )
            )
          )
        )
      )
    )
  )
}

create_matriz_filters <- function(){
  shiny::tagList(
    h4("Configuración de la Matriz de Costes"),
    
    fluidRow(
      column(12,
        h5("Opciones de Visualización"),
        helpText("Configure las opciones específicas para la matriz de costes.")
      )
    ),
    
    fluidRow(
      column(12,
        checkboxInput("modal_excluir_estaticos",
                      "Mostrar Movimientos Estáticos",
                      value = FALSE),
        helpText("Muestra los costes que no se mueven de su ubicación inicial (costes directos).")
      )
    ),
    
    hr(),
    
    fluidRow(
      column(12,
        p(strong("Nota:"), "Los filtros generales (Centro Gestor, Nivel de Agregación, Origen, Destino, etc.) se aplican desde el panel lateral izquierdo.",
          style = "font-size: 11px; color: #666;")
      )
    )
  )
}