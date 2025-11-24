# ============================================================================
# SANKEY PANEL UI
# ============================================================================

#' Create Sankey panel content
#' Interactive Sankey diagram with modern styling following radar.R pattern
#' 
#' @return A tagList with the panel UI structure
create_sankey_content <- function() {
  shiny::tagList(
    # Info Boxes
    fluidRow(
      infoBoxOutput("sankey_info_nodos", width = 4),
      infoBoxOutput("sankey_info_enlaces", width = 4),
      infoBoxOutput("sankey_info_importe", width = 4)
    ),
    
    # Diagrama Sankey con controles
    fluidRow(
      column(
        width = 12,
        div(
          style = "position: relative",
          tabBox(
            id = "sankey_box",
            width = NULL,
            height = 700,
            tabPanel(
              title = "Diagrama Sankey",
              
              # Controles en esquina inferior izquierda
              div(
                style = "position: absolute; left: 0.5em; bottom: 0.5em;",
                dropdown(
                  checkboxInput("sankey_mostrar_parametro", 
                               "Incluir parámetro", 
                               value = FALSE),
                  size = "xs",
                  icon = icon("gear", class = "opt"),
                  style = "fill",
                  up = TRUE
                )
              ),
              
              # Diagrama con spinner
              withSpinner(
                plotlyOutput("sankey_diagram", height = 600),
                type = 4,
                color = "#d33724",
                size = 0.7
              )
            ),
            
            # Botón de expandir (abajo derecha)
            div(
              style = "position: absolute; right: 0.5em; bottom: 0.5em;",
              actionBttn(
                inputId = "expand_sankey",
                icon = icon("search-plus", class = "opt"),
                style = "fill",
                size = "xs"
              )
            ),
            
            # Botón de descarga (abajo izquierda, al lado de config)
            div(
              style = "position: absolute; left: 4em; bottom: 0.5em;",
              dropdown(
                tags$div(
                  style = "padding: 2px;",
                  downloadButton(
                    outputId = "download_sankey",
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
    
    # Tabla de enlaces detallados
    fluidRow(
      column(
        width = 12,
        div(
          style = "position: relative",
          tabBox(
            id = "sankey_enlaces_box",
            width = NULL,
            height = 500,
            tabPanel(
              title = "Enlaces Detallados",
              withSpinner(
                DT::dataTableOutput("sankey_enlaces_table"),
                type = 4,
                color = "#d33724",
                size = 0.5
              )
            ),
            
            # Botón de expandir tabla
            div(
              style = "position: absolute; right: 0.5em; bottom: 0.5em;",
              actionBttn(
                inputId = "expand_enlaces_table",
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


# ============================================================================
# SANKEY FILTERS
# ============================================================================

#' Create Sankey-specific filters UI
#' This will be shown in the modal when the user clicks the filter button
create_sankey_filters <- function() {
  shiny::tagList(
    h4("Configuración del Diagrama Sankey"),
    
    fluidRow(
      column(12,
        h5("Opciones de Visualización"),
        helpText("Configure las opciones específicas para el diagrama de Sankey.")
      )
    ),
    
    fluidRow(
      column(6,
        checkboxInput("modal_mostrar_parametro", 
                     "Incluir parámetro en agregación", 
                     value = FALSE),
        helpText("Cuando esté activado, los enlaces se agruparán también por parámetro de reparto, mostrando más detalle.")
      ),
      column(6,
        checkboxInput("modal_excluir_estaticos",
                      "Mostrar Movimientos Estáticos",
                      value = FALSE),
        helpText("Muestra los costes que no se mueven de su ubicación inicial (costes directos).")
      )
    ),
    
    hr(),
    
    fluidRow(
      column(12,
        p(strong("Nota:"), "Los filtros generales (Centro Gestor, Nivel de Agregación, Fases, etc.) se aplican desde el panel lateral izquierdo.",
          style = "font-size: 11px; color: #666;")
      )
    )
  )
}
