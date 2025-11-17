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
    
    # Diagrama Sankey con controles - siguiendo patrón de radar.R
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
                color = "danger",
                size = "xs"
              )
            ),
            
            # Botón de descarga (abajo izquierda, al lado de config)
            div(
              style = "position: absolute; left: 4em; bottom = 0.5em;",
              dropdown(
                downloadButton(outputId = "download_sankey", label = "Descargar diagrama"),
                size = "xs",
                icon = icon("download", class = "opt"),
                up = TRUE
              )
            )
          )
        )
      )
    ),
    
    # Información y tablas en dos columnas
    fluidRow(
      column(
        width = 6,
        box(
          title = "Información del Diagrama",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          verbatimTextOutput("sankey_info")
        )
      ),
      column(
        width = 6,
        box(
          title = "Enlaces Detallados",
          status = "warning",
          solidHeader = TRUE,
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          div(
            style = "overflow-x:auto;",
            withSpinner(
              DT::dataTableOutput("sankey_enlaces_table"),
              type = 4,
              color = "#d33724",
              size = 0.5
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
      column(6,
        checkboxInput("mostrar_parametro", 
                     "Incluir parámetro", 
                     value = FALSE),
        helpText("Colorea los enlaces según el parámetro de reparto")
      ),
      column(6,
        # Placeholder for additional filters
        # You can add more configuration options here later
      )
    )
  )
}
