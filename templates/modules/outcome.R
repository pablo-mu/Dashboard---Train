# =============================================================================
# OUTCOME PANEL MODULE
# =============================================================================

create_outcome_content <- function() {
  tagList(
    fluidRow(
      introBox(
        box(title = "Estancia Hospitalaria", status = "success", solidHeader = TRUE, width = 6,
            plotOutput("los_plot"),
            br(),
            "Análisis de duración de estancia hospitalaria"
        ),
        box(title = "Resultados Clínicos", status = "success", solidHeader = TRUE, width = 6,
            plotOutput("outcomes_plot"),
            br(),
            "Distribución de resultados: alta, traslado, fallecimiento"
        ),
        data.step = 3, data.intro = "Panel de análisis de resultados."
      )
    ),
    fluidRow(
      introBox(
        box(title = "Mortalidad y Factores de Riesgo", status = "danger", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("mortality_table"),
            br(),
            "Análisis de factores de riesgo y mortalidad"
        ),
        data.step = 4, data.intro = "Datos de mortalidad y factores de riesgo."
      )
    )
  )
}

create_outcome_filters <- function() {
  list(
    h4("Filtros para Outcome"),
    selectInput("outcome_type", "Tipo de resultado:", 
               choices = c("Todos", "Alta médica", "Traslado", "Fallecimiento")),
    sliderInput("los_filter", "Estancia hospitalaria (días):", min = 1, max = 90, value = c(1, 30))
  )
}