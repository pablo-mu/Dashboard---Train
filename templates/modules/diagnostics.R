# =============================================================================
# DIAGNOSTICS PANEL MODULE
# =============================================================================

create_diagnostics_content <- function() {
  tagList(
    fluidRow(
      introBox(
        box(title = "Cultivos de Sangre", status = "warning", solidHeader = TRUE, width = 6,
            plotOutput("blood_cultures_plot"),
            br(),
            "Resultados y tendencias de hemocultivos"
        ),
        box(title = "Cultivos de Orina", status = "warning", solidHeader = TRUE, width = 6,
            plotOutput("urine_cultures_plot"),
            br(),
            "Análisis de urocultivos y patógenos frecuentes"
        ),
        data.step = 3, data.intro = "Panel de análisis diagnóstico."
      )
    ),
    fluidRow(
      introBox(
        box(title = "Microorganismos Identificados", status = "danger", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("microorganisms_table"),
            br(),
            "Lista de microorganismos identificados en cultivos"
        ),
        data.step = 4, data.intro = "Datos microbiológicos detallados."
      )
    )
  )
}

create_diagnostics_filters <- function() {
  list(
    h4("Filtros para Diagnósticos"),
    selectInput("test_type", "Tipo de prueba:", 
               choices = c("Todas", "Sangre", "Orina", "Esputo")),
    dateRangeInput("test_date", "Fecha de pruebas:")
  )
}