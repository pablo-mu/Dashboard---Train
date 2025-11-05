# =============================================================================
# PATIENTS PANEL MODULE
# =============================================================================

create_patients_content <- function() {
  tagList(
    fluidRow(
      introBox(
        box(title = "Demografía de Pacientes", status = "info", solidHeader = TRUE, width = 6,
            plotOutput("demographics_plot"),
            br(),
            "Distribución por edad y género de pacientes"
        ),
        box(title = "Procedencia de Pacientes", status = "info", solidHeader = TRUE, width = 6,
            plotOutput("origin_plot"),
            br(),
            "Análisis de origen de admisiones (ER, Ward, etc.)"
        ),
        data.step = 3, data.intro = "Panel de análisis de pacientes."
      )
    ),
    fluidRow(
      introBox(
        box(title = "Especialidades Médicas", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("specialties_table"),
            br(),
            "Distribución de pacientes por especialidad médica"
        ),
        data.step = 4, data.intro = "Datos de especialidades médicas."
      )
    )
  )
}

create_patients_filters <- function() {
  list(
    h4("Filtros para Pacientes"),
    checkboxGroupInput("age_groups", "Grupos de edad:", 
                      choices = c("0-18", "19-65", "65+")),
    selectInput("gender_filter", "Género:", 
               choices = c("Todos", "Masculino", "Femenino"))
  )
}