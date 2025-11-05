# =============================================================================
# ANTIMICROBIALS PANEL MODULE
# =============================================================================

create_antimicrobials_content <- function() {
  tagList(
    # Layout con fluid_design - gráficos con filtros integrados
    fluid_design("antimicrobials_panel", "box1_ab", "box2_ab", "box3_ab", "box4_ab"),
    
    # Tabla de resistencia
    fluidRow(
      introBox(
        box(title = "Resistencia Antimicrobiana", status = "warning", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("resistance_table"),
            br(),
            "Tabla de patrones de resistencia detectados"
        ),
        data.step = 4, data.intro = "Datos de resistencia antimicrobiana."
      )
    )
  )
}

# Crear los outputs individuales para el fluid_design
create_antimicrobials_boxes <- function(output) {
  
  # Box 1: Gráfico principal con filtros
  output$box1_ab <- renderUI({
    create_box_with_filters(
      title = "Antimicrobianos por Tipo",
      status = "primary",
      plot_output_id = "antimicrobials_plot",
      filters_list = tagList(
        create_compact_filter("ab_type_filter", "Tipo", 
                            choices = c("Todos", "Penicilinas", "Cefalosporinas", "Quinolonas"),
                            selected = "Todos", type = "select"),
        create_compact_filter("ab_route_filter", "Ruta", 
                            choices = c("Todas", "IV", "Oral", "IM"),
                            selected = "Todas", type = "select")
      ),
      description = "Distribución de antimicrobianos por tipo y frecuencia de uso"
    )
  })
  
  # Box 2: Duración del tratamiento con filtros
  output$box2_ab <- renderUI({
    create_box_with_filters(
      title = "Duración del Tratamiento",
      status = "info", 
      plot_output_id = "duration_plot",
      filters_list = tagList(
        create_compact_filter("duration_range", "Días", 
                            choices = c(1, 30), selected = c(1, 14), type = "slider"),
        create_compact_filter("specialty_filter", "Especialidad",
                            choices = c("Todas", "Medicina", "Cirugía", "UCI"),
                            selected = "Todas", type = "select")
      ),
      description = "Análisis de duración de tratamientos antimicrobianos por especialidad"
    )
  })
  
  # Box 3: Tendencias temporales con filtros
  output$box3_ab <- renderUI({
    create_box_with_filters(
      title = "Tendencias Temporales",
      status = "success",
      plot_output_id = "temporal_trends_plot", 
      filters_list = tagList(
        create_compact_filter("time_period", "Período",
                            choices = c(Sys.Date()-90, Sys.Date()), type = "daterange"),
        create_compact_filter("aggregation", "Agrupar por",
                            choices = c("Día", "Semana", "Mes"),
                            selected = "Semana", type = "select")
      ),
      description = "Evolución temporal del uso de antimicrobianos"
    )
  })
  
  # Box 4: Resistencia por patógeno con filtros  
  output$box4_ab <- renderUI({
    create_box_with_filters(
      title = "Resistencia por Patógeno",
      status = "warning",
      plot_output_id = "resistance_plot",
      filters_list = tagList(
        create_compact_filter("pathogen_filter", "Patógeno",
                            choices = c("Todos", "E. coli", "S. aureus", "K. pneumoniae"),
                            selected = "Todos", type = "select"),
        create_compact_filter("resistance_threshold", "% Resistencia",
                            choices = c(0, 100), selected = c(20, 100), type = "slider")
      ),
      description = "Patrones de resistencia antimicrobiana por microorganismo"
    )
  })
}

create_antimicrobials_filters <- function() {
  list(
    h4("Filtros para Antimicrobianos"),
    selectInput("ab_filter", "Tipo de Antimicrobiano:", 
               choices = c("Todos", "Penicilinas", "Cefalosporinas", "Quinolonas")),
    sliderInput("ab_duration", "Duración mínima (días):", min = 1, max = 30, value = 3)
  )
}